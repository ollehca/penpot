;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ADDED BY KIZUKU (2026-10-21): bridges the Electron-side AI provider
;; turn loop (src/services/ai-provider-client.js) to PenPot's real
;; mutation events. Receives tool calls pushed over window.electronAPI.ai
;; (see src/preload.js), dispatches each one via st/emit!, and replies
;; with the tool's result over the same per-call channel the Electron
;; side is waiting on — the actual canvas mutation always happens through
;; the same events the rest of the app uses (shapes.cljs/colors.cljs), so
;; undo/redo work for AI-driven edits exactly as they do for manual edits.
;;
;; Deliberately NOT routed through app.plugins.* — that runtime is a
;; locked-down SES sandbox meant for untrusted third-party code and would
;; block the free network access the AI calls need anyway (see the
;; project plan for the full reasoning).

(ns app.main.data.workspace.ai-tools
  (:require
   [app.common.math :as mth]
   [app.common.types.color :as ctc]
   [app.common.types.fills :as types.fills]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.ai-chat :as ai-chat]
   [app.main.data.workspace.colors :as dwc]
   [app.main.data.workspace.groups :as dwg]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]
   [clojure.string :as str]))

(defn- electron-api
  []
  (unchecked-get js/window "electronAPI"))

(defn- parse-uuid-safe
  "Parse a string into a uuid, returning nil (not throwing) on anything
  that isn't a valid uuid — tool-call args come from an LLM, so malformed
  ids are an expected, recoverable case, not a programmer error."
  [s]
  (when (string? s)
    (try (uuid/parse s) (catch :default _ nil))))

(defn- resolve-target-ids
  "Resolve the shape ids a tool call should act on: explicit ids from the
  tool args when given, otherwise the current selection."
  [{:keys [shape_ids]}]
  (if (seq shape_ids)
    (into #{} (keep parse-uuid-safe) shape_ids)
    (into #{} (dsh/lookup-selected @st/state))))

(defn- first-fill-color
  "The hex color of a shape's first fill slot, or nil if it has none —
  :fills is seqable whether it's a plain vector or the packed binary
  representation (app.common.types.fills.impl's Fills type implements
  ISeqable), so plain first/map works on it regardless."
  [fills]
  (some-> fills first types.fills/fill->color :color))

(defn- text-fill-color
  "A text shape's own :fills (if any) is a leftover/default, not what's
  actually rendered — real text color lives per-run, inside :content's
  paragraph tree (see app.common.types.text/text-fills). Read the first
  run's fill instead, falling back to the shape-level one."
  [shape]
  (or (->> (txt/node-seq txt/is-text-node? (:content shape))
           first :fills first-fill-color)
      (first-fill-color (:fills shape))))

(defn- describe-shape
  [[id shape]]
  (let [parent-id (:parent-id shape)]
    (cond-> {:id (str id)
             :type (name (:type shape))
             :name (:name shape)
             :x (:x shape)
             :y (:y shape)
             :width (:width shape)
             :height (:height shape)
             :parentId (when (and parent-id (not= parent-id uuid/zero))
                         (str parent-id))}
      (= (:type shape) :text)
      (assoc :fillColor (text-fill-color shape))

      (not= (:type shape) :text)
      (assoc :fillColor (first-fill-color (:fills shape))))))

(defn describe-canvas-state
  "A snapshot of current selection + page objects, serializable to plain
  JS data — the main process includes this in the prompt context before
  calling the provider, so the model knows what's already on the canvas
  and what's selected without needing a separate round-trip tool call for
  every turn. Includes each shape's fill color so the model can reason
  about contrast/legibility (e.g. \"make the text accessible\") against
  what's actually already on the canvas, instead of guessing blind, and
  each shape's parentId so it can see existing group/frame structure
  before deciding what (if anything) still needs grouping. uuid/zero
  (the page's own implicit root) is excluded — it isn't a real shape
  the model should ever reference."
  []
  (let [state    @st/state
        objects  (dsh/lookup-page-objects state)
        selected (dsh/lookup-selected state)]
    (clj->js
     {:selectedIds (mapv str selected)
      :shapes (->> objects
                   (remove (fn [[id _]] (= id uuid/zero)))
                   (mapv describe-shape))})))

(defn build-system-prompt
  "The full system message sent with every turn — each provider adapter
  (anthropic/openai/google.js) forwards this verbatim as the entire
  system prompt (no static framing of its own on the JS side), so the
  schema explanation has to live here, right next to the schema itself,
  or the model is just handed a bare JSON blob with no field docs."
  []
  (str "You are editing a design canvas. Below is the current canvas state as JSON: "
       "selectedIds (currently selected shape ids) and shapes (every shape on the "
       "current page). Each shape has an id, type, name, x/y/width/height, fillColor, "
       "and parentId (the id of the group or frame it's directly inside, or null if "
       "it sits at the page/frame's top level). fillColor is the shape's own fill/"
       "background color as a hex string, or null if it has none. For a text shape, "
       "fillColor is the color the text itself renders in, not a background.\n\n"
       "When asked to make text legible, readable, or accessible, don't guess a color "
       "blind — find which shape or frame the text's x/y falls inside (its bounding "
       "box overlapping the text's position), read that shape's fillColor from this "
       "snapshot, then call check_contrast with your candidate text color as foreground "
       "and that fillColor as background before applying it. Pick a color that passes "
       "passesAANormalText (or at minimum passesAALargeText for large headings), "
       "adjusting and re-checking if it doesn't. Text created with no fillColor defaults "
       "to a light color meant for this app's dark canvas background — override it "
       "whenever the text sits over something lighter.\n\n"
       "Keep the layer tree organized as you build, using group_shapes and parentId: "
       "after creating a set of shapes that form one logical unit (e.g. a button's "
       "background plus its label, or all the shapes making up one card or section of "
       "a wireframe), group them together and give the group a clear name, rather than "
       "leaving everything flat at the top level — this is what lets a human find and "
       "edit things afterward in a larger file. Before grouping, check parentId: don't "
       "re-group shapes that already share a sensible parent, and don't break apart an "
       "existing group unless asked to.\n\n"
       "move_shape and resize_shape operate on EXISTING shapes already on the canvas — use "
       "them for requests like \"move this left\", \"make these the same width\", or "
       "\"align these\" (read each target's current x/y from this snapshot and compute "
       "matching positions). move_shape is relative (dx/dy from current position), not "
       "absolute — for an exact placement, compute the offset from the shape's current x/y "
       "first.\n\n"
       "Canvas state:\n"
       (.stringify js/JSON (describe-canvas-state))))

;; --- Tool implementations ---------------------------------------------
;; Each returns a plain JS-serializable value (or throws, which the
;; dispatcher below turns into an { error } reply) describing what
;; happened — kept small and stable per Anthropic's own tool-design
;; guidance, since this becomes the tool_result content the model reads.

(defn- tool-create-rectangle
  [{:keys [x y width height fill_color]}]
  ;; create-and-add-shape's 2nd/3rd args are canvas coordinates used ONLY
  ;; to find which frame to nest the new shape inside (ctst/top-nested-frame)
  ;; — distinct from attrs' own :x/:y, which is where the shape is actually
  ;; placed. The tool schema marks x/y required, so the LLM should always
  ;; supply numbers, but a malformed/hallucinated call is still possible —
  ;; fall back to 0 rather than pass nil into frame-lookup code that
  ;; expects numeric coordinates.
  (let [frame-x (if (number? x) x 0)
        frame-y (if (number? y) y 0)
        attrs   (cond-> {:width (or width 100) :height (or height 100)}
                  (number? x) (assoc :x x)
                  (number? y) (assoc :y y)
                  (string? fill_color) (assoc :fills [{:fill-color fill_color :fill-opacity 1}]))]
    (st/emit! (dwsh/create-and-add-shape :rect frame-x frame-y attrs))
    #js {:created true :type "rect"}))

(defn- tool-create-ellipse
  [{:keys [x y width height fill_color]}]
  (let [frame-x (if (number? x) x 0)
        frame-y (if (number? y) y 0)
        attrs   (cond-> {:width (or width 100) :height (or height 100)}
                  (number? x) (assoc :x x)
                  (number? y) (assoc :y y)
                  (string? fill_color) (assoc :fills [{:fill-color fill_color :fill-opacity 1}]))]
    (st/emit! (dwsh/create-and-add-shape :circle frame-x frame-y attrs))
    #js {:created true :type "circle"}))

(defn- text->content
  "Convert plain text into the paragraph-tree shape text shapes need,
  one paragraph per newline-separated line — mirrors
  app.main.data.workspace.clipboard's private as-content helper (not
  reused directly, to keep this file independent of clipboard internals
  for a conversion this small). fill-color, when given, overrides the
  default (light, dark-canvas-oriented) per-run fill — color lives on
  each text run, not just at the shape level, so a shape-level :fills
  alone would be silently ignored for actual rendered text color."
  [text & [fill-color]]
  (let [run-attrs  (cond-> (txt/get-default-text-attrs)
                     (string? fill-color)
                     (assoc :fills [{:fill-color fill-color :fill-opacity 1}]))
        paragraphs (->> (str/split-lines text)
                        (mapv #(hash-map :type "paragraph"
                                        :children [(merge run-attrs {:text %})])))]
    {:type "root"
     :children [{:type "paragraph-set" :children paragraphs}]}))

(defn- tool-create-text
  [{:keys [x y text fill_color]}]
  (when (empty? text)
    (throw (js/Error. "No text content given.")))
  (let [frame-x (if (number? x) x 0)
        frame-y (if (number? y) y 0)
        width   (max 8 (min (* 7 (count text)) 700))
        attrs   {:x frame-x
                 :y frame-y
                 :width width
                 :height 16
                 :grow-type (if (> (count text) 100) :auto-height :auto-width)
                 :content (text->content text fill_color)}]
    (st/emit! (dwsh/create-and-add-shape :text frame-x frame-y attrs))
    #js {:created true :type "text"}))

(defn- tool-create-frame
  [{:keys [x y width height name]}]
  (let [frame-x (if (number? x) x 0)
        frame-y (if (number? y) y 0)
        attrs   (cond-> {:width (or width 400) :height (or height 300)}
                  (number? x) (assoc :x x)
                  (number? y) (assoc :y y)
                  (string? name) (assoc :name name))]
    (st/emit! (dwsh/create-and-add-shape :frame frame-x frame-y attrs))
    #js {:created true :type "frame"}))

(defn- tool-change-fill-color
  [{:keys [fill_color] :as args}]
  (let [ids (resolve-target-ids args)]
    (when (empty? ids)
      (throw (js/Error. "No shapes selected and no shape_ids given.")))
    (st/emit! (dwc/change-fill ids {:color fill_color} 0))
    #js {:updated (count ids)}))

(defn- tool-delete-shape
  [args]
  (let [ids (resolve-target-ids args)]
    (when (empty? ids)
      (throw (js/Error. "No shapes selected and no shape_ids given.")))
    (st/emit! (dwsh/delete-shapes ids))
    #js {:deleted (count ids)}))

(defn- tool-group-shapes
  [{:keys [name] :as args}]
  (let [ids (resolve-target-ids args)]
    (when (< (count ids) 2)
      (throw (js/Error. "Need at least 2 shapes (via shape_ids, or a current selection) to group.")))
    (let [group-id (uuid/next)]
      (st/emit! (dwg/group-shapes group-id ids))
      (when (string? name)
        (st/emit! (dwsh/update-shapes [group-id] #(assoc % :name name))))
      #js {:groupId (str group-id) :grouped (count ids)})))

(defn- tool-move-shape
  "Relative move only (dx/dy applied to each shape's own current
  position), not an absolute target — moving several shapes at once to
  the same {:x :y} would stack them on top of each other instead of
  shifting them together, which is never what \"move these right 20px\"
  means. update-position itself is absolute (it takes a target, not a
  delta), so each shape's current x/y has to be read first and the
  offset added before calling it."
  [{:keys [dx dy] :as args}]
  (when-not (or (number? dx) (number? dy))
    (throw (js/Error. "At least one of dx or dy is required.")))
  (let [ids     (resolve-target-ids args)
        objects (dsh/lookup-page-objects @st/state)]
    (when (empty? ids)
      (throw (js/Error. "No shapes selected and no shape_ids given.")))
    (doseq [id ids]
      (when-let [shape (get objects id)]
        (st/emit! (dwt/update-position
                    id
                    {:x (+ (:x shape) (or dx 0))
                     :y (+ (:y shape) (or dy 0))}))))
    #js {:moved (count ids)}))

(defn- tool-resize-shape
  [{:keys [width height] :as args}]
  (when-not (or (number? width) (number? height))
    (throw (js/Error. "At least one of width or height is required.")))
  (let [ids (resolve-target-ids args)]
    (when (empty? ids)
      (throw (js/Error. "No shapes selected and no shape_ids given.")))
    (when (number? width)
      (st/emit! (dwt/update-dimensions ids :width width)))
    (when (number? height)
      (st/emit! (dwt/update-dimensions ids :height height)))
    #js {:resized (count ids)}))

(defn- srgb-channel->linear
  "One sRGB 0-255 channel to its linear-light value, per the WCAG 2.x
  relative luminance definition (the standard gamma-correction piecewise
  formula, not a perceptual-brightness approximation)."
  [v]
  (let [c (/ v 255)]
    (if (<= c 0.03928)
      (/ c 12.92)
      (mth/pow (/ (+ c 0.055) 1.055) 2.4))))

(defn- relative-luminance
  [hex]
  (let [[r g b] (ctc/hex->rgb hex)]
    (+ (* 0.2126 (srgb-channel->linear r))
       (* 0.7152 (srgb-channel->linear g))
       (* 0.0722 (srgb-channel->linear b)))))

(defn- contrast-ratio
  "WCAG 2.x contrast ratio between two colors, 1 (no contrast) to 21
  (black on white) — (L1 + 0.05) / (L2 + 0.05) with L1 the lighter of
  the two relative luminances."
  [hex-a hex-b]
  (let [la (relative-luminance hex-a)
        lb (relative-luminance hex-b)
        l1 (max la lb)
        l2 (min la lb)]
    (/ (+ l1 0.05) (+ l2 0.05))))

(defn- tool-check-contrast
  [{:keys [foreground background]}]
  (when (or (empty? foreground) (empty? background))
    (throw (js/Error. "Both foreground and background colors are required.")))
  (let [ratio (contrast-ratio foreground background)]
    #js {:ratio (mth/round ratio 0.01)
         :passesAANormalText (>= ratio 4.5)
         :passesAALargeText (>= ratio 3.0)}))

(def ^:private tool-impls
  {"create_rectangle" tool-create-rectangle
   "create_ellipse" tool-create-ellipse
   "create_text" tool-create-text
   "create_frame" tool-create-frame
   "change_fill_color" tool-change-fill-color
   "delete_shape" tool-delete-shape
   "check_contrast" tool-check-contrast
   "group_shapes" tool-group-shapes
   "move_shape" tool-move-shape
   "resize_shape" tool-resize-shape})

;; Human-readable labels for display-log tool-call entries (not shown as
;; chips in the chat panel UI anymore, but kept in the data model for a
;; possible future "what did the AI do" detail view).
(def ^:private tool-labels
  {"create_rectangle" "Created rectangle"
   "create_ellipse" "Created ellipse"
   "create_text" "Created text"
   "create_frame" "Created board"
   "change_fill_color" "Changed fill colour"
   "delete_shape" "Deleted shape"
   "check_contrast" "Checked contrast"
   "group_shapes" "Grouped layers"
   "move_shape" "Moved shape"
   "resize_shape" "Resized shape"})

(defn dispatch-tool-call
  "Run one tool call (as pushed by the Electron main process) against the
  real canvas, then report the result back over the per-call reply
  channel the main process is waiting on."
  [tool-call]
  (let [id        (unchecked-get tool-call "id")
        tool-name (unchecked-get tool-call "name")
        args      (js->clj (unchecked-get tool-call "args") :keywordize-keys true)
        impl      (get tool-impls tool-name)
        ai-api    ^js (.-ai (electron-api))
        label     (get tool-labels tool-name tool-name)]
    (if (nil? impl)
      (do
        (st/emit! (ai-chat/append-display-entry
                    {:role :tool-call :tool-name tool-name :label label :failed? true}))
        (.reportToolCallResult ai-api id #js {:error (str "Unknown AI tool: " tool-name)}))
      (try
        (let [output (impl args)]
          (st/emit! (ai-chat/append-display-entry
                      {:role :tool-call :tool-name tool-name :label label}))
          (.reportToolCallResult ai-api id #js {:output output}))
        (catch :default e
          (st/emit! (ai-chat/append-display-entry
                      {:role :tool-call :tool-name tool-name :label label :failed? true}))
          (.reportToolCallResult ai-api id #js {:error (or (.-message e) (str e))}))))))

;; --- Turn-level undo wrapping -------------------------------------------
;; One AI "turn" (all tool calls resulting from a single user prompt)
;; collapses into exactly one undo entry, regardless of how many shapes
;; it touches — mirrors how a multi-shape drag or multi-select edit
;; already behaves in this app. Driven entirely by ai:turn-status pushes
;; (see ai-provider-client.js's onStatus) rather than a separate call the
;; main process would need to make — "started" opens the transaction,
;; "done"/"error" closes it, so the renderer never has to be told a turn's
;; identity explicitly.
(defonce ^:private current-turn-tx (atom nil))

(defn- handle-turn-status
  [status]
  (let [phase (unchecked-get status "phase")]
    (case phase
      "started"
      (let [tx-id (js/Symbol)]
        (reset! current-turn-tx tx-id)
        (st/emit! (dwu/start-undo-transaction tx-id))
        (st/emit! (ai-chat/set-status {:phase phase})))

      "tool-executing"
      (st/emit! (ai-chat/set-status {:phase phase}))

      "done"
      (let [final-text (unchecked-get status "finalText")]
        (when-let [tx-id @current-turn-tx]
          (reset! current-turn-tx nil)
          (st/emit! (dwu/commit-undo-transaction tx-id)))
        (when (and (string? final-text) (seq final-text))
          (st/emit! (ai-chat/append-display-entry {:role :assistant :text final-text})))
        (st/emit! (ai-chat/set-status {:phase phase})))

      "error"
      (let [message (unchecked-get status "message")]
        (when-let [tx-id @current-turn-tx]
          (reset! current-turn-tx nil)
          (st/emit! (dwu/commit-undo-transaction tx-id)))
        (st/emit! (ai-chat/set-status {:phase phase :message message})))

      nil)))

(defonce ^:private listeners-registered? (atom false))

(defn init!
  "Register the ai:tool-call and ai:turn-status listeners once per app
  lifetime. Safe to call on every workspace mount — guarded so a remount
  never double-registers."
  []
  (when (and (electron-api) (not @listeners-registered?))
    (reset! listeners-registered? true)
    (let [ai-api ^js (.-ai (electron-api))]
      (.onToolCall ai-api dispatch-tool-call)
      (.onTurnStatus ai-api handle-turn-status))))
