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
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.colors :as dwc]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]))

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

(defn describe-canvas-state
  "A snapshot of current selection + page objects, serializable to plain
  JS data — the main process includes this in the prompt context before
  calling the provider, so the model knows what's already on the canvas
  and what's selected without needing a separate round-trip tool call for
  every turn."
  []
  (let [state     @st/state
        objects   (dsh/lookup-page-objects state)
        selected  (dsh/lookup-selected state)]
    (clj->js
     {:selectedIds (mapv str selected)
      :shapes (mapv (fn [[id shape]]
                      {:id (str id)
                       :type (name (:type shape))
                       :name (:name shape)
                       :x (:x shape)
                       :y (:y shape)
                       :width (:width shape)
                       :height (:height shape)})
                    objects)})))

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

(def ^:private tool-impls
  {"create_rectangle" tool-create-rectangle
   "change_fill_color" tool-change-fill-color
   "delete_shape" tool-delete-shape})

(defn dispatch-tool-call
  "Run one tool call (as pushed by the Electron main process) against the
  real canvas, then report the result back over the per-call reply
  channel the main process is waiting on."
  [tool-call]
  (let [id        (unchecked-get tool-call "id")
        tool-name (unchecked-get tool-call "name")
        args      (js->clj (unchecked-get tool-call "args") :keywordize-keys true)
        impl      (get tool-impls tool-name)
        ai-api    ^js (.-ai (electron-api))]
    (if (nil? impl)
      (.reportToolCallResult ai-api id #js {:error (str "Unknown AI tool: " tool-name)})
      (try
        (let [output (impl args)]
          (.reportToolCallResult ai-api id #js {:output output}))
        (catch :default e
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
        (st/emit! (dwu/start-undo-transaction tx-id)))

      ("done" "error")
      (when-let [tx-id @current-turn-tx]
        (reset! current-turn-tx nil)
        (st/emit! (dwu/commit-undo-transaction tx-id)))

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
