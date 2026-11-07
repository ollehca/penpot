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
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.common.types.color :as ctc]
   [app.common.types.fills :as types.fills]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.ai-chat :as ai-chat]
   [app.main.data.workspace.colors :as dwc]
   [app.main.data.workspace.edition :as dwe]
   [app.main.data.workspace.groups :as dwg]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.shape-layout :as dwsl]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.data.workspace.undo :as dwu]
   [app.main.fonts :as fonts]
   [app.main.store :as st]
   [app.util.i18n :refer [tr]]
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

(defn first-fill-color
  "The hex color of a shape's first fill slot, or nil if it has none —
  :fills is seqable whether it's a plain vector or the packed binary
  representation (app.common.types.fills.impl's Fills type implements
  ISeqable), so plain first/map works on it regardless. Public (not
  defn-), along with text-fill-color and describe-shape below, so this
  pure logic is independently unit-testable — see ai_tools_test.cljs."
  [fills]
  (some-> fills first types.fills/fill->color :color))

(defn text-fill-color
  "A text shape's own :fills (if any) is a leftover/default, not what's
  actually rendered — real text color lives per-run, inside :content's
  paragraph tree (see app.common.types.text/text-fills). Read the first
  run's fill instead, falling back to the shape-level one."
  [shape]
  (or (->> (txt/node-seq txt/is-text-node? (:content shape))
           first :fills first-fill-color)
      (first-fill-color (:fills shape))))

(defn- first-text-node
  "The first text run of a text shape's content, or nil."
  [content]
  (first (filter txt/is-text-node? (txt/node-seq content))))

(defn- text-of
  "Plain text of a text shape, paragraphs joined by newlines."
  [content]
  (->> (txt/node-seq content)
       (filter txt/is-text-node?)
       (map :text)
       (str/join "")))

(defn describe-shape
  "One shape as the model sees it: id, type, name, box, parent, fill, and
  for text its content and font size. Kept flat and small — this appears
  once per shape in every system prompt and list_shapes result."
  [[id shape]]
  (let [parent-id (:parent-id shape)
        text?     (= (:type shape) :text)
        run       (when text? (first-text-node (:content shape)))]
    (cond-> {:id (str id)
             :type (name (:type shape))
             :name (:name shape)
             :x (mth/round (:x shape))
             :y (mth/round (:y shape))
             :width (mth/round (:width shape))
             :height (mth/round (:height shape))
             :parentId (when (and parent-id (not= parent-id uuid/zero))
                         (str parent-id))}
      text?
      (assoc :fillColor (text-fill-color shape)
             :text (let [t (text-of (:content shape))]
                     (if (> (count t) 120) (str (subs t 0 120) "…") t))
             :fontSize (some-> run :font-size js/parseFloat)
             :fontFamily (:font-family run)
             :fontWeight (:font-weight run))

      (not text?)
      (assoc :fillColor (first-fill-color (:fills shape)))

      (seq (:shapes shape))
      (assoc :children (count (:shapes shape)))

      (:hidden shape)
      (assoc :hidden true)

      (and (number? (:rotation shape)) (not (zero? (:rotation shape))))
      (assoc :rotation (:rotation shape)))))

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
  forwards this verbatim as the entire system prompt, so the working
  rules live here next to the canvas snapshot they refer to."
  []
  (str "You are a senior product designer editing a PenPot-based design canvas through "
       "tools. Everything you make must be real, editable layers; a person will take over "
       "the file afterwards.\n\n"
       "Coordinates are canvas units (px at 100%), origin top-left, y grows downward. "
       "Colors are hex strings; opacities are 0–100 percent.\n\n"
       "How to work:\n"
       "1. For a screen or section, create_frame first (e.g. 1440×900 desktop, 390×844 "
       "phone), then build inside it: a shape whose position falls within a board becomes "
       "its child. Give boards and groups clear names.\n"
       "2. Every create_* call returns the new shape's id. Keep those ids and use them for "
       "every later edit (shape_ids); never guess ids. list_shapes shows what exists with "
       "ids, positions and text, both the user's existing work and what you built this "
       "turn.\n"
       "3. Typography is real: create_text and update_text take font_size, font_weight, "
       "font_family, text_align, line_height and letter_spacing. The default is 14px "
       "regular Source Sans Pro, which is body size — a hero headline is 64–120px at "
       "weight 700 with line_height about 1.05 and slightly negative letter_spacing; body "
       "copy is 16–18px with line_height 1.5; give paragraphs a width so they wrap. Google "
       "Fonts families work by name (e.g. Inter, Playfair Display, Space Grotesk).\n"
       "4. Legibility: before placing text on a colored surface, call check_contrast with "
       "the text color and the surface's fillColor and pick a color that passes "
       "passesAANormalText (passesAALargeText is enough for headlines). Text defaults to a "
       "light color for the dark canvas; set fill_color whenever it sits on something "
       "light.\n"
       "5. Keep the layer tree tidy: group the shapes of one logical unit (a button and its "
       "label, a card, a nav bar) with group_shapes and name the group; use set_parent if "
       "something landed outside its board; use reorder_shape when a shape must sit above "
       "or below another.\n"
       "6. move_shape takes absolute x/y or a relative dx/dy; resize_shape, rotate_shape, "
       "set_stroke, set_corner_radius, set_shadow, set_opacity and change_fill_color edit "
       "existing shapes by id. set_flex_layout turns a board into an auto layout with gap "
       "and padding when you want evenly spaced rows or columns.\n"
       "7. Only delete what you created or what the user asked you to delete, always by id. "
       "The whole request is one undo step for the user.\n"
       "8. When you finish, reply with a short plain-language summary of what you built and "
       "anything the user should check. If the tools cannot do something, say so plainly "
       "instead of improvising.\n\n"
       "Canvas state (selectedIds and every shape on the current page; parentId is the "
       "board or group a shape sits in, null at the page root):\n"
       (.stringify js/JSON (describe-canvas-state))))

;; --- Tool implementations ---------------------------------------------
;; Each returns a plain JS-serializable value (or throws, which the
;; dispatcher below turns into an { error } reply) describing what
;; happened. Every mutation goes through the same events the editor's own
;; UI uses, so the result is ordinary, undoable PenPot data.

(defn- pct->opacity
  "0–100 percent (the tool API) to PenPot's 0–1, defaulting to opaque."
  [v]
  (if (number? v) (-> v (/ 100) (max 0) (min 1)) 1))

(defn- fill-attrs
  [color opacity]
  (when (string? color)
    {:fills [{:fill-color color :fill-opacity (pct->opacity opacity)}]}))

(defn- stroke-attrs
  [color width opacity alignment style]
  (when (and (string? color) (number? width) (pos? width))
    {:strokes [{:stroke-color color
                :stroke-opacity (pct->opacity opacity)
                :stroke-width width
                :stroke-alignment (keyword (or alignment "inner"))
                :stroke-style (keyword (or style "solid"))}]}))

(defn- radius-attrs
  [r]
  (when (number? r) {:r1 r :r2 r :r3 r :r4 r}))

(defn- ids->js
  [ids]
  (clj->js (mapv str ids)))

(defn- require-ids
  "The target ids of an editing call, or a clear error for the model."
  [args]
  (let [ids (resolve-target-ids args)]
    (when (empty? ids)
      (throw (js/Error. "No shape_ids given and nothing is selected.")))
    ids))

(defn- default-name
  [type text]
  (case type
    :rect "Rectangle"
    :circle "Ellipse"
    :frame "Board"
    :text (let [t (str/trim (or text ""))]
            (if (> (count t) 30) (str (subs t 0 30) "…") t))
    "Shape"))

(defn- create-shape!
  "Create a shape through the editor's own event and report it. The id is
  minted here (setup-shape keeps an explicit :id), so the model learns it
  from the result without a round trip."
  [type {:keys [x y width height] layer-name :name :as args} extra-attrs]
  (let [id      (uuid/next)
        frame-x (if (number? x) x 0)
        frame-y (if (number? y) y 0)
        shape-name (if (string? layer-name) layer-name (default-name type (:text args)))
        attrs   (cond-> (merge {:id id :name shape-name
                                :width (or width 100) :height (or height 100)}
                               extra-attrs)
                  (number? x) (assoc :x x)
                  (number? y) (assoc :y y))]
    ;; A text arrives with its content, so it never opens the editor: the
    ;; editor would commit that content again as its own undo entry when
    ;; it closes at the end of the turn, and one ⌘Z would only pop that.
    (st/emit! (dwsh/create-and-add-shape type frame-x frame-y attrs {:no-edition? true}))
    #js {:created true
         :id (str id)
         :type (name type)
         :name shape-name
         :x frame-x
         :y frame-y
         :width (:width attrs)
         :height (:height attrs)}))

(defn- tool-create-rectangle
  [{:keys [fill_color fill_opacity corner_radius stroke_color stroke_width opacity] :as args}]
  (create-shape! :rect args
                 (merge (fill-attrs fill_color fill_opacity)
                        (radius-attrs corner_radius)
                        (stroke-attrs stroke_color stroke_width 100 "inner" "solid")
                        (when (number? opacity) {:opacity (pct->opacity opacity)}))))

(defn- tool-create-ellipse
  [{:keys [fill_color fill_opacity stroke_color stroke_width opacity] :as args}]
  (create-shape! :circle args
                 (merge (fill-attrs fill_color fill_opacity)
                        (stroke-attrs stroke_color stroke_width 100 "inner" "solid")
                        (when (number? opacity) {:opacity (pct->opacity opacity)}))))

(defn- tool-create-frame
  [{:keys [fill_color corner_radius clip_content] :as args}]
  (create-shape! :frame (merge {:width 400 :height 300} args)
                 (merge (fill-attrs fill_color 100)
                        (radius-attrs corner_radius)
                        {:show-content (not (false? clip_content))})))

;; --- text ---------------------------------------------------------------

(defn- resolve-font
  "Font attrs for a family name, weight and style. An unknown family keeps
  base-font-id (the shape's current font, or the default); the weight
  falls back to the closest one the font has."
  [family weight style base-font-id]
  (let [font   (or (when (string? family) (fonts/find-font-family family))
                   (fonts/get-font-data (or base-font-id (:font-id txt/default-text-attrs))))
        style' (if (= style "italic") "italic" "normal")
        want-w (if (number? weight) (int weight) nil)]
    (when font
      (let [variants (:variants font)
            exact    (fonts/find-variant font (cond-> {:style style'}
                                                want-w (assoc :weight (str want-w))))
            closest  (->> variants
                          (filter #(= style' (:style %)))
                          (sort-by #(js/Math.abs (- (js/parseInt (:weight %) 10) (or want-w 400))))
                          first)
            variant  (or exact closest (first variants))]
        (fonts/ensure-loaded! (:id font) (:id variant))
        {:font-id (:id font)
         :font-family (:family font)
         :font-variant-id (:id variant)
         :font-weight (:weight variant)
         :font-style (:style variant)}))))

(defn- text-style-attrs
  "Run attrs for the typography fields of create_text / update_text."
  [{:keys [font_size font_weight font_family font_style text_align line_height
           letter_spacing text_transform fill_color]}
   base-font-id]
  (cond-> {}
    (number? font_size) (assoc :font-size (str font_size))
    (or (string? font_family) (number? font_weight) (string? font_style))
    (merge (resolve-font font_family font_weight font_style base-font-id))
    (string? text_align) (assoc :text-align text_align)
    (number? line_height) (assoc :line-height (str line_height))
    (number? letter_spacing) (assoc :letter-spacing (str letter_spacing))
    (string? text_transform) (assoc :text-transform text_transform)
    (string? fill_color) (assoc :fills [{:fill-color fill_color :fill-opacity 1}])))

(defn- text->content
  "Plain text to the paragraph tree text shapes need, one paragraph per
  line, every run carrying run-attrs (the default attrs merged with the
  requested style). text-align also lives on the paragraph nodes."
  [text run-attrs]
  (let [align      (:text-align run-attrs)
        paragraphs (->> (str/split-lines text)
                        (mapv (fn [line]
                                (cond-> {:type "paragraph"
                                         :children [(merge run-attrs {:text line})]}
                                  align (assoc :text-align align)))))]
    {:type "root"
     :children [{:type "paragraph-set" :children paragraphs}]}))

(defonce ^:private turn-text-ids (atom #{}))

(defn- tool-create-text
  [{:keys [text width font_size line_height] :as args}]
  (when (empty? text)
    (throw (js/Error. "No text content given.")))
  (let [style     (text-style-attrs args nil)
        run-attrs (merge (txt/get-default-text-attrs) style)
        size      (if (number? font_size) font_size 14)
        lines     (count (str/split-lines text))
        lh        (if (number? line_height) line_height 1.2)
        est-w     (if (number? width) width (max 8 (min (* 0.6 size (count text)) 1200)))
        est-h     (max 8 (* size lh lines))
        attrs     {:width est-w
                   :height est-h
                   :grow-type (if (number? width) :auto-height :auto-width)
                   :content (text->content text run-attrs)}]
    (let [result (create-shape! :text (merge args {:width est-w :height est-h}) attrs)]
      (swap! turn-text-ids conj (uuid/parse (unchecked-get result "id")))
      ;; add-shape starts text-edit mode on every new text layer (so a
      ;; person can type into it). Here nobody is typing, and an editor
      ;; left open swallows the next undo and the menu commands.
      (st/emit! (dwe/clear-edition-mode))
      result)))

(defn- tool-update-text
  [{:keys [text] :as args}]
  (let [ids      (require-ids args)
        objects  (dsh/lookup-page-objects @st/state)
        text-ids (filterv #(= :text (:type (get objects %))) ids)]
    (when (empty? text-ids)
      (throw (js/Error. "None of those ids is a text layer.")))
    (doseq [id text-ids]
      (let [shape    (get objects id)
            run      (or (first-text-node (:content shape)) (txt/get-default-text-attrs))
            style    (text-style-attrs args (:font-id run))
            base     (dissoc run :text)
            content  (if (string? text)
                       (text->content text (merge base style))
                       (cond->> (txt/transform-nodes txt/is-text-node? #(merge % style) (:content shape))
                         (:text-align style)
                         (txt/transform-nodes txt/is-paragraph-node?
                                              #(assoc % :text-align (:text-align style)))))]
        (st/emit! (dwsh/update-shapes [id] #(-> %
                                                 (assoc :content content)
                                                 (dissoc :position-data))))))
    (swap! turn-text-ids into text-ids)
    #js {:updated (count text-ids) :ids (ids->js text-ids)}))

;; --- reading --------------------------------------------------------------

(defn- tool-list-shapes
  [{:keys [parent_id]}]
  (let [objects (dsh/lookup-page-objects @st/state)
        root    (parse-uuid-safe parent_id)
        inside? (fn [shape]
                  (or (nil? root)
                      (loop [p (:parent-id shape)]
                        (cond
                          (or (nil? p) (= p uuid/zero)) false
                          (= p root) true
                          :else (recur (:parent-id (get objects p)))))))
        rows    (->> objects
                     (remove #(= (key %) uuid/zero))
                     (filter #(inside? (val %)))
                     (take 400)
                     (mapv describe-shape))]
    (clj->js {:count (count rows) :shapes rows})))

;; --- editing --------------------------------------------------------------

(defn- update-shapes!
  [ids f]
  (st/emit! (dwsh/update-shapes (vec ids) f))
  #js {:updated (count ids) :ids (ids->js ids)})

(defn- tool-change-fill-color
  [{:keys [fill_color fill_opacity] :as args}]
  (let [ids (require-ids args)]
    (when-not (string? fill_color)
      (throw (js/Error. "fill_color is required.")))
    (st/emit! (dwc/change-fill ids {:color fill_color :opacity (pct->opacity fill_opacity)} 0))
    #js {:updated (count ids) :ids (ids->js ids)}))

(defn- tool-set-stroke
  [{:keys [stroke_color stroke_width stroke_opacity stroke_alignment stroke_style] :as args}]
  (let [ids (require-ids args)]
    (if (and (number? stroke_width) (pos? stroke_width))
      (do
        (when-not (string? stroke_color)
          (throw (js/Error. "stroke_color is required for a stroke width above 0.")))
        (update-shapes! ids #(merge % (stroke-attrs stroke_color stroke_width stroke_opacity
                                                     stroke_alignment stroke_style))))
      (update-shapes! ids #(assoc % :strokes [])))))

(defn- tool-set-corner-radius
  [{:keys [radius top_left top_right bottom_right bottom_left] :as args}]
  (let [ids (require-ids args)]
    (update-shapes! ids (fn [shape]
                          (if (contains? #{:rect :frame :image} (:type shape))
                            (let [r (if (number? radius) radius 0)]
                              (assoc shape
                                     :r1 (if (number? top_left) top_left (if (number? radius) r (:r1 shape 0)))
                                     :r2 (if (number? top_right) top_right (if (number? radius) r (:r2 shape 0)))
                                     :r3 (if (number? bottom_right) bottom_right (if (number? radius) r (:r3 shape 0)))
                                     :r4 (if (number? bottom_left) bottom_left (if (number? radius) r (:r4 shape 0)))))
                            shape)))))

(defn- tool-set-opacity
  [{:keys [opacity] :as args}]
  (let [ids (require-ids args)]
    (when-not (number? opacity)
      (throw (js/Error. "opacity (0–100) is required.")))
    (update-shapes! ids #(assoc % :opacity (pct->opacity opacity)))))

(defn- tool-set-shadow
  [{:keys [remove offset_x offset_y blur spread color opacity inner] :as args}]
  (let [ids (require-ids args)]
    (if remove
      (update-shapes! ids #(assoc % :shadow []))
      (let [shadow {:id (uuid/next)
                    :style (if inner :inner-shadow :drop-shadow)
                    :offset-x (if (number? offset_x) offset_x 0)
                    :offset-y (if (number? offset_y) offset_y 4)
                    :blur (if (number? blur) blur 16)
                    :spread (if (number? spread) spread 0)
                    :hidden false
                    :color {:color (if (string? color) color "#000000")
                            :opacity (if (number? opacity) (pct->opacity opacity) 0.25)}}]
        (update-shapes! ids #(assoc % :shadow [shadow]))))))

(defn- tool-move-shape
  "Absolute x/y when given, otherwise a relative dx/dy from each shape's
  own position (update-position itself is absolute, so the offset is
  added to the current x/y first)."
  [{:keys [x y dx dy] :as args}]
  (when-not (or (number? x) (number? y) (number? dx) (number? dy))
    (throw (js/Error. "Give x/y for an absolute position or dx/dy for a relative move.")))
  (let [ids     (require-ids args)
        objects (dsh/lookup-page-objects @st/state)]
    (doseq [id ids]
      (when-let [shape (get objects id)]
        (st/emit! (dwt/update-position
                   id
                   {:x (cond (number? x) x (number? dx) (+ (:x shape) dx) :else (:x shape))
                    :y (cond (number? y) y (number? dy) (+ (:y shape) dy) :else (:y shape))}))))
    #js {:moved (count ids) :ids (ids->js ids)}))

(defn- tool-resize-shape
  [{:keys [width height] :as args}]
  (when-not (or (number? width) (number? height))
    (throw (js/Error. "At least one of width or height is required.")))
  (let [ids (require-ids args)]
    (when (number? width)
      (st/emit! (dwt/update-dimensions (vec ids) :width width)))
    (when (number? height)
      (st/emit! (dwt/update-dimensions (vec ids) :height height)))
    #js {:resized (count ids) :ids (ids->js ids)}))

(defn- tool-rotate-shape
  [{:keys [angle relative] :as args}]
  (when-not (number? angle)
    (throw (js/Error. "angle (degrees) is required.")))
  (let [ids (require-ids args)]
    (st/emit! (dwt/increase-rotation (vec ids) angle {:delta? (boolean relative)}))
    #js {:rotated (count ids) :ids (ids->js ids)}))

(defn- tool-rename-shape
  [{:keys [shape_id] layer-name :name}]
  (let [id (parse-uuid-safe shape_id)]
    (when-not (and id (string? layer-name) (seq layer-name))
      (throw (js/Error. "shape_id and a non-empty name are required.")))
    (update-shapes! [id] #(assoc % :name layer-name))))

(defn- tool-reorder-shape
  [{:keys [position] :as args}]
  (let [ids (require-ids args)
        loc (case position
              "front" :top
              "back" :bottom
              "forward" :up
              "backward" :down
              nil)]
    (when-not loc
      (throw (js/Error. "position must be front, back, forward or backward.")))
    (st/emit! (dws/select-shapes (into (d/ordered-set) ids)))
    (st/emit! (dw/vertical-order-selected loc))
    #js {:reordered (count ids) :ids (ids->js ids)}))

(defn- tool-set-parent
  [{:keys [parent_id] :as args}]
  (let [ids       (require-ids args)
        objects   (dsh/lookup-page-objects @st/state)
        parent-id (if (= parent_id "root") uuid/zero (parse-uuid-safe parent_id))
        parent    (get objects parent-id)]
    (when-not (or (= parent-id uuid/zero)
                  (contains? #{:frame :group} (:type parent)))
      (throw (js/Error. "parent_id must be a board or group id, or \"root\".")))
    (st/emit! (dwsh/relocate-shapes (set ids) parent-id 0))
    #js {:moved (count ids) :ids (ids->js ids) :parentId (str parent-id)}))

(defn- tool-group-shapes
  [{layer-name :name :as args}]
  (let [ids (resolve-target-ids args)]
    (when (< (count ids) 2)
      (throw (js/Error. "Need at least 2 shapes (shape_ids, or a current selection) to group.")))
    (let [group-id (uuid/next)]
      (st/emit! (dwg/group-shapes group-id ids))
      (when (string? layer-name)
        (st/emit! (dwsh/update-shapes [group-id] #(assoc % :name layer-name))))
      #js {:groupId (str group-id) :grouped (count ids)})))

(defn- tool-ungroup-shapes
  [args]
  (let [ids     (require-ids args)
        objects (dsh/lookup-page-objects @st/state)
        groups  (filterv #(= :group (:type (get objects %))) ids)]
    (when (empty? groups)
      (throw (js/Error. "None of those ids is a group.")))
    (st/emit! (dwg/ungroup-shapes groups))
    #js {:ungrouped (count groups) :ids (ids->js groups)}))

(defn- tool-set-flex-layout
  [{:keys [shape_id direction gap padding padding_horizontal padding_vertical
           align_items justify_content wrap remove]}]
  (let [id      (parse-uuid-safe shape_id)
        objects (dsh/lookup-page-objects @st/state)
        shape   (get objects id)]
    (when-not (= :frame (:type shape))
      (throw (js/Error. "shape_id must be a board.")))
    (if remove
      (do (st/emit! (dwsl/remove-layout [id]))
          #js {:layout "none" :id (str id)})
      (let [fresh? (not (:layout shape))
            ph     (or padding_horizontal padding)
            pv     (or padding_vertical padding)
            changes (cond-> {}
                      (or fresh? (string? direction))
                      (assoc :layout-flex-dir (keyword (or direction "row")))
                      (number? gap)
                      (assoc :layout-gap-type :simple
                             :layout-gap {:row-gap gap :column-gap gap})
                      (or (number? ph) (number? pv))
                      (assoc :layout-padding-type :simple
                             :layout-padding {:p1 (or pv 0) :p2 (or ph 0) :p3 (or pv 0) :p4 (or ph 0)})
                      (string? align_items)
                      (assoc :layout-align-items (keyword align_items))
                      (string? justify_content)
                      (assoc :layout-justify-content (keyword justify_content))
                      (boolean? wrap)
                      (assoc :layout-wrap-type (if wrap :wrap :nowrap)))]
        (when fresh?
          (st/emit! (dwsl/create-layout-from-id id :flex :from-frame? true :calculate-params? false)))
        (when (seq changes)
          (st/emit! (dwsl/update-layout [id] changes)))
        #js {:layout "flex" :id (str id)}))))

(defn- tool-select-shapes
  [args]
  (let [ids (require-ids args)]
    (st/emit! (dws/select-shapes (into (d/ordered-set) ids)))
    #js {:selected (count ids) :ids (ids->js ids)}))

(defn- tool-delete-shape
  [args]
  (let [ids (require-ids args)]
    (st/emit! (dwsh/delete-shapes ids))
    #js {:deleted (count ids) :ids (ids->js ids)}))

(defn srgb-channel->linear
  "One sRGB 0-255 channel to its linear-light value, per the WCAG 2.x
  relative luminance definition (the standard gamma-correction piecewise
  formula, not a perceptual-brightness approximation). Public (not
  defn-) so it and its two callers below are independently unit-testable
  — see ai_tools_test.cljs."
  [v]
  (let [c (/ v 255)]
    (if (<= c 0.03928)
      (/ c 12.92)
      (mth/pow (/ (+ c 0.055) 1.055) 2.4))))

(defn relative-luminance
  [hex]
  (let [[r g b] (ctc/hex->rgb hex)]
    (+ (* 0.2126 (srgb-channel->linear r))
       (* 0.7152 (srgb-channel->linear g))
       (* 0.0722 (srgb-channel->linear b)))))

(defn contrast-ratio
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
  {"create_frame" tool-create-frame
   "create_rectangle" tool-create-rectangle
   "create_ellipse" tool-create-ellipse
   "create_text" tool-create-text
   "list_shapes" tool-list-shapes
   "check_contrast" tool-check-contrast
   "update_text" tool-update-text
   "change_fill_color" tool-change-fill-color
   "set_stroke" tool-set-stroke
   "set_corner_radius" tool-set-corner-radius
   "set_opacity" tool-set-opacity
   "set_shadow" tool-set-shadow
   "move_shape" tool-move-shape
   "resize_shape" tool-resize-shape
   "rotate_shape" tool-rotate-shape
   "rename_shape" tool-rename-shape
   "reorder_shape" tool-reorder-shape
   "set_parent" tool-set-parent
   "group_shapes" tool-group-shapes
   "ungroup_shapes" tool-ungroup-shapes
   "set_flex_layout" tool-set-flex-layout
   "select_shapes" tool-select-shapes
   "delete_shape" tool-delete-shape})

;; Human-readable labels for display-log tool-call entries (kept in the
;; data model for a possible "what did the AI do" detail view).
(def ^:private tool-labels
  {"create_frame" "Created board"
   "create_rectangle" "Created rectangle"
   "create_ellipse" "Created ellipse"
   "create_text" "Created text"
   "list_shapes" "Read the page"
   "check_contrast" "Checked contrast"
   "update_text" "Edited text"
   "change_fill_color" "Changed fill colour"
   "set_stroke" "Changed stroke"
   "set_corner_radius" "Changed corner radius"
   "set_opacity" "Changed opacity"
   "set_shadow" "Changed shadow"
   "move_shape" "Moved shape"
   "resize_shape" "Resized shape"
   "rotate_shape" "Rotated shape"
   "rename_shape" "Renamed layer"
   "reorder_shape" "Reordered layers"
   "set_parent" "Moved into board"
   "group_shapes" "Grouped layers"
   "ungroup_shapes" "Ungrouped layers"
   "set_flex_layout" "Set flex layout"
   "select_shapes" "Selected shapes"
   "delete_shape" "Deleted shape"})

;; The file the running turn belongs to. A turn outlives a tab switch (one
;; provider round alone can take a minute) and every tool call runs against
;; whatever page is current: without this a turn started in one file would
;; carry on editing the next one, and its reply would be appended to, and
;; saved with, that file's conversation.
(defonce ^:private turn-file-id (atom nil))

(defn- turn-file-open?
  "True while the file the current turn started in is the one on screen."
  []
  (= @turn-file-id (:current-file-id @st/state)))

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
    (cond
      (nil? impl)
      (do
        (st/emit! (ai-chat/append-display-entry
                    {:role :tool-call :tool-name tool-name :label label :failed? true}))
        (.reportToolCallResult ai-api id #js {:error (str "Unknown AI tool: " tool-name)}))

      ;; The page on screen is another file's: refuse, so the model stops.
      (not (turn-file-open?))
      (.reportToolCallResult
       ai-api id
       #js {:error "The file this turn started in is no longer open. Stop; do not retry."})

      :else
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

(defn- text-layout-settled?
  "True once every text layer touched this turn has its measured layout
  and PenPot's debounced text writes are idle."
  [state ids]
  (let [objects (dsh/lookup-page-objects state)]
    (and (every? #(seq (:position-data (get objects %))) ids)
         (nil? (:app.main.data.workspace.texts/resize-text-debounce-props state))
         (nil? (:app.main.data.workspace.texts/update-position-data state)))))

(defonce ^:private commit-pending? (atom false))

(defn turn-commit-pending?
  "True between a turn's final status push and the commit of its undo
  entry. The panel must not report the turn settled before then: PenPot's
  undo ignores an open transaction and pops the previous entry, so a ⌘Z in
  that window undid the user's last action and left the turn in place."
  []
  @commit-pending?)

(defn- commit-turn-transaction!
  "Commit the turn's undo transaction once any text created or edited
  this turn has been laid out, then call on-committed. PenPot measures a
  text layer after it renders and writes the size and position data back
  on a debounce, through an undo transaction of its own; committing ours
  before that lands left a second undo entry behind the turn's, so the
  first ⌘Z appeared to do nothing."
  [tx-id on-committed]
  (let [ids   @turn-text-ids
        t0    (js/Date.now)
        done! (fn []
                (reset! turn-text-ids #{})
                ;; edit mode is started on a delay after a text shape is
                ;; added, so clear it again now that the turn is over
                (when (seq ids)
                  (st/emit! (dwe/clear-edition-mode)))
                (st/emit! (dwu/commit-undo-transaction tx-id))
                (reset! commit-pending? false)
                (on-committed))]
    (reset! commit-pending? true)
    (if (empty? ids)
      (done!)
      (letfn [(poll []
                (if (or (text-layout-settled? @st/state ids)
                        (> (- (js/Date.now) t0) 4000))
                  (js/setTimeout done! 150)
                  (js/setTimeout poll 100)))]
        (poll)))))

;; PenPot force-commits an undo transaction left open for 20 s. A turn with
;; a real page of edits takes longer than that (one provider round alone
;; can), and a force-commit splits the turn into several undo entries.
(def ^:private turn-undo-timeout-ms (* 10 60 1000))

(defn- handle-turn-status
  [status]
  (let [phase (unchecked-get status "phase")]
    (case phase
      "started"
      (let [tx-id (js/Symbol)]
        (reset! current-turn-tx tx-id)
        (reset! turn-file-id (:current-file-id @st/state))
        (reset! turn-text-ids #{})
        (st/emit! (dwu/start-undo-transaction tx-id :timeout turn-undo-timeout-ms))
        (st/emit! (ai-chat/set-status {:phase phase})))

      "tool-executing"
      (st/emit! (ai-chat/set-status {:phase phase}))

      "done"
      (let [final-text (unchecked-get status "finalText")
            tool-calls (or (unchecked-get status "toolCalls") 0)
            truncated? (true? (unchecked-get status "truncated"))
            settle!    #(st/emit! (ai-chat/set-status {:phase phase}))]
        ;; The reply belongs to the file the turn started in; another
        ;; file's conversation must not receive it.
        (when (turn-file-open?)
          (cond
            (and (string? final-text) (seq final-text))
            (st/emit! (ai-chat/append-display-entry {:role :assistant :text final-text}))

            ;; Neither text nor edits: say so, a silent turn reads as a hang.
            (zero? tool-calls)
            (st/emit! (ai-chat/append-display-entry
                       {:role :assistant :text (tr "kizuku.ai.empty-reply")})))
          (when truncated?
            (st/emit! (ai-chat/append-display-entry
                       {:role :error :text (tr "kizuku.ai.truncated")}))))
        ;; Settled only once the turn's undo entry exists.
        (if-let [tx-id @current-turn-tx]
          (do (reset! current-turn-tx nil)
              (commit-turn-transaction! tx-id settle!))
          (settle!)))

      "error"
      (let [message (unchecked-get status "message")
            settle! #(st/emit! (ai-chat/set-status
                                (if (turn-file-open?)
                                  {:phase phase :message message}
                                  ;; unlock the panel, no error bubble in
                                  ;; another file's conversation
                                  {:phase "done"})))]
        (if-let [tx-id @current-turn-tx]
          (do (reset! current-turn-tx nil)
              (commit-turn-transaction! tx-id settle!))
          (settle!)))

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
