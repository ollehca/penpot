;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.shapes.export
  "Components that generates penpot specific svg nodes with
  exportation data. This xml nodes serves mainly to enable
  importation."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.shapes :as gsh]
   [app.common.json :as json]
   [app.common.svg :as csvg]
   [app.main.ui.context :as muc]
   [app.util.object :as obj]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(def ^:private internal-counter (atom 0))

(def include-metadata-ctx
  (mf/create-context false))

(mf/defc render-xml
  [{{:keys [tag attrs content] :as node} :xml}]

  (cond
    (map? node)
    (let [props (-> (csvg/attrs->props attrs)
                    (json/->js :key-fn name))]
      [:> (d/name tag) props
       (for [child content]
         [:& render-xml {:xml child :key (swap! internal-counter inc)}])])

    (string? node)
    node

    :else
    nil))

(defn bool->str [val]
  (when (some? val) (str val)))

(defn touched->str [val]
  (str/join " " (map str val)))

(defn add-factory [shape]
  (fn add!
    ([props attr]
     (add! props attr str))

    ([props attr trfn]
     (let [val (get shape attr)
           val (if (keyword? val) (d/name val) val)
           ns-attr (-> (str "penpot:" (-> attr d/name))
                       (str/strip-suffix "?"))]
       (cond-> props
         (some? val)
         (obj/set! ns-attr (trfn val)))))))

(defn add-data
  "Adds as metadata properties that we cannot deduce from the exported SVG"
  [props shape]
  (let [add! (add-factory shape)
        frame? (= :frame (:type shape))
        group? (= :group (:type shape))
        rect?  (= :rect (:type shape))
        image? (= :image (:type shape))
        text?  (= :text (:type shape))
        path?  (= :path (:type shape))
        mask?  (and group? (:masked-group shape))
        bool?  (= :bool (:type shape))
        center (gsh/shape->center shape)]
    (-> props
        (add! :name)
        (add! :blocked)
        (add! :hidden)
        (add! :type)
        (add! :stroke-style)
        (add! :stroke-alignment)
        (add! :hide-fill-on-export)
        (add! :transform)
        (add! :transform-inverse)
        (add! :flip-x)
        (add! :flip-y)
        (add! :proportion)
        (add! :proportion-lock)
        (add! :rotation)
        (obj/set! "penpot:center-x" (-> center :x str))
        (obj/set! "penpot:center-y" (-> center :y str))

        ;; Constraints
        (add! :constraints-h)
        (add! :constraints-v)
        (add! :fixed-scroll)

        (cond-> frame?
          (-> (add! :show-content)
              (add! :hide-in-viewer)))

        (cond-> (and frame? (:use-for-thumbnail shape))
          (add! :use-for-thumbnail))

        (cond-> (and (or rect? image? frame?) (some? (:r1 shape)))
          (-> (add! :r1)
              (add! :r2)
              (add! :r3)
              (add! :r4)))

        (cond-> path?
          (-> (add! :stroke-cap-start)
              (add! :stroke-cap-end)))

        (cond-> text?
          (-> (add! :x)
              (add! :y)
              (add! :width)
              (add! :height)
              (add! :grow-type)
              (add! :content json/encode)
              (add! :position-data json/encode)))

        (cond-> mask?
          (obj/set! "penpot:masked-group" "true"))

        (cond-> bool?
          (add! :bool-type)))))

(defn add-library-refs [props shape]
  (let [add! (add-factory shape)]
    (-> props
        (add! :fill-color-ref-id)
        (add! :fill-color-ref-file)
        (add! :stroke-color-ref-id)
        (add! :stroke-color-ref-file)
        (add! :typography-ref-id)
        (add! :typography-ref-file)
        (add! :component-file)
        (add! :component-id)
        (add! :component-root)
        (add! :main-instance)
        (add! :shape-ref)
        (add! :touched touched->str))))

(defn prefix-keys [m]
  (letfn [(prefix-entry [[k v]]
            [(str "penpot:" (d/name k)) v])]
    (into {} (map prefix-entry) m)))

(defn- export-grid-data [{:keys [grids]}]
  (when (d/not-empty? grids)
    (mf/html
     [:> "penpot:grids" #js {}
      (for [{:keys [type display params]} grids]
        (let [props (->> (dissoc params :color)
                         (prefix-keys)
                         (clj->js))]
          [:> "penpot:grid"
           (-> props
               (obj/set! "penpot:color" (get-in params [:color :color]))
               (obj/set! "penpot:opacity" (get-in params [:color :opacity]))
               (obj/set! "penpot:type" (d/name type))
               (cond-> (some? display)
                 (obj/set! "penpot:display" (str display))))]))])))

(mf/defc export-flows
  [{:keys [flows]}]
  [:> "penpot:flows" #js {}
   (for [{:keys [id name starting-frame]} (vals flows)]
     [:> "penpot:flow" #js {:id id
                            :key id
                            :name name
                            :starting-frame starting-frame}])])

(mf/defc export-guides
  [{:keys [guides]}]
  [:> "penpot:guides" #js {}
   (for [{:keys [position frame-id axis]} (vals guides)]
     [:> "penpot:guide" #js {:position position
                             :frame-id frame-id
                             :axis (d/name axis)}])])

(mf/defc export-page
  {::mf/props :obj}
  [{:keys [page]}]
  (let [id     (get page :id)
        grids  (get page :grids)
        flows  (get page :flows)
        guides (get page :guides)]
    [:> "penpot:page" #js {:id id}
     (when (d/not-empty? grids)
       (let [parse-grid (fn [[type params]] {:type type :params params})
             grids (mapv parse-grid grids)]
         [:& export-grid-data {:grids grids}]))

     (when (d/not-empty? flows)
       [:& export-flows {:flows flows}])

     (when (d/not-empty? guides)
       [:& export-guides {:guides guides}])]))

(defn- export-shadow-data [{:keys [shadow]}]
  (mf/html
   (for [{:keys [style hidden color offset-x offset-y blur spread]} shadow]
     [:> "penpot:shadow"
      #js {:kizu:shadow-type (d/name style)
           :key (swap! internal-counter inc)
           :kizu:hidden (str hidden)
           :kizu:color (str (:color color))
           :kizu:opacity (str (:opacity color))
           :kizu:offset-x (str offset-x)
           :kizu:offset-y (str offset-y)
           :kizu:blur (str blur)
           :kizu:spread (str spread)}])))

(defn- export-blur-data [{:keys [blur]}]
  (when-let [{:keys [type hidden value]} blur]
    (mf/html
     [:> "penpot:blur"
      #js {:kizu:blur-type (d/name type)
           :kizu:hidden    (str hidden)
           :kizu:value     (str value)}])))

(defn export-exports-data [{:keys [exports]}]
  (mf/html
   (for [{:keys [scale suffix type]} exports]
     [:> "penpot:export"
      #js {:kizu:type   (d/name type)
           :key (swap! internal-counter inc)
           :kizu:suffix suffix
           :kizu:scale  (str scale)}])))

(defn str->style
  [style-str]
  (if (string? style-str)
    (->> (str/split style-str ";")
         (map str/trim)
         (map #(str/split % ":"))
         (group-by first)
         (map (fn [[key val]]
                (vector (keyword key) (second (first val)))))
         (into {}))
    style-str))

(defn style->str
  [style]
  (->> style
       (map (fn [[key val]] (str (d/name key) ":" val)))
       (str/join "; ")))

(defn- export-svg-data [shape]
  (mf/html
   [:*
    (when (contains? shape :svg-attrs)
      (let [svg-transform (get shape :svg-transform)
            svg-attrs     (->> shape :svg-attrs keys (mapv (comp d/name str/kebab)) (str/join ","))
            svg-defs      (->> shape :svg-defs keys (mapv d/name) (str/join ","))]
        [:> "penpot:svg-import"
         #js {:kizu:svg-attrs          (when-not (empty? svg-attrs) svg-attrs)
              ;; Style and filter are special properties so we need to save it otherwise will be indistingishible from
              ;; standard properties
              :kizu:svg-style          (when (contains? (:svg-attrs shape) :style) (style->str (get-in shape [:svg-attrs :style])))
              :kizu:svg-filter         (when (contains? (:svg-attrs shape) :filter) (get-in shape [:svg-attrs :filter]))
              :kizu:svg-defs           (when-not (empty? svg-defs) svg-defs)
              :kizu:svg-transform      (when svg-transform (str svg-transform))
              :kizu:svg-viewbox-x      (get-in shape [:svg-viewbox :x])
              :kizu:svg-viewbox-y      (get-in shape [:svg-viewbox :y])
              :kizu:svg-viewbox-width  (get-in shape [:svg-viewbox :width])
              :kizu:svg-viewbox-height (get-in shape [:svg-viewbox :height])}
         (for [[def-id def-xml] (:svg-defs shape)]
           [:> "penpot:svg-def" #js {:def-id def-id
                                     :key (swap! internal-counter inc)}
            [:& render-xml {:xml def-xml}]])]))

    (when (= (:type shape) :svg-raw)
      (let [shape (-> shape (d/update-in-when [:content :attrs :style] str->style))
            props
            (-> (obj/create)
                (obj/set! "penpot:x" (:x shape))
                (obj/set! "penpot:y" (:y shape))
                (obj/set! "penpot:width" (:width shape))
                (obj/set! "penpot:height" (:height shape))
                (obj/set! "penpot:tag" (-> (get-in shape [:content :tag]) d/name))
                (obj/merge! (-> (get-in shape [:content :attrs])
                                (clj->js))))]
        [:> "penpot:svg-content" props
         (for [leaf (->> shape :content :content (filter string?))]
           [:> "penpot:svg-child" {:key (swap! internal-counter inc)} leaf])]))]))


(defn- export-fills-data [{:keys [fills]}]
  (when-let [fills     (seq fills)]
    (let [render-id (mf/use-ctx muc/render-id)]
      (mf/html
       [:> "penpot:fills" #js {}
        (for [[index fill] (d/enumerate fills)]
          (let [fill-image-id (dm/str "fill-image-" render-id "-" index)]
            [:> "penpot:fill"
             #js {:kizu:fill-color          (cond
                                                (some? (:fill-color-gradient fill))
                                                (str/format "url(#%s)" (str "fill-color-gradient-" render-id "-" index))

                                                :else
                                                (d/name (:fill-color fill)))
                  :key                        (swap! internal-counter inc)

                  :kizu:fill-image-id       (when (:fill-image fill) fill-image-id)
                  :kizu:fill-color-ref-file (d/name (:fill-color-ref-file fill))
                  :kizu:fill-color-ref-id   (d/name (:fill-color-ref-id fill))
                  :kizu:fill-opacity        (d/name (:fill-opacity fill))}]))]))))

(defn- export-strokes-data [{:keys [strokes]}]
  (when-let [strokes (seq strokes)]
    (let [render-id (mf/use-ctx muc/render-id)]
      (mf/html
       [:> "penpot:strokes" #js {}
        (for [[index stroke] (d/enumerate strokes)]
          (let [stroke-image-id (dm/str "stroke-image-" render-id "-" index)]
            [:> "penpot:stroke"
             #js {:kizu:stroke-color          (cond
                                                  (some? (:stroke-color-gradient stroke))
                                                  (str/format "url(#%s)" (str "stroke-color-gradient-" render-id "-" index))

                                                  :else
                                                  (d/name (:stroke-color stroke)))
                  :key                          (swap! internal-counter inc)
                  :kizu:stroke-image-id       (when (:stroke-image stroke) stroke-image-id)
                  :kizu:stroke-color-ref-file (d/name (:stroke-color-ref-file stroke))
                  :kizu:stroke-color-ref-id   (d/name (:stroke-color-ref-id stroke))
                  :kizu:stroke-opacity        (d/name (:stroke-opacity stroke))
                  :kizu:stroke-style          (d/name (:stroke-style stroke))
                  :kizu:stroke-width          (d/name (:stroke-width stroke))
                  :kizu:stroke-alignment      (d/name (:stroke-alignment stroke))
                  :kizu:stroke-cap-start      (d/name (:stroke-cap-start stroke))
                  :kizu:stroke-cap-end        (d/name (:stroke-cap-end stroke))}]))]))))

(defn- export-interactions-data [{:keys [interactions]}]
  (when-let [interactions (seq interactions)]
    (mf/html
     [:> "penpot:interactions" #js {}
      (for [interaction interactions]
        [:> "penpot:interaction"
         #js {:kizu:event-type (d/name (:event-type interaction))
              :kizu:action-type (d/name (:action-type interaction))
              :kizu:delay ((d/nilf str) (:delay interaction))
              :kizu:destination ((d/nilf str) (:destination interaction))
              :kizu:overlay-pos-type ((d/nilf d/name) (:overlay-pos-type interaction))
              :kizu:overlay-position-x ((d/nilf get-in) interaction [:overlay-position :x])
              :kizu:overlay-position-y ((d/nilf get-in) interaction [:overlay-position :y])
              :kizu:url (:url interaction)
              :key (swap! internal-counter inc)
              :kizu:close-click-outside ((d/nilf str) (:close-click-outside interaction))
              :kizu:background-overlay ((d/nilf str) (:background-overlay interaction))
              :kizu:preserve-scroll ((d/nilf str) (:preserve-scroll interaction))}])])))


(defn- export-layout-container-data
  [{:keys [layout
           layout-flex-dir
           layout-gap
           layout-gap-type
           layout-wrap-type
           layout-padding-type
           layout-padding
           layout-justify-items
           layout-justify-content
           layout-align-items
           layout-align-content
           layout-grid-dir
           layout-grid-rows
           layout-grid-columns
           layout-grid-cells]}]

  (when layout
    (mf/html
     [:> "penpot:layout"
      #js {:kizu:layout (d/name layout)
           :kizu:layout-flex-dir (d/name layout-flex-dir)
           :kizu:layout-gap-type (d/name layout-gap-type)
           :kizu:layout-gap-row (:row-gap layout-gap)
           :kizu:layout-gap-column (:column-gap layout-gap)
           :kizu:layout-wrap-type (d/name layout-wrap-type)
           :kizu:layout-padding-type (d/name layout-padding-type)
           :kizu:layout-padding-p1 (:p1 layout-padding)
           :kizu:layout-padding-p2 (:p2 layout-padding)
           :kizu:layout-padding-p3 (:p3 layout-padding)
           :kizu:layout-padding-p4 (:p4 layout-padding)
           :kizu:layout-justify-items (d/name layout-justify-items)
           :kizu:layout-justify-content (d/name layout-justify-content)
           :kizu:layout-align-items (d/name layout-align-items)
           :kizu:layout-align-content (d/name layout-align-content)
           :kizu:layout-grid-dir (d/name layout-grid-dir)}

      [:> "penpot:grid-rows" #js {}
       (for [[idx {:keys [type value]}] (d/enumerate layout-grid-rows)]
         [:> "penpot:grid-track"
          #js {:kizu:index idx
               :key (swap! internal-counter inc)
               :kizu:type (d/name type)
               :kizu:value value}])]

      [:> "penpot:grid-columns" #js {}
       (for [[idx {:keys [type value]}] (d/enumerate layout-grid-columns)]
         [:> "penpot:grid-track"
          #js {:kizu:index idx
               :key (swap! internal-counter inc)
               :kizu:type (d/name type)
               :kizu:value value}])]

      [:> "penpot:grid-cells" #js {}
       (for [[_ {:keys [id
                        area-name
                        row
                        row-span
                        column
                        column-span
                        position
                        align-self
                        justify-self
                        shapes]}] layout-grid-cells]
         [:> "penpot:grid-cell"
          #js {:kizu:id id
               :key (swap! internal-counter inc)
               :kizu:area-name area-name
               :kizu:row row
               :kizu:row-span row-span
               :kizu:column column
               :kizu:column-span column-span
               :kizu:position (d/name position)
               :kizu:align-self (d/name align-self)
               :kizu:justify-self (d/name justify-self)
               :kizu:shapes (str/join " " shapes)}])]])))

(defn- export-layout-item-data
  [{:keys [layout-item-margin
           layout-item-margin-type
           layout-item-h-sizing
           layout-item-v-sizing
           layout-item-max-h
           layout-item-min-h
           layout-item-max-w
           layout-item-min-w
           layout-item-align-self
           layout-item-absolute
           layout-item-z-index]}]

  (when (or layout-item-margin
            layout-item-margin-type
            layout-item-h-sizing
            layout-item-v-sizing
            layout-item-max-h
            layout-item-min-h
            layout-item-max-w
            layout-item-min-w
            layout-item-align-self
            layout-item-absolute
            layout-item-z-index)
    (mf/html
     [:> "penpot:layout-item"
      #js {:kizu:layout-item-margin-m1 (:m1 layout-item-margin)
           :kizu:layout-item-margin-m2 (:m2 layout-item-margin)
           :kizu:layout-item-margin-m3 (:m3 layout-item-margin)
           :kizu:layout-item-margin-m4 (:m4 layout-item-margin)
           :kizu:layout-item-margin-type (d/name layout-item-margin-type)
           :kizu:layout-item-h-sizing (d/name layout-item-h-sizing)
           :kizu:layout-item-v-sizing (d/name layout-item-v-sizing)
           :kizu:layout-item-max-h layout-item-max-h
           :kizu:layout-item-min-h layout-item-min-h
           :kizu:layout-item-max-w layout-item-max-w
           :kizu:layout-item-min-w layout-item-min-w
           :kizu:layout-item-align-self (d/name layout-item-align-self)
           :kizu:layout-item-absolute layout-item-absolute
           :kizu:layout-item-z-index layout-item-z-index}])))


(mf/defc export-data
  [{:keys [shape]}]
  (let [props (-> (obj/create) (add-data shape) (add-library-refs shape))]
    [:> "penpot:shape" props
     (export-shadow-data           shape)
     (export-blur-data             shape)
     (export-exports-data          shape)
     (export-svg-data              shape)
     (export-interactions-data     shape)
     (export-fills-data            shape)
     (export-strokes-data          shape)
     (export-grid-data             shape)
     (export-layout-container-data shape)
     (export-layout-item-data      shape)]))

