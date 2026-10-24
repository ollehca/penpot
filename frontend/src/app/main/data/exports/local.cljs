;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ============================================================================
;; ADDED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; New file — offline, in-app shape export. Licensed under MPL-2.0 like the
;; rest of the frontend.
;; Date: 2026-10-24
;; ============================================================================

(ns app.main.data.exports.local
  "Offline shape export for the desktop app.

  Upstream PenPot exports shapes by asking the cloud exporter service
  (rp/cmd! :export) to screenshot render.html in a headless browser and
  stream the file back. Kizuku ships no exporter and its local backend
  never implemented :export, so every export button used to fail with a
  404 the user never saw.

  This namespace produces the same standalone SVG the exporter renders
  (app.main.render/object-svg with embedded images and fonts), then turns
  it into the requested format without leaving the app:

    :svg              the markup itself
    :png :jpeg :webp  rasterised by the thumbnail rasterizer iframe
                      (app.main.rasterizer) at the export scale; jpeg and
                      webp are re-encoded through an OffscreenCanvas
    :pdf              printed by the Electron main process
                      (window.electronAPI.export.svgToPdf); \"Export boards
                      as PDF\" sends every board as one page of one PDF

  Several exports are zipped client-side (app.util.zip) like the
  exporter's multi-export, and progress is published through the same
  :export state slice export-progress-widget already renders, so the
  existing widget, retry button and sidebar busy state keep working."
  (:require
   ["react-dom/server" :as rds]
   [app.common.files.helpers :as cfh]
   [app.common.geom.shapes.bounds :as gsb]
   [app.common.media :as cm]
   [app.common.time :as ct]
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.fonts :as fonts]
   [app.main.rasterizer :as thr]
   [app.main.render :as render]
   [app.main.store :as st]
   [app.main.ui.shapes.text.fontfaces :as ff]
   [app.util.dom :as dom]
   [app.util.http :as http]
   [app.util.i18n :refer [tr]]
   [app.util.zip :as zip]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]
   [promesa.core :as p]
   [rumext.v2 :as mf]))

;; How long the "complete" widget lingers before clearing (same as upstream).
(def ^:private default-timeout 5000)

;; Same characters the exporter strips from file names (export_shapes.cljs).
(def ^:private sanitize-file-regex #"[\\/:*?\"<>|]")

(defn enabled?
  "True inside the Kizuku desktop shell. The plain PenPot web build
  (storybook, tests) keeps the upstream exporter path."
  []
  (and (exists? js/window)
       (some? (unchecked-get js/window "electronAPI"))))

(defn- electron-api
  []
  (unchecked-get js/window "electronAPI"))

;; --- Naming ---------------------------------------------------------------

(defn type->mtype
  [type]
  (case type
    :png  "image/png"
    :jpeg "image/jpeg"
    :webp "image/webp"
    :svg  "image/svg+xml"
    :pdf  "application/pdf"
    "application/octet-stream"))

(defn sanitize-name
  [name]
  (let [name (str/replace (or name "") sanitize-file-regex "_")]
    (if (str/blank? name) "export" name)))

(defn single-filename
  "<name><ext> — the exporter's single-export naming: handle-single-export
  uses the export's :name verbatim (the sidebar already appended the
  suffix for a lone export)."
  [{:keys [name type]}]
  (str (sanitize-name name) (cm/mtype->extension (type->mtype type))))

(defn assoc-filenames
  "Mirrors the exporter's assoc-file-name: <name><suffix>[-N]<ext>, with
  -N appended to avoid duplicate entries inside one zip."
  [exports]
  (let [used (volatile! #{})]
    (mapv (fn [{:keys [name suffix type] :as export}]
            (let [ext  (cm/mtype->extension (type->mtype type))
                  base (sanitize-name (str name suffix))
                  filename
                  (loop [index 0]
                    (let [candidate (str base
                                         (when (pos? index) (str "-" (inc index)))
                                         ext)]
                      (if (contains? @used candidate)
                        (recur (inc index))
                        candidate)))]
              (vswap! used conj filename)
              (assoc export :filename filename)))
          exports)))

;; --- Rendering ------------------------------------------------------------

(defn- subtree
  "The object plus all its descendants — what the caches need warming for."
  [objects object-id]
  (select-keys objects (into [object-id] (cfh/get-children-ids objects object-id))))

(defn- embedded-fonts-css
  "The @font-face rules for every font the subtree uses, with each font
  file inlined as a data: URI so the SVG stays self-contained — the URL
  form points at the app's own Google-fonts proxy and is dead outside it."
  [object objects]
  (let [font-refs (ff/shape->fonts object objects)]
    (if (empty? font-refs)
      (rx/of "")
      (->> (fonts/render-font-styles-cached font-refs)
           (rx/mapcat
            (fn [css]
              (let [urls (fonts/extract-fontface-urls css)]
                (if (empty? urls)
                  (rx/of css)
                  (->> (rx/from urls)
                       (rx/merge-map http/fetch-data-uri)
                       (rx/reduce (fn [css mapping]
                                    (reduce-kv (fn [css url data-uri]
                                                 (str/replace css url data-uri))
                                               css
                                               mapping))
                                  css))))))))))

(defn- render-svg
  "Observable of one standalone SVG document for the object."
  [objects object-id]
  (let [object (get objects object-id)
        scope  (subtree objects object-id)]
    (rx/concat
     ;; Warm the data-uri caches first so the embed context resolves every
     ;; image synchronously during the static render below.
     (->> (rx/merge (render/populate-images-cache scope)
                    (render/populate-fonts-cache scope))
          (rx/ignore))
     (->> (embedded-fonts-css object objects)
          (rx/map
           (fn [css]
             (let [markup (rds/renderToStaticMarkup
                           (mf/element render/object-svg
                                       #js {:objects objects
                                            :object-id object-id
                                            :embed true}))]
               ;; Appended last so these rules win over the URL-based
               ;; ones object-svg's own fontfaces-style may have emitted.
               (if (str/blank? css)
                 markup
                 (str/replace markup #"</svg>\s*$"
                              (str "<style>" css "</style></svg>"))))))))))

;; --- Encoding -------------------------------------------------------------

(defn- transcode-bitmap
  "Re-encode a PNG blob as jpeg/webp. JPEG has no alpha channel, so it is
  composited onto white first — the exporter does the same (omit-background?
  false for :jpeg)."
  [blob mtype]
  (->> (rx/from (js/createImageBitmap blob))
       (rx/mapcat
        (fn [bitmap]
          (let [width  (.-width ^js bitmap)
                height (.-height ^js bitmap)
                canvas (js/OffscreenCanvas. width height)
                ctx    (.getContext ^js canvas "2d")]
            (when (= mtype "image/jpeg")
              (set! (.-fillStyle ctx) "#ffffff")
              (.fillRect ctx 0 0 width height))
            (.drawImage ctx bitmap 0 0)
            (.close ^js bitmap)
            (rx/from (.convertToBlob ^js canvas #js {:type mtype :quality 0.92})))))))

(defn- pages->pdf
  "One PDF from a vector of #js {:svg :width :height} pages, printed by the
  Electron main process (one page per entry, each sized to its board)."
  [pages]
  (if-let [api (electron-api)]
    (->> (rx/from (.svgToPdf ^js (.-export ^js api) #js {:pages (into-array pages)}))
         (rx/map (fn [bytes]
                   (js/Blob. #js [bytes] #js {:type "application/pdf"}))))
    (rx/throw (ex-info "PDF export requires the desktop app" {:type :internal}))))

(defn- svg->pdf
  [svg width height]
  (pages->pdf [#js {:svg svg :width width :height height}]))

(defn- encode
  "Observable of the finished Blob for an already-rendered SVG."
  [svg {:keys [type scale] :or {scale 1}} width height]
  (case type
    :svg (rx/of (js/Blob. #js [svg] #js {:type "image/svg+xml"}))
    :pdf (svg->pdf svg width height)
    (:png :jpeg :webp)
    (->> (thr/render {:data svg
                      :styles ""
                      :width (* width scale)
                      :result "blob"})
         (rx/mapcat (fn [blob]
                      (if (= type :png)
                        (rx/of blob)
                        (transcode-bitmap blob (type->mtype type))))))
    (rx/throw (ex-info "unsupported export type" {:type :internal :export-type type}))))

(defn- export-blob
  "Observable of the finished Blob for one export spec."
  [state {:keys [page-id object-id] :as export}]
  (let [objects (dsh/lookup-page-objects state page-id)
        object  (get objects object-id)]
    (if (nil? object)
      (rx/throw (ex-info "export: object not found" {:type :internal :object-id object-id}))
      (let [bounds (gsb/get-object-bounds objects object {:ignore-margin? false})]
        (->> (render-svg objects object-id)
             (rx/take 1)
             (rx/mapcat #(encode % export (:width bounds) (:height bounds)))
             (rx/take 1))))))

;; --- State (the slice export-progress-widget reads) -----------------------

(defn- clear-export-state
  [id]
  (ptk/reify ::clear-export-state
    ptk/UpdateEvent
    (update [_ state]
      (let [existing-id (-> state :export :id)]
        (if (and (some? existing-id) (not= id existing-id))
          state
          (dissoc state :export))))))

(defn- initialize-export-status
  [id exports]
  (ptk/reify ::initialize-export-status
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :export {:in-progress true
                            :id id
                            :resource-id id
                            :healthy? true
                            :error false
                            :progress 0
                            :widget-visible true
                            :detail-visible true
                            :exports exports
                            :last-update (ct/now)
                            :cmd :export-shapes}))))

(defn- update-export-progress
  [done]
  (ptk/reify ::update-export-progress
    ptk/UpdateEvent
    (update [_ state]
      (update state :export assoc :progress done :last-update (ct/now) :healthy? true))))

(defn- finish-export-status
  []
  (ptk/reify ::finish-export-status
    ptk/UpdateEvent
    (update [_ state]
      (update state :export assoc :in-progress false :last-update (ct/now)))))

(defn- fail-export-status
  [cause]
  (ptk/reify ::fail-export-status
    ptk/UpdateEvent
    (update [_ state]
      (update state :export assoc
              :in-progress false
              :error (or (ex-message cause) (str cause))
              :last-update (ct/now)))))

(defn- notify-failure
  [cause]
  (js/console.error "Kizuku export failed" cause)
  (ntf/error (tr "workspace.options.exporting-object-error")))

;; --- Entry points ---------------------------------------------------------

(defn request-simple-export
  "One export → one download. Keeps upstream's :in-progress/uuid/zero
  contract so the sidebar's export button shows its busy state."
  [{:keys [export]}]
  (ptk/reify ::request-simple-export
    ptk/UpdateEvent
    (update [_ state]
      (update state :export assoc :in-progress true :id uuid/zero))

    ptk/WatchEvent
    (watch [_ state _]
      (let [filename (single-filename export)]
        (->> (export-blob state export)
             (rx/map (fn [blob]
                       (dom/trigger-download filename blob)
                       (clear-export-state uuid/zero)))
             (rx/catch (fn [cause]
                         (rx/of (clear-export-state uuid/zero)
                                (notify-failure cause)))))))))

(defn- build-zip
  "Promise of a zip Blob holding every [filename blob] entry, in order."
  [entries]
  (let [writer (zip/writer (zip/blob-writer :mtype "application/zip"))]
    (-> (p/loop [entries (seq entries)]
          (when-let [[filename blob] (first entries)]
            (p/do (zip/add writer filename blob)
                  (p/recur (rest entries)))))
        (p/then (fn [_] (zip/close writer))))))

(defn- export-entries
  "Observable emitting the growing vector of [filename blob] entries, one
  emission per finished export, strictly in order (each export only starts
  once the previous one is done, which also keeps the rasterizer queue
  shallow)."
  [state exports]
  (->> (apply rx/concat
              (map (fn [export]
                     (->> (export-blob state export)
                          (rx/map (fn [blob] [(:filename export) blob]))))
                   exports))
       (rx/scan conj [])))

(defn- frame-page
  "Observable of one PDF page (#js {:svg :width :height}) for a board."
  [state {:keys [page-id object-id]}]
  (let [objects (dsh/lookup-page-objects state page-id)
        object  (get objects object-id)]
    (if (nil? object)
      (rx/throw (ex-info "export: board not found" {:type :internal :object-id object-id}))
      (let [{:keys [width height]} (gsb/get-object-bounds objects object {:ignore-margin? false})]
        (->> (render-svg objects object-id)
             (rx/take 1)
             (rx/map (fn [svg] #js {:svg svg :width width :height height})))))))

(defn request-frames-pdf
  "\"Export boards as PDF\": every chosen board becomes one page of a single
  PDF named after the file. Drives the same :export widget state as the
  zip path, one tick per rendered board."
  [{:keys [exports]}]
  (ptk/reify ::request-frames-pdf
    ptk/WatchEvent
    (watch [_ state _]
      (let [id       (uuid/next)
            total    (count exports)
            file-name (or (some-> (dsh/lookup-file state) :name) (:name (first exports)))
            pdf-name (str (sanitize-name file-name) ".pdf")]
        (swap! st/ongoing-tasks conj :export)
        (rx/concat
         (rx/of (initialize-export-status id exports))
         (->> (apply rx/concat (map #(frame-page state %) exports))
              (rx/scan conj [])
              (rx/mapcat
               (fn [pages]
                 (if (< (count pages) total)
                   (rx/of (update-export-progress (count pages)))
                   (rx/concat
                    (rx/of (update-export-progress total))
                    (->> (pages->pdf pages)
                         (rx/map (fn [blob]
                                   (dom/trigger-download pdf-name blob)
                                   (finish-export-status))))
                    (->> (rx/of (clear-export-state id))
                         (rx/delay default-timeout))))))
              (rx/catch (fn [cause]
                          (rx/of (fail-export-status cause)
                                 (notify-failure cause))))
              (rx/finalize #(swap! st/ongoing-tasks disj :export))))))))

(defn request-multiple-export
  "Several exports → one zip, with live progress in the export widget."
  [{:keys [exports]}]
  (ptk/reify ::request-multiple-export
    ptk/WatchEvent
    (watch [_ state _]
      (let [id       (uuid/next)
            exports  (assoc-filenames exports)
            total    (count exports)
            zip-name (str (sanitize-name (:name (first exports))) ".zip")]
        (swap! st/ongoing-tasks conj :export)
        (rx/concat
         (rx/of (initialize-export-status id exports))
         (->> (export-entries state exports)
              (rx/mapcat
               (fn [entries]
                 (if (< (count entries) total)
                   (rx/of (update-export-progress (count entries)))
                   (rx/concat
                    (rx/of (update-export-progress total))
                    (->> (rx/from (build-zip entries))
                         (rx/map (fn [blob]
                                   (dom/trigger-download zip-name blob)
                                   (finish-export-status))))
                    (->> (rx/of (clear-export-state id))
                         (rx/delay default-timeout))))))
              (rx/catch (fn [cause]
                          (rx/of (fail-export-status cause)
                                 (notify-failure cause))))
              (rx/finalize #(swap! st/ongoing-tasks disj :export))))))))
