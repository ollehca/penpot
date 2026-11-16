;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

;; ============================================================================
;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; Original file from PenPot (https://github.com/penpot/penpot)
;; Licensed under Mozilla Public License Version 2.0
;; Modifications: an image the SVG links to by address (anything that is
;;   not embedded as a data: URI) is no longer sent to
;;   :create-file-media-object-from-url. Kizuku has no server to download
;;   it and the app does not fetch from the network. The image is left out,
;;   as upstream does for a failed upload, and the user is told once per
;;   SVG (kizuku.svg.linked-images-skipped).
;; Date: 2026-11-15
;; ============================================================================

(ns app.main.data.workspace.svg-upload
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.files.shapes-builder :as sb]
   [app.common.svg :as csvg]
   [app.common.types.shape-tree :as ctst]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.util.i18n :refer [tr]]
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

(defn extract-name [href]
  (let [query-idx (str/last-index-of href "?")
        href (if (> query-idx 0) (subs href 0 query-idx) href)
        filename (->> (str/split href "/") (last))
        ext-idx (str/last-index-of filename ".")]
    (if (> ext-idx 0) (subs filename 0 ext-idx) filename)))

(defn- warn-linked-images-skipped
  "Kizuku: tell the user when images were left out because the SVG links
  to them instead of containing them."
  [images]
  (when (some (fn [[href image]]
                (and (nil? image) (not (str/starts-with? href "data:"))))
              images)
    (st/emit! (ntf/warn (tr "kizuku.svg.linked-images-skipped")))))

(defn upload-images
  "Extract all bitmap images inside the svg data, and upload them, associated to the file.
  Return a map {<href> <image-data>}."
  [svg-data file-id]
  (->> (rx/from (csvg/collect-images svg-data))
       (rx/map (fn [{:keys [href] :as item}]
                 (let [item (-> item
                                (assoc :file-id file-id)
                                (assoc :is-local true)
                                (assoc :name "image"))]
                   (if (str/starts-with? href "data:")
                     (assoc item :content (wapi/data-uri->blob href))
                     (-> item
                         (assoc :name (extract-name href))
                         (assoc :url href))))))
       (rx/mapcat (fn [item]
                    ;; Kizuku: a linked image is left out, not sent to
                    ;; :create-file-media-object-from-url (see the header).
                    (->> (if (contains? item :content)
                           (rp/cmd! :upload-file-media-object (dissoc item :href))
                           (rx/of nil))
                         ;; When the image uploaded fail we skip the shape
                         ;; returning `nil` will afterward not create the shape.
                         (rx/catch #(rx/of nil))
                         (rx/map #(vector (:href item) %)))))
       (rx/reduce conj {})
       (rx/tap warn-linked-images-skipped)))

(defn add-svg-shapes
  ([svg-data position]
   (add-svg-shapes nil svg-data position nil))

  ([id svg-data position {:keys [change-selection? ignore-selection?]
                          :or {ignore-selection? false change-selection? true}}]
   (ptk/reify ::add-svg-shapes
     ptk/WatchEvent
     (watch [it state _]
       (try
         (let [id              (d/nilv id (uuid/next))
               page-id         (:current-page-id state)
               objects         (dsh/lookup-page-objects state page-id)
               selected        (if ignore-selection? #{} (dsh/lookup-selected state))
               base            (cfh/get-base-shape objects selected)

               selected-id     (first selected)
               selected-frame? (and (= 1 (count selected))
                                    (= :frame (dm/get-in objects [selected-id :type])))

               base-id         (:parent-id base)

               frame-id        (if (or selected-frame? (empty? selected)
                                       (not= :frame (dm/get-in objects [base-id :type])))
                                 (ctst/top-nested-frame objects position)
                                 base-id)

               parent-id       (if (or selected-frame? (empty? selected))
                                 frame-id
                                 base-id)

               [new-shape new-children]
               (sb/create-svg-shapes id svg-data position objects frame-id parent-id selected true)

               changes         (-> (pcb/empty-changes it page-id)
                                   (pcb/with-objects objects)
                                   (pcb/add-object new-shape))

               changes         (reduce (fn [changes new-child]
                                         (pcb/add-object changes new-child))
                                       changes
                                       new-children)

               changes         (pcb/resize-parents changes
                                                   (->> (:redo-changes changes)
                                                        (filter #(= :add-obj (:type %)))
                                                        (map :id)
                                                        (reverse)
                                                        (vec)))
               undo-id         (js/Symbol)]

           (rx/of (dwu/start-undo-transaction undo-id)
                  (dch/commit-changes changes)
                  (when change-selection?
                    (dws/select-shapes (d/ordered-set (:id new-shape))))
                  (ptk/data-event :layout/update {:ids [(:id new-shape)]})
                  (dwu/commit-undo-transaction undo-id)))

         (catch :default cause
           (js/console.error cause)
           (rx/throw (ex/error :type :svg-parser
                               :hint (ex-message cause)))))))))

