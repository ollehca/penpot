;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ============================================================================
;; ADDED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; New file — "Import table from CSV": turns tabular data into a real,
;; editable table of boards and text on the current page. Licensed under
;; MPL-2.0 like the rest of the frontend.
;; Date: 2026-10-25
;; ============================================================================

(ns app.main.data.workspace.table-import
  "Tabular data (a dropped/opened CSV, or pasted spreadsheet cells) → a table
  laid out with nested flex boards:

    table   flex column, 1px gaps and padding, filled with the grid-line
            colour — the gaps ARE the hairlines, so there are no doubled
            borders anywhere
    row     flex row, 1px gaps, transparent
    cell    flex board with padding, fixed width per column and a fixed,
            generously estimated row height — PenPot's hugging rows can't
            stretch `fill` children (they collapse) and ignore min-height
            on hugging ones, so fixed is the only way every cell in a row
            lines up; striped fills, a tinted header row
    text    fills the cell width and grows in height, so long values wrap

  Everything is created in ONE change batch inside ONE undo transaction, then
  the layout engine is asked to re-run, the table is selected and zoomed to.
  The first row is always the header — predictable beats clever here."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.shapes-helpers :as cfsh]
   [app.common.math :as mth]
   [app.common.types.shape :as cts]
   [app.common.types.shape-tree :as ctst]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.main.data.workspace.zoom :as dwz]
   [app.main.store :as st]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [clojure.string :as str]
   [potok.v2.core :as ptk]))

;; --- Limits & look --------------------------------------------------------

(def ^:private max-rows 100)
(def ^:private max-cols 20)
(def ^:private max-cell-chars 500)

(def ^:private font-size 13)
(def ^:private line-height 1.2)
(def ^:private pad-x 12)
(def ^:private pad-y 8)
(def ^:private min-col-width 72)
(def ^:private max-col-width 320)
(def ^:private char-width 0.6) ; em — generous for Source Sans Pro, so rows never run short

(def ^:private color-grid "#E5E7EB")
(def ^:private color-header-bg "#F3F4F6")
(def ^:private color-row-bg "#FFFFFF")
(def ^:private color-row-alt-bg "#F9FAFB")
(def ^:private color-header-text "#111827")
(def ^:private color-body-text "#374151")

;; --- Normalising input ----------------------------------------------------

(defn- clamp-cell
  [value]
  (let [text (str/trim (str (or value "")))]
    (if (> (count text) max-cell-chars)
      (str (subs text 0 (dec max-cell-chars)) "…")
      text)))

(defn normalise-rows
  "Rectangular, bounded grid: blank rows dropped, short rows padded, trailing
  empty columns trimmed, caps applied. Returns {:columns :rows :truncated}
  where :columns is the first (header) row. Pure."
  [rows]
  (let [clean (->> rows
                   (map #(mapv clamp-cell %))
                   (filter #(some seq %))
                   (vec))]
    (if (empty? clean)
      {:columns [] :rows [] :truncated {:rows 0 :cols 0}}
      (let [width (reduce (fn [w row]
                            (max w (inc (or (last (keep-indexed #(when (seq %2) %1) row)) -1))))
                          0 clean)
            cols  (min width max-cols)
            pad   (fn [row] (vec (take cols (concat row (repeat "")))))
            body  (->> (rest clean) (take max-rows) (mapv pad))
            head  (->> (pad (first clean))
                       (map-indexed (fn [idx h] (if (seq h) h (str (tr "kizuku.table.column") " " (inc idx)))))
                       (vec))]
        {:columns head
         :rows body
         :truncated {:rows (max 0 (- (dec (count clean)) max-rows))
                     :cols (max 0 (- width max-cols))}}))))

(defn tsv->rows
  "Rows from tab-separated text, or nil when it doesn't look like a grid:
  at least two non-blank lines, every one with the same number of tabs
  (≥1). A single line is just text."
  [text]
  (when (string? text)
    (let [lines (->> (str/split-lines text) (remove str/blank?) vec)
          counts (mapv #(count (re-seq #"\t" %)) lines)]
      (when (and (>= (count lines) 2)
                 (pos? (first counts))
                 (apply = counts))
        (mapv #(str/split % #"\t") lines)))))

(defn html-table->rows
  "Rows from the first <table> in an HTML fragment (what spreadsheets put on
  the clipboard next to the TSV text), or nil."
  [html]
  (when (and (string? html) (re-find #"(?i)<table" html))
    (let [doc   (.parseFromString (js/DOMParser.) html "text/html")
          table (.querySelector doc "table")]
      (when table
        (let [nodes (fn [root sel] (seq (.from js/Array (.querySelectorAll root sel))))
              rows  (mapv (fn [tr]
                            (mapv #(str/trim (.-textContent ^js %)) (nodes tr "td, th")))
                          (nodes table "tr"))]
          (when (seq rows) rows))))))

(defn table-paste?
  "Does this paste carry a grid worth turning into a table (≥2 rows and ≥2
  columns)? A single copied cell must stay a plain text paste."
  [html text]
  (let [rows (or (tsv->rows text) (html-table->rows html))]
    (boolean (and rows
                  (>= (count rows) 2)
                  (>= (apply max (map count rows)) 2)))))

;; --- Geometry -------------------------------------------------------------

(defn- text-width
  [text bold?]
  (* (count text) font-size char-width (if bold? 1.08 1)))

(defn column-widths
  "One width per column: the widest cell (header counts bold), padded and
  clamped so a single long value cannot blow the table up."
  [columns rows]
  (vec
   (for [c (range (count columns))]
     (let [widest (reduce max
                          (text-width (nth columns c) true)
                          (map #(text-width (nth % c "") false) rows))]
       (mth/round (mth/clamp (+ widest (* 2 pad-x)) min-col-width max-col-width))))))

(defn- cell-lines
  [text width]
  (let [avail (- width (* 2 pad-x))]
    (reduce + (map #(max 1 (mth/ceil (/ (text-width % false) avail)))
                   (str/split-lines (if (str/blank? text) " " text))))))

(defn row-height
  "Tallest wrapped cell in the row, plus padding."
  [cells widths]
  (let [lines (reduce max 1 (map-indexed (fn [c text] (cell-lines text (nth widths c))) cells))]
    (mth/round (+ 2 (* 2 pad-y) (* lines font-size line-height)))))

;; --- Shapes ---------------------------------------------------------------

(defn- fill [color] [{:fill-color color :fill-opacity 1}])

(def ^:private flex-base
  {:layout                 :flex
   :layout-gap-type        :multiple
   :layout-align-items     :start
   :layout-justify-content :start
   :layout-align-content   :stretch
   :layout-wrap-type       :nowrap
   :layout-padding-type    :simple})

(defn- flex
  [dir gap padding]
  (assoc flex-base
         :layout-flex-dir dir
         :layout-gap {:row-gap gap :column-gap gap}
         :layout-padding {:p1 padding :p2 padding :p3 padding :p4 padding}))

(defn- frame
  [attrs parent-id frame-id]
  (cts/setup-shape (merge {:type :frame :id (uuid/next) :fills [] :strokes []}
                          attrs
                          {:parent-id parent-id :frame-id frame-id})))

(defn- text-content
  [text bold? color]
  (let [attrs (merge (txt/get-default-text-attrs)
                     {:font-size (str font-size)
                      :line-height (str line-height)
                      :font-weight (if bold? "700" "400")
                      :font-variant-id (if bold? "bold" "regular")
                      :fills (fill color)})]
    {:type "root"
     :children [{:type "paragraph-set"
                 :children (mapv #(hash-map :type "paragraph" :children [(assoc attrs :text %)])
                                 (str/split-lines (if (str/blank? text) " " text)))}]}))

(defn- text-shape
  [text bold? color x y width height parent-id frame-id]
  (cts/setup-shape {:type :text
                    :id (uuid/next)
                    :name (if (str/blank? text) "—" (subs text 0 (min 40 (count text))))
                    :x x :y y :width width :height height
                    :grow-type :auto-height
                    :layout-item-h-sizing :fill
                    :layout-item-v-sizing :auto
                    :content (text-content text bold? color)
                    :parent-id parent-id
                    :frame-id frame-id}))

(defn- cell-shapes
  "The cell board and its text, positioned absolutely as the layout engine
  will place them."
  [{:keys [x y width height text bold? bg fg name parent-id]}]
  (let [cell (frame {:name name
                     :x x :y y :width width :height height
                     :fills (fill bg)
                     :show-content true
                     :layout-item-h-sizing :fix
                     :layout-item-v-sizing :fix
                     :layout-align-items :start}
                    parent-id parent-id)
        cell (merge cell (flex :row 0 0) {:layout-padding {:p1 pad-y :p2 pad-x :p3 pad-y :p4 pad-x}})]
    [cell
     (text-shape text bold? fg
                 (+ x pad-x) (+ y pad-y)
                 (- width (* 2 pad-x)) (* font-size line-height)
                 (:id cell) (:id cell))]))

(defn build-table-shapes
  "Every shape for the table, parents before children, absolutely positioned
  with the table's top-left at [tx ty]. Returns {:table-id :shapes}.

  Children are emitted LAST-FIRST (last row, then its last cell…): a flex
  board lays its children out in reverse of the :shapes vector (the topmost
  layer comes first), so appending in reading order would render the table
  upside-down and right-to-left."
  [{:keys [name columns rows]} tx ty root-frame-id]
  (let [widths   (column-widths columns rows)
        all-rows (into [columns] rows)
        heights  (mapv #(row-height % widths) all-rows)
        t-width  (+ 2 (reduce + widths) (dec (count widths)))
        t-height (+ 2 (reduce + heights) (dec (count heights)))
        table    (merge (frame {:name (str (tr "kizuku.table.name") " · " name)
                                :x tx :y ty :width t-width :height t-height
                                :fills (fill color-grid)
                                :show-content true
                                :layout-item-h-sizing :auto
                                :layout-item-v-sizing :auto}
                               root-frame-id root-frame-id)
                        (flex :column 1 1))
        table-id (:id table)
        row-y    (vec (reductions (fn [y rh] (+ y rh 1)) (+ ty 1) heights))
        col-x    (vec (reductions (fn [x w] (+ x w 1)) (+ tx 1) widths))
        row-shapes
        (fn [idx]
          (let [cells   (nth all-rows idx)
                header? (zero? idx)
                rh      (nth heights idx)
                y       (nth row-y idx)
                row     (merge (frame {:name (if header?
                                               (tr "kizuku.table.header")
                                               (tr "kizuku.table.row" idx))
                                       :x (+ tx 1) :y y
                                       :width (- t-width 2) :height rh
                                       :show-content true
                                       :layout-item-h-sizing :auto
                                       :layout-item-v-sizing :auto}
                                      table-id table-id)
                               (flex :row 1 0))
                bg      (cond header? color-header-bg
                              (odd? idx) color-row-bg
                              :else color-row-alt-bg)]
            (into [row]
                  (mapcat (fn [c]
                            (cell-shapes {:x (nth col-x c) :y y
                                          :width (nth widths c) :height rh
                                          :text (nth cells c "")
                                          :bold? header?
                                          :bg bg
                                          :fg (if header? color-header-text color-body-text)
                                          :name (nth columns c)
                                          :parent-id (:id row)})))
                  (reverse (range (count widths))))))]
    {:table-id table-id
     :shapes (into [table] (mapcat row-shapes) (reverse (range (count all-rows))))}))

;; --- Events ---------------------------------------------------------------

(defn create-table
  "Create a table from normalised data at the viewport centre."
  [{:keys [columns rows truncated] :as data}]
  (ptk/reify ::create-table
    ptk/WatchEvent
    (watch [it state _]
      (let [page-id (:current-page-id state)
            objects (dsh/lookup-page-objects state page-id)
            center  (or (dsh/get-viewport-center state) {:x 0 :y 0})
            ;; Size first (needs the data only) to centre the table.
            widths  (column-widths columns rows)
            heights (mapv #(row-height % widths) (into [columns] rows))
            t-width (+ 2 (reduce + widths) (dec (count widths)))
            t-height (+ 2 (reduce + heights) (dec (count heights)))
            tx      (mth/round (- (:x center) (/ t-width 2)))
            ty      (mth/round (- (:y center) (/ t-height 2)))
            root    (ctst/top-nested-frame objects {:x tx :y ty})
            {:keys [table-id shapes]} (build-table-shapes data tx ty root)
            changes (reduce (fn [changes shape]
                              (second (cfsh/prepare-add-shape changes shape (pcb/get-objects changes))))
                            (-> (pcb/empty-changes it page-id)
                                (pcb/with-objects objects))
                            shapes)
            undo-id (js/Symbol)]
        (rx/of (dwu/start-undo-transaction undo-id)
               (dch/commit-changes changes)
               (ptk/data-event :layout/update {:ids [table-id]})
               (dws/select-shapes (d/ordered-set table-id))
               dwz/zoom-to-selected-shape
               (dwu/commit-undo-transaction undo-id)
               (ntf/success (tr "kizuku.table.created" (count rows) (count columns)))
               (when (or (pos? (:rows truncated 0)) (pos? (:cols truncated 0)))
                 (ntf/warn (tr "kizuku.table.truncated" (count rows) (count columns)))))))))

(defn create-table-from-rows
  "Raw rows (first row = header) → normalise → create; a toast when there is
  nothing usable."
  [name rows]
  (let [{:keys [columns] :as data} (normalise-rows rows)]
    (if (empty? columns)
      (ntf/warn (tr "kizuku.table.failed"))
      (create-table (assoc data :name name)))))

(defn paste-table
  "Paste handler for spreadsheet cells / HTML tables (see clipboard.cljs)."
  [html text]
  (let [rows (or (tsv->rows text) (html-table->rows html))]
    (create-table-from-rows (tr "kizuku.table.pasted") rows)))

(defn- electron-table-api
  []
  (when (exists? js/window)
    (some-> (unchecked-get js/window "electronAPI")
            (unchecked-get "table"))))

(defn handle-shell-table
  "A table pushed by the Electron shell (File menu, dropped .csv/.tsv)."
  [payload]
  (let [{:keys [name columns rows truncated]} (js->clj payload :keywordize-keys true)]
    (if (nil? (:current-page-id @st/state))
      (st/emit! (ntf/warn (tr "kizuku.table.no-file")))
      (st/emit! (create-table {:name (or name (tr "kizuku.table.name"))
                               :columns (vec columns)
                               :rows (mapv vec rows)
                               :truncated (or truncated {:rows 0 :cols 0})})))))

(defn init!
  "Listen for tables pushed by the shell. No-op in the browser."
  []
  (when-let [api (electron-table-api)]
    (.onCreate ^js api handle-shell-table)))
