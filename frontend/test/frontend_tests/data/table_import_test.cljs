;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ADDED BY KIZUKU (2026-10-25): unit tests for the CSV → table builder.

(ns frontend-tests.data.table-import-test
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.workspace.table-import :as ti]
   [cljs.test :as t :include-macros true]))

(t/deftest normalise-rows-basics
  (let [{:keys [columns rows truncated]} (ti/normalise-rows [["Name" "Qty" ""] ["Tea" "2"] ["" ""] ["Coffee" "1" ""]])]
    (t/is (= ["Name" "Qty"] columns))
    (t/is (= [["Tea" "2"] ["Coffee" "1"]] rows))
    (t/is (= {:rows 0 :cols 0} truncated))))

(t/deftest normalise-rows-empty-and-caps
  (t/is (= [] (:columns (ti/normalise-rows [[""] ["" ""]]))))
  (let [big (vec (repeat 150 (vec (map str (range 30)))))
        {:keys [columns rows truncated]} (ti/normalise-rows big)]
    (t/is (= 20 (count columns)))
    (t/is (= 100 (count rows)))
    (t/is (= {:rows 49 :cols 10} truncated))))

(t/deftest tsv-detection
  (t/is (= [["a" "b"] ["1" "2"]] (ti/tsv->rows "a\tb\n1\t2\n")))
  (t/is (nil? (ti/tsv->rows "a\tb")))
  (t/is (nil? (ti/tsv->rows "a\tb\n1\t2\t3")))
  (t/is (nil? (ti/tsv->rows "plain text\nmore text")))
  (t/is (nil? (ti/tsv->rows nil))))

(t/deftest column-widths-are-clamped
  (let [w (ti/column-widths ["A" "Long header name" "X"]
                            [["1" "short" (apply str (repeat 200 "x"))]])]
    (t/is (= 3 (count w)))
    (t/is (= 72 (nth w 0)))
    (t/is (= 320 (nth w 2)))
    (t/is (< 72 (nth w 1) 320))))

(t/deftest row-height-grows-with-wrapping
  (let [widths [100 320]
        short  (ti/row-height ["a" "b"] widths)
        long   (ti/row-height ["a" (apply str (repeat 150 "x"))] widths)]
    (t/is (> long short))
    (t/is (> short 20))))

(t/deftest build-table-shapes-structure
  (let [{:keys [table-id shapes]} (ti/build-table-shapes {:name "T" :columns ["A" "B"] :rows [["1" "2"] ["3" "4"]]}
                                                         100 200 uuid/zero)
        by-id   (into {} (map (juxt :id identity)) shapes)
        table   (get by-id table-id)
        rows    (filter #(= table-id (:parent-id %)) shapes)
        cells   (filter #(contains? (set (map :id rows)) (:parent-id %)) shapes)
        texts   (filter #(= :text (:type %)) shapes)]
    (t/is (= (+ 1 3 6 6) (count shapes)))
    (t/is (= :flex (:layout table)))
    (t/is (= :column (:layout-flex-dir table)))
    (t/is (= [100 200] [(:x table) (:y table)]))
    (t/is (= 3 (count rows)))
    (t/is (every? #(= :row (:layout-flex-dir %)) rows))
    (t/is (= 6 (count cells)))
    (t/is (every? #(= :fix (:layout-item-v-sizing %)) cells))
    (t/is (= 6 (count texts)))
    (t/is (every? #(= :fill (:layout-item-h-sizing %)) texts))
    ;; parents always precede children
    (t/is (every? (fn [[idx shape]]
                    (or (= (:id shape) table-id)
                        (some #(= (:id %) (:parent-id shape)) (take idx shapes))))
                  (map-indexed vector shapes)))
    ;; every cell is positioned inside the table
    (t/is (every? #(and (>= (:x %) (:x table)) (>= (:y %) (:y table))) cells))))
