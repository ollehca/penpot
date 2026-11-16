;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; MODIFIED BY KIZUKU (2026-11-15): tests for the colour new text takes
;; from the background it is placed on.

(ns common-tests.types.text-test
  (:require

   [app.common.types.shape :as cts]
   [app.common.types.text :as cttx]
   [app.common.uuid :as uuid]
   [clojure.test :as t :include-macros true]))

(def content-base
  (-> (cts/setup-shape {:type :text :x 0 :y 0 :grow-type :auto-width})
      (get :content)
      (cttx/change-text "hello world")))

(def content-changed-text
  (assoc-in content-base [:children 0 :children 0 :children 0 :text] "changed"))

(def content-changed-attr
  (assoc-in content-base [:children 0 :children 0 :children 0 :font-size] "32"))

(def content-changed-both
  (-> content-base
      (assoc-in [:children 0 :children 0 :children 0 :text] "changed")
      (assoc-in  [:children 0 :children 0 :children 0 :font-size] "32")))

(def line
  (get-in content-base [:children 0 :children 0 :children 0]))

(def content-changed-structure
  (update-in content-base [:children 0 :children 0 :children]
             #(conj % (assoc line :font-weight "700"))))


(def content-changed-structure-same-attrs
  (update-in content-base [:children 0 :children 0 :children] #(conj % line)))

(t/deftest test-get-diff-type
  (let [diff-text                 (cttx/get-diff-type content-base content-changed-text)
        diff-attr                 (cttx/get-diff-type content-base content-changed-attr)
        diff-both                 (cttx/get-diff-type content-base content-changed-both)
        diff-structure            (cttx/get-diff-type content-base content-changed-structure)
        diff-structure-same-attrs (cttx/get-diff-type content-base content-changed-structure-same-attrs)]

    (t/is (= #{:text-content-text} diff-text))
    (t/is (= #{:text-content-attribute} diff-attr))
    (t/is (= #{:text-content-text :text-content-attribute} diff-both))
    (t/is (= #{:text-content-structure} diff-structure))
    (t/is (= #{:text-content-structure} diff-structure-same-attrs))))


(t/deftest test-get-diff-attrs
  (let [attrs-text                 (cttx/get-diff-attrs content-base content-changed-text)
        attrs-attr                 (cttx/get-diff-attrs content-base content-changed-attr)
        attrs-both                 (cttx/get-diff-attrs content-base content-changed-both)
        attrs-structure            (cttx/get-diff-attrs content-base content-changed-structure)
        attrs-structure-same-attrs (cttx/get-diff-attrs content-base content-changed-structure-same-attrs)]

    (t/is (= #{} attrs-text))
    (t/is (= #{:font-size} attrs-attr))
    (t/is (= #{:font-size} attrs-both))
    (t/is (= #{} attrs-structure))
    (t/is (= #{} attrs-structure-same-attrs))))


(t/deftest test-equal-structure
  (t/is (true? (cttx/equal-structure? content-base content-changed-text)))
  (t/is (true? (cttx/equal-structure? content-base content-changed-attr)))
  (t/is (true? (cttx/equal-structure? content-base content-changed-both)))
  (t/is (false? (cttx/equal-structure? content-base content-changed-structure))))


(t/deftest test-copy-text-keys
  (let [copy-base-to-changed-text (cttx/copy-text-keys content-base content-changed-text)
        copy-changed-text-to-base (cttx/copy-text-keys content-changed-text content-base)

        copy-base-to-changed-attr (cttx/copy-text-keys content-base content-changed-attr)

        copy-changes-text-to-changed-attr (cttx/copy-text-keys content-changed-text content-changed-attr)
        updates-text-in-changed-attr (assoc-in content-changed-attr [:children 0 :children 0 :children 0 :text] "changed")]

    ;; If we copy the text of the base to the content-changed-text, the result is equal than the base
    (t/is (= copy-base-to-changed-text content-base))

    ;; If we copy the text of the content-changed-text to the base, the result is equal than the content-changed-text
    (t/is (= copy-changed-text-to-base content-changed-text))

    ;; If we copy the text of the base to the content-changed-attr, it doesn't nothing because the text were equal
    (t/is (= copy-base-to-changed-attr content-changed-attr))

    ;; If we copy the text of the content-changed-text to the content-changed-attr, it keeps the changes on the attrs
    ;; and the changes on the texts
    (t/is (= copy-changes-text-to-changed-attr updates-text-in-changed-attr))))


(t/deftest test-copy-attrs-keys
  (let [attrs (-> (cttx/get-first-paragraph-text-attrs content-changed-structure-same-attrs)
                  (assoc :font-size "32"))
        updated (cttx/copy-attrs-keys content-changed-structure-same-attrs attrs)
        get-font-sizes (fn get-font-sizes [fonts item]
                         (let [font-size (:font-size item)
                               fonts (if font-size (conj fonts font-size) fonts)]
                           (if (seq (:children item))
                             (reduce get-font-sizes fonts (:children item))
                             fonts)))
        original-font-sizes (get-font-sizes [] content-changed-structure-same-attrs)
        updated-font-sizes (get-font-sizes [] updated)]

    (t/is (every? #(= % "14") original-font-sizes))
    (t/is (every? #(= % "32") updated-font-sizes))))

;; KIZUKU: the default colour of new text follows its background.

(t/deftest test-text-color-on-background
  (t/testing "black where black reads better, the light default elsewhere"
    (t/is (= "#000000" (cttx/text-color-on "#FFFFFF")))
    (t/is (= "#000000" (cttx/text-color-on "#e8e9ea")))
    (t/is (= "#000000" (cttx/text-color-on "#808080")))
    (t/is (= "#f0f2f5" (cttx/text-color-on "#0a0a14")))
    (t/is (= "#f0f2f5" (cttx/text-color-on "#606060")))
    (t/is (= "#f0f2f5" (cttx/text-color-on "#0000ff"))))

  (t/testing "what is not a 6-digit hex colour counts as the dark canvas"
    (t/is (= "#f0f2f5" (cttx/text-color-on nil)))
    (t/is (= "#f0f2f5" (cttx/text-color-on "#fff")))
    (t/is (= "#f0f2f5" (cttx/text-color-on "rgb(255,255,255)"))))

  (t/testing "the fills carry that colour"
    (t/is (= [{:fill-color "#000000" :fill-opacity 1}]
             (vec (cttx/get-default-text-fills-on "#ffffff"))))))

(t/deftest test-background-color-at
  (let [board    (uuid/next)
        dark     (uuid/next)
        clear    (uuid/next)
        gradient (uuid/next)
        objects  {uuid/zero {:id uuid/zero :type :frame :frame-id uuid/zero}
                  board     {:id board :type :frame :frame-id uuid/zero
                             :fills [{:fill-color "#FFFFFF" :fill-opacity 1}]}
                  dark      {:id dark :type :frame :frame-id board
                             :fills [{:fill-color "#1a1a2e" :fill-opacity 1}]}
                  clear     {:id clear :type :frame :frame-id board :fills []}
                  gradient  {:id gradient :type :frame :frame-id uuid/zero
                             :fills [{:fill-color-gradient {:type :linear} :fill-opacity 1}]}}]

    (t/testing "a board's own colour"
      (t/is (= "#FFFFFF" (cttx/background-color-at objects board nil)))
      (t/is (= "#1a1a2e" (cttx/background-color-at objects dark nil))))

    (t/testing "a board without a colour shows the one around it"
      (t/is (= "#FFFFFF" (cttx/background-color-at objects clear nil))))

    (t/testing "outside every board: the page colour, else the default canvas"
      (t/is (= "#0a0a14" (cttx/background-color-at objects uuid/zero nil)))
      (t/is (= "#E8E9EA" (cttx/background-color-at objects uuid/zero "#E8E9EA")))
      (t/is (= "#0a0a14" (cttx/background-color-at objects gradient nil)))
      (t/is (= "#0a0a14" (cttx/background-color-at objects (uuid/next) nil))))))
