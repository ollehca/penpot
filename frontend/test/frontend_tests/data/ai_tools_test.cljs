;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ADDED BY KIZUKU: unit tests for ai_tools.cljs's pure logic — the WCAG
;; contrast-ratio math behind the check_contrast AI tool, and the fill-
;; color/canvas-state-description helpers behind describe-canvas-state.
;; The tool-dispatch functions themselves (tool-move-shape, tool-resize-
;; shape, etc.) are NOT covered here — they call (st/emit! ...) against
;; app.main.store's real global state AND unconditionally reach into
;; window.electronAPI.ai via dispatch-tool-call, so testing them properly
;; needs either a store-rebinding harness (see
;; frontend_tests.plugins.context-shapes-test for the (set! st/state ...)
;; pattern) or a mocked electronAPI, neither of which exists yet for this
;; namespace — a real gap, left for follow-up rather than worked around
;; here by relaxing what dispatch-tool-call actually requires.

(ns frontend-tests.data.ai-tools-test
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.workspace.ai-tools :as ait]
   [cljs.test :as t :include-macros true]))

;; --- contrast-ratio / relative-luminance --------------------------------
;; Reference values are the well-known WCAG test vectors (black/white is
;; the canonical 21:1 maximum; the rest are commonly cited AA-boundary
;; examples), not just round-tripping the implementation against itself.

(t/deftest test-contrast-ratio-black-white-is-max
  (t/is (= 21.0 (ait/contrast-ratio "#000000" "#FFFFFF")))
  (t/is (= 21.0 (ait/contrast-ratio "#FFFFFF" "#000000"))
        "order shouldn't matter — ratio always uses the lighter/darker of the two"))

(t/deftest test-contrast-ratio-identical-colors-is-one
  (t/is (= 1.0 (ait/contrast-ratio "#7C3AED" "#7C3AED"))))

(t/deftest test-contrast-ratio-known-midtone-pair
  ;; #767676 on white is a commonly-cited "just barely passes AA normal
  ;; text" reference pair (~4.54:1)
  (let [ratio (ait/contrast-ratio "#767676" "#FFFFFF")]
    (t/is (> ratio 4.5))
    (t/is (< ratio 4.6))))

(t/deftest test-contrast-ratio-kata-default-text-on-dark-canvas
  ;; This app's own default text color (#f0f2f5) against its dark canvas
  ;; (#0a0a14) — this pairing was a deliberate design choice (see
  ;; app.common.types.text's default-text-fills comment); confirms it
  ;; actually passes AA by a comfortable margin, not just by eye.
  (let [ratio (ait/contrast-ratio "#f0f2f5" "#0a0a14")]
    (t/is (>= ratio 4.5) "should pass WCAG AA for normal text")))

(t/deftest test-relative-luminance-black-is-zero
  (t/is (= 0.0 (ait/relative-luminance "#000000"))))

(t/deftest test-relative-luminance-white-is-one
  (t/is (= 1.0 (ait/relative-luminance "#FFFFFF"))))

(t/deftest test-relative-luminance-white-greater-than-black
  (t/is (> (ait/relative-luminance "#FFFFFF") (ait/relative-luminance "#000000"))))

;; --- first-fill-color ----------------------------------------------------

(t/deftest test-first-fill-color-plain-vector
  (t/is (= "#FF0000" (ait/first-fill-color [{:fill-color "#FF0000" :fill-opacity 1}]))))

(t/deftest test-first-fill-color-takes-first-of-several
  (t/is (= "#FF0000" (ait/first-fill-color [{:fill-color "#FF0000" :fill-opacity 1}
                                             {:fill-color "#00FF00" :fill-opacity 1}]))))

(t/deftest test-first-fill-color-nil-when-empty
  (t/is (nil? (ait/first-fill-color [])))
  (t/is (nil? (ait/first-fill-color nil))))

;; --- text-fill-color -----------------------------------------------------

(t/deftest test-text-fill-color-reads-first-run-fill
  (let [shape {:type :text
               :fills [{:fill-color "#SHAPE-LEVEL" :fill-opacity 1}]
               :content {:type "root"
                         :children [{:type "paragraph-set"
                                     :children [{:type "paragraph"
                                                 :children [{:text "hello"
                                                             :fills [{:fill-color "#RUN-LEVEL" :fill-opacity 1}]}]}]}]}}]
    (t/is (= "#RUN-LEVEL" (ait/text-fill-color shape))
          "per-run fill (what's actually rendered) wins over the shape-level one")))

(t/deftest test-text-fill-color-falls-back-to-shape-level
  (let [shape {:type :text
               :fills [{:fill-color "#SHAPE-LEVEL" :fill-opacity 1}]
               :content {:type "root" :children []}}]
    (t/is (= "#SHAPE-LEVEL" (ait/text-fill-color shape))
          "no text runs found — falls back to the shape's own :fills")))

;; --- describe-shape --------------------------------------------------------

(t/deftest test-describe-shape-basic-rect
  (let [id    (uuid/next)
        shape {:type :rect :name "My Rect" :x 10 :y 20 :width 100 :height 50
               :fills [{:fill-color "#ABCDEF" :fill-opacity 1}]}
        out   (ait/describe-shape [id shape])]
    (t/is (= (str id) (:id out)))
    (t/is (= "rect" (:type out)))
    (t/is (= "My Rect" (:name out)))
    (t/is (= 10 (:x out)))
    (t/is (= 20 (:y out)))
    (t/is (= 100 (:width out)))
    (t/is (= 50 (:height out)))
    (t/is (= "#ABCDEF" (:fillColor out)))))

(t/deftest test-describe-shape-parent-id-nil-for-top-level
  (let [id    (uuid/next)
        shape {:type :rect :parent-id uuid/zero}
        out   (ait/describe-shape [id shape])]
    (t/is (nil? (:parentId out))
          "uuid/zero (the page's own implicit root) should never surface as a real parentId")))

(t/deftest test-describe-shape-parent-id-present-for-nested
  (let [id        (uuid/next)
        parent-id (uuid/next)
        shape     {:type :rect :parent-id parent-id}
        out       (ait/describe-shape [id shape])]
    (t/is (= (str parent-id) (:parentId out)))))

(t/deftest test-describe-shape-text-uses-text-fill-color
  (let [id    (uuid/next)
        shape {:type :text
               :fills [{:fill-color "#SHAPE-LEVEL" :fill-opacity 1}]
               :content {:type "root"
                         :children [{:type "paragraph-set"
                                     :children [{:type "paragraph"
                                                 :children [{:text "hi"
                                                             :fills [{:fill-color "#RUN-LEVEL" :fill-opacity 1}]}]}]}]}}
        out   (ait/describe-shape [id shape])]
    (t/is (= "#RUN-LEVEL" (:fillColor out))
          "text shapes should report the actually-rendered run color, not the shape-level fallback")))
