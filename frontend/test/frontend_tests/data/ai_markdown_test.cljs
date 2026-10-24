;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ADDED BY KIZUKU (2026-10-24): unit tests for the AI reply Markdown subset.

(ns frontend-tests.data.ai-markdown-test
  (:require
   [app.main.ui.workspace.sidebar.ai-markdown :as md]
   [cljs.test :as t :include-macros true]))

(t/deftest inline-formatting
  (t/is (= ["plain"] (md/parse-inline "plain")))
  (t/is (= ["a " [:strong "bold"] " b"] (md/parse-inline "a **bold** b")))
  (t/is (= [[:code "x"] " and " [:em "y"]] (md/parse-inline "`x` and *y*")))
  (t/is (= ["<b>not html</b>"] (md/parse-inline "<b>not html</b>")))
  (t/is (= ["unterminated **bold"] (md/parse-inline "unterminated **bold"))))

(t/deftest paragraphs-and-headings
  (t/is (= [{:type :p :text "one two"} {:type :h :text "Title"} {:type :p :text "three"}]
           (md/blocks "one\ntwo\n\n## Title\nthree"))))

(t/deftest lists-with-nesting
  (t/is (= [{:type :p :text "Groups:"}
            {:type :ul :items [{:depth 0 :text "**Header**"}
                               {:depth 1 :text "Logo"}
                               {:depth 1 :text "Links"}
                               {:depth 0 :text "Footer"}]}]
           (md/blocks "Groups:\n- **Header**\n  - Logo\n  - Links\n- Footer")))
  (t/is (= [{:type :ol :items [{:depth 0 :text "first"} {:depth 0 :text "second"}]}]
           (md/blocks "1. first\n2) second"))))

(t/deftest fenced-code
  (t/is (= [{:type :p :text "see:"} {:type :code :text "let x = 1;\nx++"} {:type :p :text "done"}]
           (md/blocks "see:\n```js\nlet x = 1;\nx++\n```\ndone")))
  (t/is (= [{:type :code :text "open"}] (md/blocks "```\nopen"))))

(t/deftest empty-input
  (t/is (= [] (md/blocks nil)))
  (t/is (= [] (md/blocks ""))))
