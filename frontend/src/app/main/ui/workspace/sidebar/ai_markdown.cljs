;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ============================================================================
;; ADDED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; New file — a tiny Markdown subset renderer for AI assistant replies.
;; Date: 2026-10-24
;; ============================================================================

(ns app.main.ui.workspace.sidebar.ai-markdown
  "Renders the Markdown subset LLM replies actually use — paragraphs,
  headings, bullet and numbered lists (with nesting), fenced code, inline
  code, bold and italics — as plain hiccup. Nothing is ever injected as
  HTML: every piece of model output ends up as a text node, so a reply
  cannot smuggle markup into the panel. Anything outside the subset falls
  through as literal text."
  (:require
   [clojure.string :as str]
   [rumext.v2 :as mf]))

;; --- Inline -------------------------------------------------------------

;; Single * and _ emphasis only counts at a word edge: inside a word or
;; between spaced operands (hero_bg_dark, 2*4*8, 5 * 3 * 2) they stay literal.
(def ^:private inline-re
  (js/RegExp.
   (str "(\\*\\*[^*\\n]+\\*\\*"
        "|`[^`\\n]+`"
        "|(?<![A-Za-z0-9])\\*(?!\\s)[^*\\n]+(?<!\\s)\\*(?![A-Za-z0-9])"
        "|(?<![A-Za-z0-9_])_[^_\\n]+_(?![A-Za-z0-9_]))")))

(defn- inline-token->hiccup
  [token]
  (cond
    (and (str/starts-with? token "**") (str/ends-with? token "**"))
    [:strong (subs token 2 (- (count token) 2))]

    (str/starts-with? token "`")
    [:code (subs token 1 (dec (count token)))]

    :else
    [:em (subs token 1 (dec (count token)))]))

(defn parse-inline
  "Split a line into plain strings and strong/em/code nodes."
  [text]
  (loop [rest text acc []]
    ;; the match's own index: the token's text can also occur earlier, where
    ;; the word-edge rule rejected it
    (if-let [m ^js (.exec inline-re rest)]
      (let [token (aget m 0)
            idx   (.-index m)
            before (subs rest 0 idx)
            after  (subs rest (+ idx (count token)))]
        (recur after (cond-> acc
                       (seq before) (conj before)
                       :always (conj (inline-token->hiccup token)))))
      (cond-> acc (seq rest) (conj rest)))))

;; --- Blocks -------------------------------------------------------------

(def ^:private bullet-re #"^(\s*)[-*•]\s+(.*)$")
(def ^:private number-re #"^(\s*)(\d+)[.)]\s+(.*)$")
(def ^:private heading-re #"^#{1,3}\s+(.*)$")

(defn- depth [indent] (quot (count indent) 2))

(defn- classify-line
  [line]
  (cond
    (str/starts-with? (str/trim line) "```") {:type :fence}
    (str/blank? line) {:type :blank}
    :else
    (if-let [[_ indent text] (re-matches bullet-re line)]
      {:type :ul :depth (depth indent) :text text}
      (if-let [[_ indent number text] (re-matches number-re line)]
        {:type :ol :depth (depth indent) :text text :number (js/parseInt number 10)}
        (if-let [[_ text] (re-matches heading-re line)]
          {:type :h :text text}
          {:type :text :text line})))))

(defn- flush-para
  [blocks para]
  (if (seq para)
    (conj blocks {:type :p :text (str/join " " para)})
    blocks))

(defn- push-item
  "Add a list item to the list block before it, or start a new block. A new
  numbered block keeps the number its first item was written with (:start,
  left out for 1), so a list interrupted by nested bullets goes on counting."
  [blocks item]
  (let [prev (peek blocks)
        item' (dissoc item :type :number)]
    (if (and prev (= (:type prev) (:type item)))
      (conj (pop blocks) (update prev :items conj item'))
      (conj blocks (cond-> {:type (:type item) :items [item']}
                     (and (= :ol (:type item)) (not= 1 (:number item)))
                     (assoc :start (:number item)))))))

(defn blocks
  "Parse text into a vector of blocks:
   {:type :p :text} {:type :h :text} {:type :code :text}
   {:type :ul/:ol :items [{:depth :text} …]} (:ol may have :start, the
   number it begins at when that is not 1)"
  [text]
  (loop [lines (str/split-lines (or text ""))
         blocks []
         para []
         code nil]
    (if-let [line (first lines)]
      (let [{:keys [type] :as parsed} (classify-line line)]
        (cond
          (some? code)
          (if (= type :fence)
            (recur (rest lines) (conj blocks {:type :code :text (str/join "\n" code)}) [] nil)
            (recur (rest lines) blocks para (conj code line)))

          (= type :fence)
          (recur (rest lines) (flush-para blocks para) [] [])

          (= type :blank)
          (recur (rest lines) (flush-para blocks para) [] nil)

          (#{:ul :ol} type)
          (recur (rest lines) (push-item (flush-para blocks para) parsed) [] nil)

          (= type :h)
          (recur (rest lines) (conj (flush-para blocks para) {:type :h :text (:text parsed)}) [] nil)

          :else
          (recur (rest lines) blocks (conj para (str/trim line)) nil)))
      (let [blocks (flush-para blocks para)]
        (if (some? code)
          (conj blocks {:type :code :text (str/join "\n" code)})
          blocks)))))

;; --- Component ----------------------------------------------------------
;;
;; Hiccup is compiled by rumext at macro time, so anything built at runtime
;; (parse-inline's vectors, the per-block dispatch) has to go through mf/html
;; explicitly or React receives raw CLJS vectors.

(defn- render-inline
  [nodes]
  (into-array
   (map-indexed
    (fn [idx node]
      (if (string? node)
        node
        (let [[tag text] node]
          (case tag
            :strong (mf/html [:strong {:key idx} text])
            :code   (mf/html [:code {:key idx} text])
            (mf/html [:em {:key idx} text])))))
    nodes)))

(defn- render-items
  [items]
  (into-array
   (map-indexed
    (fn [idx {:keys [depth text]}]
      (mf/html
       [:li {:key idx
             ;; JS object (camelCase): a runtime-built map is not converted
             ;; for React, so nested lists never indented.
             :style (when (pos? depth) #js {:marginLeft (str (* 12 depth) "px")})}
        (render-inline (parse-inline text))]))
    items)))

(defn- render-block
  [idx {:keys [type text items start]}]
  (case type
    :h    (mf/html [:h4 {:key idx} (render-inline (parse-inline text))])
    :code (mf/html [:pre {:key idx} [:code text]])
    :ul   (mf/html [:ul {:key idx} (render-items items)])
    :ol   (mf/html [:ol {:key idx :start start} (render-items items)])
    (mf/html [:p {:key idx} (render-inline (parse-inline text))])))

(mf/defc markdown*
  "Render a Markdown subset as React elements. `class` is applied to the
  wrapper."
  [{:keys [text class]}]
  [:div {:class class}
   (into-array (map-indexed render-block (blocks text)))])
