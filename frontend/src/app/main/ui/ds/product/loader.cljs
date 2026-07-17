;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; MODIFIED BY KIZUKU (2026-07-17): PenPot's spinning-pencil loader
;; replaced with the Kizuku triangle mark (canonical geometry from
;; src/frontend-integration/kizuku-svg-logo.js): a faint ghost of the
;; full mark with the outer ring drawing itself in teal via a CSS
;; stroke-dashoffset animation (see loader.scss). Public API of loader*
;; (class/width/height/title/overlay/file-loading/children) unchanged;
;; only the icon internals and the width:height ratio (pencil 100:27 ->
;; mark 1220:1044) differ.

(ns app.main.ui.ds.product.loader
  (:require-macros
   [app.common.data.macros :as dm]
   [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.util.i18n :as i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [rumext.v2 :as mf]))

(defn- get-tips
  []
  [{:title (tr "loader.tips.01.title")
    :message (tr "loader.tips.01.message")}
   {:title (tr "loader.tips.02.title")
    :message (tr "loader.tips.02.message")}
   {:title (tr "loader.tips.03.title")
    :message (tr "loader.tips.03.message")}
   {:title (tr "loader.tips.04.title")
    :message (tr "loader.tips.04.message")}
   {:title (tr "loader.tips.05.title")
    :message (tr "loader.tips.05.message")}
   {:title (tr "loader.tips.06.title")
    :message (tr "loader.tips.06.message")}
   {:title (tr "loader.tips.07.title")
    :message (tr "loader.tips.07.message")}
   {:title (tr "loader.tips.08.title")
    :message (tr "loader.tips.08.message")}
   {:title (tr "loader.tips.09.title")
    :message (tr "loader.tips.09.message")}
   {:title (tr "loader.tips.10.title")
    :message (tr "loader.tips.10.message")}])

;; Kizuku triangle mark: outer ring (animated draw) + inner facet
;; strokes meeting at the centre vertex (static ghost).
(def ^:private
  svg:loader-ring
  "M610 42L1165 1010L55 1010Z")

(def ^:private
  svg:loader-facets
  "M606.5 692.6L610 42M606.5 692.6L55 1010M606.5 692.6L1165 1010")

(mf/defc loader-icon*
  {::mf/private true}
  [{:keys [width height title] :rest props}]
  (let [class (stl/css :loader)
        props (mf/spread-props props {:viewBox "0 0 1220 1044"
                                      :role "status"
                                      :fill "none"
                                      :width width
                                      :height height
                                      :class class})]
    [:> :svg props
     [:title title]
     [:g
      [:path {:class (stl/css :loader-ghost)
              :d svg:loader-facets}]
      [:path {:class (stl/css :loader-ghost)
              :d svg:loader-ring}]
      [:path {:class (stl/css :loader-draw)
              :d svg:loader-ring
              :pathLength 1}]]]))

(def ^:private schema:loader
  [:map
   [:class {:optional true} :string]
   [:width {:optional true} :int]
   [:height {:optional true} :int]
   [:title {:optional true} :string]
   [:overlay {:optional true} :boolean]
   [:file-loading {:optional true} :boolean]])

(mf/defc loader*
  {::mf/schema schema:loader}
  [{:keys [class width height title overlay children file-loading] :rest props}]
  (let [width  (or width (when (some? height) (mth/ceil (* height (/ 1220 1044)))) 100)
        height (or height (when (some? width) (mth/ceil (* width (/ 1044 1220)))) 86)

        class  (dm/str (d/nilv class "") " "
                       (stl/css-case :wrapper true
                                     :wrapper-overlay overlay
                                     :file-loading file-loading))

        title  (or title (tr "labels.loading"))
        tips   (mf/use-memo get-tips)

        tip*   (mf/use-state nil)
        tip    (deref tip*)]

    (mf/with-effect [file-loading tips]
      (when file-loading
        (let [sub (->> (rx/timer 1000 4000)
                       (rx/subs! #(reset! tip* (rand-nth tips))))]
          (partial rx/dispose! sub))))

    [:> :div {:class class}
     [:div {:class (stl/css :loader-content)}
      [:> loader-icon* {:title title
                        :width width
                        :height height}]
      (when (and file-loading tip)
        [:div {:class (stl/css :tips-container)}
         [:div {:class (stl/css :tip-title)}
          (get tip :title)]
         [:div {:class (stl/css :tip-message)}
          (get tip :message)]])]

     children]))
