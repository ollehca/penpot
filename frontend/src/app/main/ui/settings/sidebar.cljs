;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

;; ============================================================================
;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; Original file from PenPot (https://github.com/penpot/penpot)
;; Licensed under Mozilla Public License Version 2.0
;; Modifications: Beta hide pass — removed "Password" (auth is license-based),
;;   "Notifications" (no email in a local app) and "Release notes" nav
;;   entries; routes for the hidden pages still exist so deep links work.
;; Date: 2026-07-08
;; ============================================================================

(ns app.main.ui.settings.sidebar
  (:require-macros [app.main.style :as stl])
  (:require
   [app.config :as cf]
   [app.main.data.common :as dcm]
   [app.main.data.team :as dtm]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.dashboard.sidebar :refer [profile-section*]]
   [app.main.ui.icons :as i]
   [app.util.i18n :as i18n :refer [tr]]
   [rumext.v2 :as mf]))

(def ^:private arrow-icon
  (i/icon-xref :arrow (stl/css :arrow-icon)))

(def ^:private feedback-icon
  (i/icon-xref :feedback (stl/css :feedback-icon)))

;; FIXME: move to common
(def ^:private go-settings-profile
  #(st/emit! (rt/nav :settings-profile)))

(def ^:private go-settings-feedback
  #(st/emit! (rt/nav :settings-feedback)))

(def ^:private go-settings-options
  #(st/emit! (rt/nav :settings-options)))

(def ^:private go-settings-subscription
  #(st/emit! (rt/nav :settings-subscription)))

(def ^:private go-settings-access-tokens
  #(st/emit! (rt/nav :settings-access-tokens)))

(mf/defc sidebar-content
  {::mf/props :obj}
  [{:keys [profile section]}]
  (let [profile?       (= section :settings-profile)
        options?       (= section :settings-options)
        feedback?      (= section :settings-feedback)
        subscription?  (= section :settings-subscription)
        access-tokens? (= section :settings-access-tokens)
        team-id        (or (dtm/get-last-team-id)
                           (:default-team-id profile))

        go-dashboard
        (mf/use-fn
         (mf/deps team-id)
         #(st/emit! (dcm/go-to-dashboard-recent :team-id team-id)))]

    [:div {:class (stl/css :sidebar-content)}
     [:div {:class (stl/css :sidebar-content-section)}
      [:button {:class (stl/css :back-to-dashboard)
                :on-click go-dashboard}
       arrow-icon
       [:span {:class (stl/css :back-text)} (tr "labels.dashboard")]]]

     [:hr {:class (stl/css :sidebar-separator)}]

     [:div {:class (stl/css :sidebar-content-section)}
      [:ul {:class (stl/css :sidebar-nav-settings)}
       [:li {:class (stl/css-case :current profile?
                                  :settings-item true)
             :on-click go-settings-profile}
        [:span {:class (stl/css :element-title)} (tr "labels.profile")]]

       [:li {:class (stl/css-case :current options?
                                  :settings-item true)
             :on-click go-settings-options
             :data-testid "settings-profile"}
        [:span {:class (stl/css :element-title)} (tr "labels.settings")]]

       (when (contains? cf/flags :subscriptions)
         [:li {:class (stl/css-case :current subscription?
                                    :settings-item true)
               :on-click go-settings-subscription
               :data-testid "settings-subscription"}
          [:span {:class (stl/css :element-title)} (tr "subscription.labels")]])

       (when (contains? cf/flags :access-tokens)
         [:li {:class (stl/css-case :current access-tokens?
                                    :settings-item true)
               :on-click go-settings-access-tokens
               :data-testid "settings-access-tokens"}
          [:span {:class (stl/css :element-title)} (tr "labels.access-tokens")]])

       (when (contains? cf/flags :user-feedback)
         [:*
          [:hr {:class (stl/css :sidebar-separator)}]
          [:li {:class (stl/css-case :current feedback?
                                     :settings-item true)
                :on-click go-settings-feedback}
           feedback-icon
           [:span {:class (stl/css :element-title)} (tr "labels.give-feedback")]]])]]]))

(mf/defc sidebar
  {::mf/wrap [mf/memo]
   ::mf/props :obj}
  [{:keys [profile section]}]
  [:div {:class (stl/css :dashboard-sidebar :settings)}
   [:& sidebar-content {:profile profile
                        :section section}]
   [:> profile-section* {:profile profile}]])

