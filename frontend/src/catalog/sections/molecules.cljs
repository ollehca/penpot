;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Molecules section — combinations of atoms forming distinct UI units.

(ns catalog.sections.molecules
  (:require
   [app.main.ui.ds.layout.tab-switcher :refer [tab-switcher*]]
   [app.main.ui.ds.notifications.toast :refer [toast*]]
   [app.main.ui.ds.notifications.context-notification
    :refer [context-notification*]]
   [app.main.ui.ds.product.avatar :refer [avatar*]]
   [app.main.ui.ds.product.cta :refer [cta*]]
   [catalog.helpers.showcase :refer [showcase* variant-group*]]
   [rumext.v2 :as mf]))

;; ---------------------------------------------------------------------------
;; Tab Switcher
;; ---------------------------------------------------------------------------

(mf/defc tabs-showcase*
  "Tab navigation component."
  []
  (let [selected* (mf/use-state "tab-1")]
    [:> showcase* {:title "Tab Switcher"
                   :description "Tabbed navigation with icon + label support"}
     [:div {:style {:width "100%" :max-width "500px"}}
      [:> tab-switcher*
       {:tabs [{:id "tab-1" :label "Design"}
               {:id "tab-2" :label "Inspect"}
               {:id "tab-3" :label "Code"}]
        :selected (deref selected*)
        :on-change #(reset! selected* %)}
       [:div {:style {:padding "16px"
                      :color "var(--catalog-text-muted)"}}
        (str "Content for: " (deref selected*))]]]]))

;; ---------------------------------------------------------------------------
;; Toast Notifications
;; ---------------------------------------------------------------------------

(mf/defc toasts-showcase*
  "Toast notification variants."
  []
  [:> showcase* {:title "Toast"
                 :description "Transient notifications — levels: default, info, warning, error, success"}
   [:div {:style {:display "flex"
                  :flex-direction "column"
                  :gap "12px"
                  :width "100%"}}
    [:> toast* {:level :default
                :on-close #(js/console.log "close")}
     "Default notification message"]
    [:> toast* {:level :info
                :on-close #(js/console.log "close")}
     "Info notification message"]
    [:> toast* {:level :warning
                :on-close #(js/console.log "close")}
     "Warning notification message"]
    [:> toast* {:level :error
                :on-close #(js/console.log "close")}
     "Error notification message"]
    [:> toast* {:level :success
                :on-close #(js/console.log "close")}
     "Success notification message"]]])

;; ---------------------------------------------------------------------------
;; Context Notifications
;; ---------------------------------------------------------------------------

(mf/defc context-notifications-showcase*
  "Persistent contextual notification variants."
  []
  [:> showcase* {:title "Context Notification"
                 :description "Persistent area notifications — levels: default, info, warning, error, success"}
   [:div {:style {:display "flex"
                  :flex-direction "column"
                  :gap "12px"
                  :width "100%"}}
    [:> context-notification* {:level :default}
     "Default context message"]
    [:> context-notification* {:level :info}
     "Info context message"]
    [:> context-notification* {:level :warning}
     "Warning context message"]
    [:> context-notification* {:level :error}
     "Error context message"]
    [:> context-notification* {:level :success}
     "Success context message"]]])

;; ---------------------------------------------------------------------------
;; Avatar
;; ---------------------------------------------------------------------------

(mf/defc avatar-showcase*
  "Avatar component at all sizes."
  []
  [:> showcase* {:title "Avatar"
                 :description "User avatar — sizes: S, M, L; generated from name when no URL"}
   [:> variant-group* {:label "Sizes"}
    [:> avatar* {:name "Alice Smith" :variant "S"}]
    [:> avatar* {:name "Bob Jones" :variant "M"}]
    [:> avatar* {:name "Carol Chen" :variant "L"}]]
   [:> variant-group* {:label "Selected state"}
    [:> avatar* {:name "Alice Smith" :variant "M" :selected true}]]
   [:> variant-group* {:label "With colour"}
    [:> avatar* {:name "Red User" :variant "M" :color "#e74c3c"}]
    [:> avatar* {:name "Blue User" :variant "M" :color "#3498db"}]
    [:> avatar* {:name "Green User" :variant "M" :color "#2ecc71"}]]])

;; ---------------------------------------------------------------------------
;; CTA (Call to Action)
;; ---------------------------------------------------------------------------

(mf/defc cta-showcase*
  "Call-to-action banner."
  []
  [:> showcase* {:title "CTA (Call-to-Action)"
                 :description "Prominent action banner with title and children content"}
   [:div {:style {:width "100%" :max-width "500px"}}
    [:> cta* {:title "Upgrade to Pro"}
     [:span "Get unlimited projects and collaboration features."]]]])

;; ---------------------------------------------------------------------------
;; Input with Meta
;; ---------------------------------------------------------------------------

(mf/defc input-meta-showcase*
  "Input with metadata/prefix/suffix."
  []
  [:> showcase* {:title "Input with Meta"
                 :description "Input field with additional metadata display"}
   [:div {:style {:width "300px"}}
    "Requires app context — shown as reference only."]])

;; ---------------------------------------------------------------------------
;; Main section
;; ---------------------------------------------------------------------------

(mf/defc molecules-section*
  "Top-level molecules page."
  []
  [:div
   [:h2 {:style {:margin "0 0 24px"
                 :font-size "24px"
                 :font-weight "700"
                 :color "var(--catalog-text, #e0e0e0)"}}
    "Molecules"]
   [:> tabs-showcase*]
   [:> toasts-showcase*]
   [:> context-notifications-showcase*]
   [:> avatar-showcase*]
   [:> cta-showcase*]
   [:> input-meta-showcase*]])
