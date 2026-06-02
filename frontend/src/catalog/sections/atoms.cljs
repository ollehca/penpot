;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Atoms section — smallest indivisible UI elements from the DS.
;; Imports real components from app.main.ui.ds.

(ns catalog.sections.atoms
  (:require
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.input :refer [input*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.main.ui.ds.foundations.assets.icon :as icon]
   [app.main.ui.ds.foundations.typography.heading :refer [heading*]]
   [app.main.ui.ds.foundations.typography.text :refer [text*]]
   [app.main.ui.ds.foundations.typography :as typo]
   [app.main.ui.ds.tooltip.tooltip :refer [tooltip*]]
   [catalog.helpers.showcase :refer [showcase* variant-group*]]
   [rumext.v2 :as mf]))

;; ---------------------------------------------------------------------------
;; Buttons
;; ---------------------------------------------------------------------------

(mf/defc buttons-showcase*
  "All button variants and states."
  []
  [:> showcase* {:title "Button"
                 :description "Primary action trigger — variants: primary, secondary, ghost, destructive"}
   [:> variant-group* {:label "Primary"}
    [:> button* {:variant "primary"} "Primary"]
    [:> button* {:variant "primary" :icon "add"} "With Icon"]
    [:> button* {:variant "primary" :disabled true} "Disabled"]]
   [:> variant-group* {:label "Secondary"}
    [:> button* {:variant "secondary"} "Secondary"]
    [:> button* {:variant "secondary" :disabled true} "Disabled"]]
   [:> variant-group* {:label "Ghost"}
    [:> button* {:variant "ghost"} "Ghost"]
    [:> button* {:variant "ghost" :disabled true} "Disabled"]]
   [:> variant-group* {:label "Destructive"}
    [:> button* {:variant "destructive"} "Destructive"]
    [:> button* {:variant "destructive" :disabled true} "Disabled"]]])

;; ---------------------------------------------------------------------------
;; Icon Buttons
;; ---------------------------------------------------------------------------

(mf/defc icon-buttons-showcase*
  "Icon-only buttons with tooltip."
  []
  [:> showcase* {:title "Icon Button"
                 :description "Icon-only buttons — variants: primary, secondary, ghost, action, destructive"}
   [:> variant-group* {:label "Variants"}
    [:> icon-button* {:icon "add" :aria-label "Add" :variant "primary"}]
    [:> icon-button* {:icon "delete" :aria-label "Delete" :variant "secondary"}]
    [:> icon-button* {:icon "settings" :aria-label "Settings" :variant "ghost"}]
    [:> icon-button* {:icon "play" :aria-label "Play" :variant "action"}]
    [:> icon-button* {:icon "close" :aria-label "Close" :variant "destructive"}]]
   [:> variant-group* {:label "Disabled"}
    [:> icon-button* {:icon "add" :aria-label "Add" :variant "primary" :disabled true}]]])

;; ---------------------------------------------------------------------------
;; Text Inputs
;; ---------------------------------------------------------------------------

(mf/defc inputs-showcase*
  "Text input variants with labels, hints, and states."
  []
  [:> showcase* {:title "Input"
                 :description "Text input — variants: dense, comfortable, seamless"}
   [:div {:style {:display "flex"
                  :flex-direction "column"
                  :gap "16px"
                  :width "100%"
                  :max-width "400px"}}
    [:> input* {:label "Dense (default)"
                :placeholder "Type here..."
                :variant "dense"}]
    [:> input* {:label "Comfortable"
                :placeholder "Type here..."
                :variant "comfortable"}]
    [:> input* {:label "With hint"
                :placeholder "Email address"
                :hint-message "We'll never share your email"
                :hint-type "hint"}]
    [:> input* {:label "Error state"
                :placeholder "Required field"
                :hint-message "This field is required"
                :hint-type "error"}]
    [:> input* {:label "Warning state"
                :placeholder "Check this"
                :hint-message "Value seems unusual"
                :hint-type "warning"}]
    [:> input* {:label "Optional field"
                :placeholder "Nickname"
                :is-optional true}]
    [:> input* {:label "With icon"
                :placeholder "Search..."
                :icon "search"}]
    [:> input* {:label "Disabled"
                :placeholder "Cannot edit"
                :disabled true}]]])

;; ---------------------------------------------------------------------------
;; Select
;; ---------------------------------------------------------------------------

(mf/defc select-showcase*
  "Select dropdown."
  []
  [:> showcase* {:title "Select"
                 :description "Dropdown select control"}
   [:div {:style {:width "300px"}}
    [:> select* {:default-selected "opt-1"
                 :options [{:id "opt-1" :label "Option 1"}
                           {:id "opt-2" :label "Option 2"}
                           {:id "opt-3" :label "Option 3"}]}]]])

;; ---------------------------------------------------------------------------
;; Typography Components
;; ---------------------------------------------------------------------------

(mf/defc typography-showcase*
  "Heading and Text components."
  []
  [:> showcase* {:title "Heading & Text"
                 :description "Typography components wrapping the token scale"}
   [:div {:style {:display "flex"
                  :flex-direction "column"
                  :gap "12px"
                  :width "100%"}}
    [:> heading* {:typography typo/title-large} "Title Large Heading"]
    [:> heading* {:typography typo/title-medium} "Title Medium Heading"]
    [:> heading* {:typography typo/title-small} "Title Small Heading"]
    [:> text* {:typography typo/body-large :as "p"} "Body Large text paragraph"]
    [:> text* {:typography typo/body-medium :as "p"} "Body Medium text paragraph"]
    [:> text* {:typography typo/body-small :as "p"} "Body Small text paragraph"]
    [:> text* {:typography typo/code-font :as "code"} "Code font sample"]]])

;; ---------------------------------------------------------------------------
;; Tooltip
;; ---------------------------------------------------------------------------

(mf/defc tooltip-showcase*
  "Tooltip component."
  []
  [:> showcase* {:title "Tooltip"
                 :description "Tooltip overlay — hover the buttons to see"}
   [:> variant-group* {:label "Positions"}
    [:> tooltip* {:content "Bottom tooltip"}
     [:button {:style {:padding "8px 16px"
                       :border "1px solid var(--catalog-border)"
                       :background "transparent"
                       :color "var(--catalog-text)"
                       :border-radius "4px"
                       :cursor "pointer"}} "Bottom"]]
    [:> tooltip* {:content "Top tooltip" :placement "top"}
     [:button {:style {:padding "8px 16px"
                       :border "1px solid var(--catalog-border)"
                       :background "transparent"
                       :color "var(--catalog-text)"
                       :border-radius "4px"
                       :cursor "pointer"}} "Top"]]]])

;; ---------------------------------------------------------------------------
;; Icons (DS component)
;; ---------------------------------------------------------------------------

(mf/defc icon-showcase*
  "Icon component from the DS at both sizes."
  []
  [:> showcase* {:title "Icon (DS Component)"
                 :description "SVG icon component — sizes: m (16px), s (12px)"}
   [:> variant-group* {:label "Size M"}
    [:> icon/icon* {:icon-id "add"}]
    [:> icon/icon* {:icon-id "search"}]
    [:> icon/icon* {:icon-id "close"}]
    [:> icon/icon* {:icon-id "settings"}]
    [:> icon/icon* {:icon-id "delete"}]]
   [:> variant-group* {:label "Size S"}
    [:> icon/icon* {:icon-id "add" :size "s"}]
    [:> icon/icon* {:icon-id "search" :size "s"}]
    [:> icon/icon* {:icon-id "close" :size "s"}]]])

;; ---------------------------------------------------------------------------
;; Main section
;; ---------------------------------------------------------------------------

(mf/defc atoms-section*
  "Top-level atoms page."
  []
  [:div
   [:h2 {:style {:margin "0 0 24px"
                 :font-size "24px"
                 :font-weight "700"
                 :color "var(--catalog-text, #e0e0e0)"}}
    "Atoms"]
   [:> buttons-showcase*]
   [:> icon-buttons-showcase*]
   [:> inputs-showcase*]
   [:> select-showcase*]
   [:> typography-showcase*]
   [:> tooltip-showcase*]
   [:> icon-showcase*]])
