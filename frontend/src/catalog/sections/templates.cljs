;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Templates section — layout patterns (wireframe representations,
;; not filled with real content).

(ns catalog.sections.templates
  (:require
   [catalog.helpers.showcase :refer [showcase*]]
   [rumext.v2 :as mf]))

(def ^:private box-style
  {:border "1px dashed var(--catalog-border, #2a2a4a)"
   :border-radius "4px"
   :display "flex"
   :align-items "center"
   :justify-content "center"
   :font-size "11px"
   :color "var(--catalog-text-muted, #8892b0)"
   :background "rgba(124, 58, 237, 0.05)"})

;; ---------------------------------------------------------------------------
;; Workspace layout
;; ---------------------------------------------------------------------------

(mf/defc workspace-template*
  "Workspace wireframe: toolbar + canvas + side panels."
  []
  [:> showcase* {:title "Workspace Layout"
                 :description "Toolbar + left sidebar + canvas + right sidebar"}
   [:div {:style {:width "100%"
                  :max-width "800px"
                  :height "400px"
                  :border "1px solid var(--catalog-border, #2a2a4a)"
                  :border-radius "8px"
                  :overflow "hidden"
                  :display "flex"
                  :flex-direction "column"}}
    ;; Top bar
    [:div {:style (merge box-style {:height "40px"
                                    :border-radius "0"
                                    :border-bottom "1px solid var(--catalog-border)"})}
     "Top Toolbar"]
    [:div {:style {:display "flex" :flex "1" :overflow "hidden"}}
     ;; Left sidebar
     [:div {:style (merge box-style {:width "200px"
                                     :border-radius "0"
                                     :border-right "1px solid var(--catalog-border)"
                                     :flex-direction "column"
                                     :gap "4px"})}
      [:span "Layers"]
      [:span "Assets"]]
     ;; Canvas
     [:div {:style (merge box-style {:flex "1"
                                     :border-radius "0"
                                     :background "rgba(0,0,0,0.1)"})}
      "Canvas / Viewport"]
     ;; Right sidebar
     [:div {:style (merge box-style {:width "240px"
                                     :border-radius "0"
                                     :border-left "1px solid var(--catalog-border)"
                                     :flex-direction "column"
                                     :gap "4px"})}
      [:span "Design"]
      [:span "Properties"]]]]])

;; ---------------------------------------------------------------------------
;; Dashboard layout
;; ---------------------------------------------------------------------------

(mf/defc dashboard-template*
  "Dashboard wireframe: sidebar + content grid."
  []
  [:> showcase* {:title "Dashboard Layout"
                 :description "Sidebar navigation + project/file content grid"}
   [:div {:style {:width "100%"
                  :max-width "800px"
                  :height "350px"
                  :border "1px solid var(--catalog-border, #2a2a4a)"
                  :border-radius "8px"
                  :overflow "hidden"
                  :display "flex"}}
    ;; Sidebar
    [:div {:style (merge box-style {:width "200px"
                                    :border-radius "0"
                                    :border-right "1px solid var(--catalog-border)"
                                    :flex-direction "column"
                                    :gap "8px"
                                    :padding "16px"})}
     [:span "Projects"]
     [:span "Drafts"]
     [:span "Libraries"]
     [:span "Fonts"]]
    ;; Content grid
    [:div {:style {:flex "1"
                   :padding "16px"
                   :display "grid"
                   :grid-template-columns "repeat(3, 1fr)"
                   :grid-template-rows "repeat(2, 1fr)"
                   :gap "12px"}}
     (for [idx (range 6)]
       [:div {:key idx
              :style (merge box-style {:border-radius "6px"})}
        (str "File " (inc idx))])]]])

;; ---------------------------------------------------------------------------
;; Settings layout
;; ---------------------------------------------------------------------------

(mf/defc settings-template*
  "Settings wireframe: nav + form content."
  []
  [:> showcase* {:title "Settings Layout"
                 :description "Navigation tabs + settings form area"}
   [:div {:style {:width "100%"
                  :max-width "600px"
                  :height "250px"
                  :border "1px solid var(--catalog-border, #2a2a4a)"
                  :border-radius "8px"
                  :overflow "hidden"
                  :display "flex"
                  :flex-direction "column"}}
    ;; Tab nav
    [:div {:style (merge box-style {:height "40px"
                                    :border-radius "0"
                                    :border-bottom "1px solid var(--catalog-border)"
                                    :gap "16px"})}
     [:span "Profile"]
     [:span "Password"]
     [:span "Preferences"]]
    ;; Form content
    [:div {:style (merge box-style {:flex "1"
                                    :border-radius "0"
                                    :flex-direction "column"
                                    :gap "8px"
                                    :padding "24px"})}
     [:div {:style (merge box-style {:height "32px" :width "80%"})} "Input field"]
     [:div {:style (merge box-style {:height "32px" :width "80%"})} "Input field"]
     [:div {:style (merge box-style {:height "32px" :width "120px"})} "Save button"]]]])

;; ---------------------------------------------------------------------------
;; Empty state
;; ---------------------------------------------------------------------------

(mf/defc empty-state-template*
  "Empty state wireframe."
  []
  [:> showcase* {:title "Empty State"
                 :description "Centered illustration + message + CTA pattern"}
   [:div {:style {:width "100%"
                  :max-width "500px"
                  :height "250px"
                  :border "1px solid var(--catalog-border, #2a2a4a)"
                  :border-radius "8px"
                  :display "flex"
                  :flex-direction "column"
                  :align-items "center"
                  :justify-content "center"
                  :gap "12px"}}
    [:div {:style (merge box-style {:width "80px"
                                    :height "80px"
                                    :border-radius "50%"})}
     "Illustration"]
    [:div {:style {:font-size "14px"
                   :color "var(--catalog-text, #e0e0e0)"}}
     "Nothing here yet"]
    [:div {:style {:font-size "12px"
                   :color "var(--catalog-text-muted, #8892b0)"}}
     "Get started by creating your first item"]
    [:div {:style (merge box-style {:padding "8px 24px"
                                    :border-radius "6px"})}
     "Create"]]])

;; ---------------------------------------------------------------------------
;; Main section
;; ---------------------------------------------------------------------------

(mf/defc templates-section*
  "Top-level templates page."
  []
  [:div
   [:h2 {:style {:margin "0 0 24px"
                 :font-size "24px"
                 :font-weight "700"
                 :color "var(--catalog-text, #e0e0e0)"}}
    "Templates"]
   [:p {:style {:margin "0 0 24px"
                :font-size "14px"
                :color "var(--catalog-text-muted, #8892b0)"}}
    "Layout patterns shown as wireframes — not filled with real content."]
   [:> workspace-template*]
   [:> dashboard-template*]
   [:> settings-template*]
   [:> empty-state-template*]])
