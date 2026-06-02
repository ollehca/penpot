;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Organisms section — complex, self-contained UI sections.
;; Many organisms require substantial app state. Those are listed
;; as reference entries with notes about their dependencies.

(ns catalog.sections.organisms
  (:require
   [app.main.ui.ds.product.loader :refer [loader*]]
   [app.main.ui.ds.product.empty-placeholder :refer [empty-placeholder*]]
   [catalog.helpers.showcase :refer [showcase* variant-group*]]
   [rumext.v2 :as mf]))

;; ---------------------------------------------------------------------------
;; Loader
;; ---------------------------------------------------------------------------

(mf/defc loader-showcase*
  "Loader component."
  []
  [:> showcase* {:title "Loader"
                 :description "Loading indicator — shown during file operations and page loads"}
   [:> variant-group* {:label "Default"}
    [:div {:style {:position "relative" :width "200px" :height "80px"}}
     [:> loader* {:width 200}]]]
   [:> variant-group* {:label "Small"}
    [:div {:style {:position "relative" :width "100px" :height "40px"}}
     [:> loader* {:width 100}]]]])

;; ---------------------------------------------------------------------------
;; Empty Placeholder
;; ---------------------------------------------------------------------------

(mf/defc placeholder-showcase*
  "Empty state placeholder."
  []
  [:> showcase* {:title "Empty Placeholder"
                 :description "Shown when a section has no content — types 1 and 2"}
   [:div {:style {:display "flex"
                  :flex-direction "column"
                  :gap "24px"
                  :width "100%"}}
    [:> empty-placeholder* {:title "No projects yet"
                            :subtitle "Create your first project to get started"
                            :type 1}]
    [:> empty-placeholder* {:title "Nothing here"
                            :subtitle "This section is empty"
                            :type 2}]]])

;; ---------------------------------------------------------------------------
;; Reference entries for state-dependent organisms
;; ---------------------------------------------------------------------------

(def ^:private organism-refs
  [{:name "Workspace Sidebar"
    :file "app.main.ui.workspace.sidebar"
    :note "Requires full workspace state (layers, assets, options tabs)"}
   {:name "Top Toolbar"
    :file "app.main.ui.workspace.top-toolbar"
    :note "Requires workspace tool state and event bus"}
   {:name "Layer Panel"
    :file "app.main.ui.workspace.sidebar.layers"
    :note "Requires layer tree data from workspace state"}
   {:name "Assets Panel"
    :file "app.main.ui.workspace.sidebar.assets"
    :note "Requires library/file data from workspace state"}
   {:name "Options / Properties Panel"
    :file "app.main.ui.workspace.sidebar.options"
    :note "Requires selected object state"}
   {:name "Color Picker"
    :file "app.main.ui.workspace.colorpicker"
    :note "Requires color state, gradient data, HSVA canvas"}
   {:name "Context Menu"
    :file "app.main.ui.workspace.context-menu"
    :note "Requires right-click position and selected object type"}
   {:name "Main Menu"
    :file "app.main.ui.workspace.main-menu"
    :note "Requires route + permissions state"}
   {:name "Dashboard Projects"
    :file "app.main.ui.dashboard.projects"
    :note "Requires project/file listing from backend"}
   {:name "Dashboard Sidebar"
    :file "app.main.ui.dashboard.sidebar"
    :note "Requires team/project navigation state"}
   {:name "Dashboard File Grid"
    :file "app.main.ui.dashboard.grid"
    :note "Requires file thumbnails and metadata"}
   {:name "Login Form"
    :file "app.main.ui.auth.login"
    :note "Requires auth state and route handling"}
   {:name "Viewer"
    :file "app.main.ui.viewer"
    :note "Requires file data, frame list, prototype state"}])

(mf/defc organism-ref-card*
  "Reference card for an organism that cannot render in isolation."
  [{:keys [name file note]}]
  [:div {:style {:border "1px solid var(--catalog-border, #2a2a4a)"
                 :border-radius "8px"
                 :padding "16px"
                 :background "var(--catalog-card-bg, #1e1e3a)"}}
   [:div {:style {:font-size "14px"
                  :font-weight "600"
                  :color "var(--catalog-text, #e0e0e0)"
                  :margin-bottom "4px"}}
    name]
   [:div {:style {:font-size "12px"
                  :font-family "monospace"
                  :color "var(--catalog-accent, #7c3aed)"
                  :margin-bottom "8px"}}
    file]
   [:div {:style {:font-size "12px"
                  :color "var(--catalog-text-muted, #8892b0)"
                  :font-style "italic"}}
    (str "Requires app context — " note)]])

(mf/defc organisms-reference*
  "Reference listing of state-dependent organisms."
  []
  [:> showcase* {:title "Complex Organisms (reference)"
                 :description "These components require full app state — shown with mock data notes"}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(auto-fill, minmax(320px, 1fr))"
                  :gap "12px"
                  :width "100%"}}
    (for [{:keys [name] :as org} organism-refs]
      [:> organism-ref-card* {:key name
                              :name name
                              :file (:file org)
                              :note (:note org)}])]])

;; ---------------------------------------------------------------------------
;; Main section
;; ---------------------------------------------------------------------------

(mf/defc organisms-section*
  "Top-level organisms page."
  []
  [:div
   [:h2 {:style {:margin "0 0 24px"
                 :font-size "24px"
                 :font-weight "700"
                 :color "var(--catalog-text, #e0e0e0)"}}
    "Organisms"]
   [:> loader-showcase*]
   [:> placeholder-showcase*]
   [:> organisms-reference*]])
