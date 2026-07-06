;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Catalog layout — sidebar navigation + main content area.
;; The sidebar lists all sections; clicking navigates via local state.

(ns catalog.layout
  (:require
   [catalog.sections.tokens :as tokens]
   [catalog.sections.atoms :as atoms]
   [catalog.sections.molecules :as molecules]
   [catalog.sections.organisms :as organisms]
   [catalog.sections.templates :as templates]
   [rumext.v2 :as mf]))

(def ^:private nav-items
  [{:id "tokens"     :label "Design Tokens"}
   {:id "atoms"      :label "Atoms"}
   {:id "molecules"  :label "Molecules"}
   {:id "organisms"  :label "Organisms"}
   {:id "templates"  :label "Templates"}])

(mf/defc sidebar*
  "Sidebar navigation listing all Atomic Design sections."
  [{:keys [selected on-select]}]
  [:nav {:style {:width "240px"
                 :min-width "240px"
                 :background "var(--catalog-sidebar-bg, #16213e)"
                 :border-right "1px solid var(--catalog-border, #2a2a4a)"
                 :padding "24px 0"
                 :overflow-y "auto"
                 :height "100vh"}}
   [:div {:style {:padding "0 20px 20px"
                  :font-size "18px"
                  :font-weight "700"
                  :letter-spacing "-0.02em"
                  :color "var(--catalog-accent, #7c3aed)"}}
    "Kata 型"]
   [:ul {:style {:list-style "none"
                 :margin "0"
                 :padding "0"}}
    (for [{:keys [id label]} nav-items]
      [:li {:key id}
       [:button {:style {:display "block"
                         :width "100%"
                         :padding "10px 20px"
                         :border "none"
                         :background (if (= id selected)
                                       "rgba(124, 58, 237, 0.15)"
                                       "transparent")
                         :color (if (= id selected)
                                  "var(--catalog-accent, #7c3aed)"
                                  "var(--catalog-text-muted, #8892b0)")
                         :font-size "14px"
                         :text-align "left"
                         :cursor "pointer"
                         :border-left (if (= id selected)
                                        "3px solid var(--catalog-accent, #7c3aed)"
                                        "3px solid transparent")}
                 :on-click #(on-select id)}
        label]])]])

(mf/defc content-panel*
  "Renders the selected section content."
  [{:keys [selected]}]
  [:main {:style {:flex "1"
                  :overflow-y "auto"
                  :padding "32px 40px"
                  :height "100vh"}}
   (case selected
     "tokens"    [:> tokens/tokens-section*]
     "atoms"     [:> atoms/atoms-section*]
     "molecules" [:> molecules/molecules-section*]
     "organisms" [:> organisms/organisms-section*]
     "templates" [:> templates/templates-section*]
     [:> tokens/tokens-section*])])

(mf/defc catalog-app
  "Root component — sidebar + content area."
  []
  (let [selected* (mf/use-state "tokens")
        selected  (deref selected*)]
    [:div {:style {:display "flex"
                   :height "100vh"
                   :background "var(--catalog-bg, #1a1a2e)"}}
     [:> sidebar* {:selected selected
                   :on-select #(reset! selected* %)}]
     [:> content-panel* {:selected selected}]]))
