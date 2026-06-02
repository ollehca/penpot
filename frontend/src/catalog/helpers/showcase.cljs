;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Reusable showcase wrapper that renders a component with its name,
;; description, and variants inside a consistent card layout.

(ns catalog.helpers.showcase
  (:require
   [rumext.v2 :as mf]))

(mf/defc showcase*
  "Renders a single catalog entry: heading, description, and children
   (the component variants) inside a bordered card."
  [{:keys [title description children]}]
  [:section {:style {:border "1px solid var(--catalog-border, #2a2a4a)"
                     :border-radius "8px"
                     :padding "24px"
                     :margin-bottom "24px"
                     :background "var(--catalog-card-bg, #1e1e3a)"}}
   [:h3 {:style {:margin "0 0 4px"
                 :font-size "16px"
                 :font-weight "600"
                 :color "var(--catalog-text, #e0e0e0)"}}
    title]
   (when description
     [:p {:style {:margin "0 0 16px"
                  :font-size "13px"
                  :color "var(--catalog-text-muted, #8892b0)"}}
      description])
   [:div {:style {:display "flex"
                  :flex-wrap "wrap"
                  :gap "12px"
                  :align-items "flex-start"}}
    children]])

(mf/defc variant-group*
  "Renders a labelled sub-group inside a showcase card."
  [{:keys [label children]}]
  [:div {:style {:display "flex"
                 :flex-direction "column"
                 :gap "8px"}}
   (when label
     [:span {:style {:font-size "11px"
                     :text-transform "uppercase"
                     :letter-spacing "0.05em"
                     :color "var(--catalog-text-muted, #8892b0)"}}
      label])
   [:div {:style {:display "flex"
                  :flex-wrap "wrap"
                  :gap "8px"
                  :align-items "center"}}
    children]])
