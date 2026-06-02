;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Design Tokens section — colours, typography, spacing, shadows,
;; border radii, and the icon grid.

(ns catalog.sections.tokens
  (:require
   [catalog.helpers.showcase :refer [showcase*]]
   [rumext.v2 :as mf]))

;; ---------------------------------------------------------------------------
;; Colour tokens
;; ---------------------------------------------------------------------------

(def ^:private color-groups
  [{:label "Status"
    :colors [{:name "--status-color-success-200" :hex "#a7e8d9"}
             {:name "--status-color-success-500" :hex "#2d9f8f"}
             {:name "--status-color-warning-500" :hex "#f5a91b"}
             {:name "--status-color-error-500"   :hex "#ff3277"}
             {:name "--status-color-info-500"    :hex "#0e9be9"}]}
   {:label "App"
    :colors [{:name "--app-white" :hex "#ffffff"}
             {:name "--app-black" :hex "#000000"}]}])

(mf/defc color-swatch*
  "Single colour swatch with label and hex."
  [{:keys [name hex]}]
  [:div {:style {:display "flex"
                 :flex-direction "column"
                 :align-items "center"
                 :gap "6px"
                 :width "120px"}}
   [:div {:style {:width "64px"
                  :height "64px"
                  :border-radius "8px"
                  :background hex
                  :border "1px solid var(--catalog-swatch-border, #3a3a5a)"}}]
   [:span {:style {:font-size "11px"
                   :color "var(--catalog-text-muted, #8892b0)"
                   :text-align "center"
                   :word-break "break-all"}}
    name]
   [:span {:style {:font-size "11px"
                   :font-family "monospace"
                   :color "var(--catalog-text, #e0e0e0)"}}
    hex]])

(mf/defc colors-section*
  "All colour tokens grouped by role."
  []
  [:div
   (for [{:keys [label colors]} color-groups]
     [:> showcase* {:key label
                    :title (str "Colours — " label)
                    :description (str (count colors) " tokens")}
      (for [{:keys [name hex]} colors]
        [:> color-swatch* {:key name :name name :hex hex}])])])

;; ---------------------------------------------------------------------------
;; Typography tokens
;; ---------------------------------------------------------------------------

(def ^:private typography-samples
  [{:id "display"         :label "Display"}
   {:id "title-large"     :label "Title Large"}
   {:id "title-medium"    :label "Title Medium"}
   {:id "title-small"     :label "Title Small"}
   {:id "headline-large"  :label "Headline Large"}
   {:id "headline-medium" :label "Headline Medium"}
   {:id "headline-small"  :label "Headline Small"}
   {:id "body-large"      :label "Body Large"}
   {:id "body-medium"     :label "Body Medium"}
   {:id "body-small"      :label "Body Small"}
   {:id "code-font"       :label "Code Font"}])

(mf/defc typography-section*
  "Every typography token rendered at actual size."
  []
  [:> showcase* {:title "Typography Scale"
                 :description "All typography tokens from the design system"}
   [:div {:style {:display "flex"
                  :flex-direction "column"
                  :gap "16px"
                  :width "100%"}}
    (for [{:keys [id label]} typography-samples]
      [:div {:key id
             :style {:display "flex"
                     :align-items "baseline"
                     :gap "16px"
                     :padding "8px 0"
                     :border-bottom "1px solid var(--catalog-border, #2a2a4a)"}}
       [:span {:style {:min-width "160px"
                       :font-size "12px"
                       :font-family "monospace"
                       :color "var(--catalog-text-muted, #8892b0)"}}
        id]
       [:span {:class id}
        (str label " — The quick brown fox")]])]])

;; ---------------------------------------------------------------------------
;; Spacing scale
;; ---------------------------------------------------------------------------

(def ^:private spacing-steps
  [0 1 2 4 6 8 12 16 20 24 28 32 40 48 64 80 96 128])

(mf/defc spacing-section*
  "Spacing scale visualised as labelled bars."
  []
  [:> showcase* {:title "Spacing Scale"
                 :description "4px base unit, shown as pixel values"}
   [:div {:style {:display "flex"
                  :flex-direction "column"
                  :gap "6px"
                  :width "100%"}}
    (for [step spacing-steps]
      [:div {:key step
             :style {:display "flex"
                     :align-items "center"
                     :gap "12px"}}
       [:span {:style {:min-width "60px"
                       :font-size "12px"
                       :font-family "monospace"
                       :text-align "right"
                       :color "var(--catalog-text-muted, #8892b0)"}}
        (str "$s-" step)]
       [:div {:style {:height "16px"
                      :width (str (max step 2) "px")
                      :background "var(--catalog-accent, #7c3aed)"
                      :border-radius "2px"}}]
       [:span {:style {:font-size "11px"
                       :color "var(--catalog-text, #e0e0e0)"}}
        (str step "px")]])]])

;; ---------------------------------------------------------------------------
;; Border radii
;; ---------------------------------------------------------------------------

(def ^:private radii
  [{:token "$br-0"      :val "0px"}
   {:token "$br-2"      :val "2px"}
   {:token "$br-4"      :val "4px"}
   {:token "$br-5"      :val "5px"}
   {:token "$br-6"      :val "6px"}
   {:token "$br-8"      :val "8px"}
   {:token "$br-10"     :val "10px"}
   {:token "$br-12"     :val "12px"}
   {:token "$br-circle" :val "50%"}])

(mf/defc radii-section*
  "Border radius tokens shown on sample shapes."
  []
  [:> showcase* {:title "Border Radii"
                 :description "All radius tokens applied to 48px squares"}
   (for [{:keys [token val]} radii]
     [:div {:key token
            :style {:display "flex"
                    :flex-direction "column"
                    :align-items "center"
                    :gap "6px"}}
      [:div {:style {:width "48px"
                     :height "48px"
                     :background "var(--catalog-accent, #7c3aed)"
                     :border-radius val}}]
      [:span {:style {:font-size "11px"
                      :font-family "monospace"
                      :color "var(--catalog-text-muted, #8892b0)"}}
       token]
      [:span {:style {:font-size "11px"
                      :color "var(--catalog-text, #e0e0e0)"}}
       val]])])

;; ---------------------------------------------------------------------------
;; Shadows
;; ---------------------------------------------------------------------------

(mf/defc shadows-section*
  "Shadow tokens shown on sample cards."
  []
  [:> showcase* {:title "Elevation / Shadows"
                 :description "menuShadow and alertShadow mixins"}
   [:div {:style {:display "flex" :gap "32px"}}
    [:div {:style {:display "flex"
                   :flex-direction "column"
                   :align-items "center"
                   :gap "8px"}}
     [:div {:style {:width "120px"
                    :height "80px"
                    :background "var(--catalog-card-bg, #1e1e3a)"
                    :border-radius "8px"
                    :box-shadow "0px 0px 12px 0px rgba(0,0,0,0.3)"}}]
     [:span {:style {:font-size "11px"
                     :font-family "monospace"
                     :color "var(--catalog-text-muted, #8892b0)"}}
      "menuShadow"]]
    [:div {:style {:display "flex"
                   :flex-direction "column"
                   :align-items "center"
                   :gap "8px"}}
     [:div {:style {:width "120px"
                    :height "80px"
                    :background "var(--catalog-card-bg, #1e1e3a)"
                    :border-radius "8px"
                    :box-shadow "0px 4px 4px rgba(0,0,0,0.3)"}}]
     [:span {:style {:font-size "11px"
                     :font-family "monospace"
                     :color "var(--catalog-text-muted, #8892b0)"}}
      "alertShadow"]]]])

;; ---------------------------------------------------------------------------
;; Icon grid
;; ---------------------------------------------------------------------------

(def ^:private sample-icons
  ["absolute" "add" "arrow" "arrow-down" "arrow-left" "arrow-right"
   "arrow-up" "board" "boolean-union" "boolean-intersection"
   "boolean-difference" "boolean-exclude" "boolean-flatten"
   "broken-link" "bug" "clip-content" "clipboard" "clock"
   "close" "code" "column" "comments" "component" "corner-radius"
   "crown" "curve" "delete" "detach" "document" "download"
   "drop" "effects" "elipse" "exit" "expand" "external-link"
   "eye-off" "feedback" "fill-content" "filter" "flex"
   "flex-grid" "flip-horizontal" "flip-vertical" "folder"
   "graphics" "grid" "group" "help" "hide" "history"
   "hug-content" "icon" "img" "info" "interaction" "layers"
   "library" "locate" "lock" "mask" "menu" "move" "path"
   "pentool" "picker" "pin" "play" "puzzle" "rectangle"
   "reload" "remove" "reorder" "rotation" "search" "settings"
   "shown" "svg" "swatches" "switch" "text" "tick" "tree"
   "unlock" "user" "wrap"])

(mf/defc icon-grid-section*
  "Searchable grid of all icons."
  []
  (let [filter-text* (mf/use-state "")
        filter-text  (deref filter-text*)
        filtered     (if (= "" filter-text)
                       sample-icons
                       (filterv #(.includes % filter-text)
                                sample-icons))]
    [:> showcase* {:title (str "Icons (" (count sample-icons) " shown)")
                   :description "Subset of the 278 icon set — uses SVG sprite #icon-{id}"}
     [:div {:style {:width "100%"}}
      [:input {:type "text"
               :placeholder "Filter icons..."
               :value filter-text
               :on-change #(reset! filter-text* (.. % -target -value))
               :style {:width "100%"
                       :max-width "300px"
                       :padding "8px 12px"
                       :margin-bottom "16px"
                       :border-radius "6px"
                       :border "1px solid var(--catalog-border, #2a2a4a)"
                       :background "var(--catalog-bg, #1a1a2e)"
                       :color "var(--catalog-text, #e0e0e0)"
                       :font-size "13px"}}]
      [:div {:style {:display "grid"
                     :grid-template-columns "repeat(auto-fill, minmax(90px, 1fr))"
                     :gap "8px"}}
       (for [icon-id filtered]
         [:div {:key icon-id
                :style {:display "flex"
                        :flex-direction "column"
                        :align-items "center"
                        :gap "4px"
                        :padding "8px 4px"
                        :border-radius "6px"
                        :border "1px solid var(--catalog-border, #2a2a4a)"}}
          [:svg {:width "16" :height "16"
                 :style {:color "var(--catalog-text, #e0e0e0)"}}
           [:use {:href (str "#icon-" icon-id)
                  :width "16" :height "16"}]]
          [:span {:style {:font-size "9px"
                          :text-align "center"
                          :color "var(--catalog-text-muted, #8892b0)"
                          :word-break "break-all"}}
           icon-id]])]]]))

;; ---------------------------------------------------------------------------
;; Main section
;; ---------------------------------------------------------------------------

(mf/defc tokens-section*
  "Top-level tokens page combining all sub-sections."
  []
  [:div
   [:h2 {:style {:margin "0 0 24px"
                 :font-size "24px"
                 :font-weight "700"
                 :color "var(--catalog-text, #e0e0e0)"}}
    "Design Tokens"]
   [:> colors-section*]
   [:> typography-section*]
   [:> spacing-section*]
   [:> radii-section*]
   [:> shadows-section*]
   [:> icon-grid-section*]])
