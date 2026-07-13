;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

;; ============================================================================
;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; Original file from PenPot (https://github.com/penpot/penpot)
;; Licensed under Mozilla Public License Version 2.0
;; Modifications: New file — canvas empty-state overlay. When the current
;;   page has no shapes (only the root frame) it renders a centered,
;;   non-blocking Kata card with the board shortcut hint and size preset
;;   buttons that create a board via dwsh/create-and-add-shape.
;; Date: 2026-07-13
;; ============================================================================

(ns app.main.ui.workspace.viewport.empty-state
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.store :as st]
   [rumext.v2 :as mf]))

(def ^:private presets
  [{:label "Desktop 1440×900" :name "Desktop" :width 1440 :height 900}
   {:label "Phone 393×852" :name "Phone" :width 393 :height 852}
   {:label "A4 595×842" :name "A4" :width 595 :height 842}
   {:label "Square 1080×1080" :name "Square" :width 1080 :height 1080}])

(defn- create-board
  "Creates a root-level board centered in the current viewport and
  selects it (centering and selection are handled by
  create-and-add-shape / add-shape). The 0 0 frame coordinates are
  only used to look up a parent frame; the page is empty so the
  parent is always the root frame."
  [{:keys [name width height]}]
  (st/emit! (dwsh/create-and-add-shape :frame 0 0 {:name name
                                                   :width width
                                                   :height height})))

(mf/defc empty-state*
  "Non-blocking helper shown when the current page has no shapes.
  Rendered inside the viewport-overlays layer (pointer-events: none);
  only the card itself accepts pointer events. Disappears reactively
  as soon as the page has any shape, while a draw is in progress, in
  comment-placement mode, or when dismissed for the current
  page/session. A merely ARMED drawing tool does not hide it: PenPot
  auto-arms the board tool on every empty page (pages.cljs
  select-frame-tool), which would otherwise suppress the card on
  exactly the fresh files it exists for."
  [{:keys [page-id objects drawing-tool drawing-obj]}]
  (let [dismissed   (mf/use-state #{})
        empty-page? (empty? (get-in objects [uuid/zero :shapes]))

        on-dismiss
        (mf/use-fn
         (mf/deps page-id)
         #(swap! dismissed conj page-id))]

    (when (and ^boolean empty-page?
               (nil? drawing-obj)
               (not= :comments drawing-tool)
               (not (contains? @dismissed page-id)))
      [:div {:class (stl/css :empty-state)}
       [:div {:class (stl/css :card)}
        [:button {:class (stl/css :dismiss)
                  :type "button"
                  :aria-label "Dismiss"
                  :on-click on-dismiss}
         "×"]
        [:div {:class (stl/css :hint)}
         "Press "
         [:kbd {:class (stl/css :kbd)} "B"]
         " to draw a board, or start from:"]
        [:div {:class (stl/css :presets)}
         (for [{:keys [label] :as preset} presets]
           [:button {:key label
                     :class (stl/css :preset-btn)
                     :type "button"
                     :on-click (partial create-board preset)}
            label])]]])))
