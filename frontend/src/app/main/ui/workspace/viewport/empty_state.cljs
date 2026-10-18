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
;; Modifications (2): Removed the dismiss (×) affordance and its per-page
;;   dismissed-state tracking. This is meant to read as ambient signage (like
;;   an EXIT sign) — always present while the page is genuinely empty, gone
;;   the instant a board exists, with no user action able to suppress it
;;   while still empty. A closable card contradicts that: a user could
;;   dismiss it and then have no indicator at all on a file that still has
;;   no boards.
;; Date: 2026-10-12
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
  "Ambient signage shown when the current page has no shapes — like an EXIT
  sign: always present while true (the page is genuinely empty), never
  present once it isn't, and not something the user can dismiss away.
  Rendered inside the viewport-overlays layer (pointer-events: none); only
  the card itself accepts pointer events, so it never blocks canvas
  interaction underneath. Disappears reactively as soon as the page has any
  shape, while a draw is in progress, or in comment-placement mode. A merely
  ARMED drawing tool does not hide it: PenPot auto-arms the board tool on
  every empty page (pages.cljs select-frame-tool), which would otherwise
  suppress the card on exactly the fresh files it exists for."
  [{:keys [objects drawing-tool drawing-obj]}]
  (let [empty-page? (empty? (get-in objects [uuid/zero :shapes]))]
    (when (and ^boolean empty-page?
               (nil? drawing-obj)
               (not= :comments drawing-tool))
      [:div {:class (stl/css :empty-state)}
       [:div {:class (stl/css :card)}
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
