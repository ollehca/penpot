;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; MODIFIED BY KIZUKU (2026-11-15): G C (comments) and G W (open in the
;; workspace) exist only with the flags that show those features in the
;; viewer header (:viewer-comments, :viewer-edit-link).

(ns app.main.data.viewer.shortcuts
  (:require
   [app.config :as cf]
   [app.main.data.common :as dcm]
   [app.main.data.shortcuts :as ds]
   [app.main.data.viewer :as dv]
   [app.main.store :as st]))

;; KIZUKU: the two shortcuts below are added only with their flag. Comments
;; need the cloud backend (posting one failed and lost the text), and the
;; workspace opened from the preview is a second editor on the same file in
;; a window the shell does not manage. Without the entries the shortcuts
;; panel does not list them either.
(def ^:private flagged-shortcuts
  {:viewer-comments
   [:open-comments     {:tooltip "G C"
                        :command "g c"
                        :subsections [:navigation-viewer]
                        :fn #(st/emit! (dv/go-to-section :comments))}]

   :viewer-edit-link
   [:open-workspace    {:tooltip "G W"
                        :command "g w"
                        :subsections [:navigation-viewer]
                        :fn #(st/emit! (dcm/go-to-workspace))}]})

(def ^:private base-shortcuts
  {:increase-zoom      {:tooltip "+"
                        :command "+"
                        :subsections [:zoom-viewer]
                        :fn #(st/emit! dv/increase-zoom)}

   :decrease-zoom      {:tooltip "-"
                        :command "-"
                        :subsections [:zoom-viewer]
                        :fn #(st/emit! dv/decrease-zoom)}

   :select-all         {:tooltip (ds/meta "A")
                        :command (ds/c-mod "a")
                        :subsections [:general-viewer]
                        :fn #(st/emit! (dv/select-all))}

   :reset-zoom         {:tooltip (ds/shift "0")
                        :command "shift+0"
                        :subsections [:zoom-viewer]
                        :fn #(st/emit! dv/reset-zoom)}

   :toggle-zoom-style  {:tooltip "F"
                        :command "f"
                        :subsections [:zoom-viewer]
                        :fn #(st/emit! dv/toggle-zoom-style)}

   :toggle-fullscreen  {:tooltip (ds/shift "F")
                        :command ["shift+f" "alt+enter"]
                        :subsections [:zoom-viewer]
                        :fn #(st/emit! dv/toggle-fullscreen)}

   :prev-frame         {:tooltip ds/left-arrow
                        :command ["left" "up" "shift+enter" "pageup" "shift+space"]
                        :subsections [:general-viewer]
                        :fn #(st/emit! dv/select-prev-frame)}

   :next-frame         {:tooltip ds/right-arrow
                        :command ["right" "down" "enter" "pagedown" "space"]
                        :subsections [:general-viewer]
                        :fn #(st/emit! dv/select-next-frame)}

   :open-inspect       {:tooltip "G I"
                        :command "g i"
                        :subsections [:navigation-viewer]
                        :fn #(st/emit! (dv/go-to-section :inspect))}

   :open-interactions  {:tooltip "G V"
                        :command "g v"
                        :subsections [:navigation-viewer]
                        :fn #(st/emit! (dv/go-to-section :interactions))}})

(def shortcuts
  (reduce-kv (fn [result flag [id shortcut]]
               (cond-> result
                 (contains? cf/flags flag) (assoc id shortcut)))
             base-shortcuts
             flagged-shortcuts))

(defn get-tooltip [shortcut]
  (assert (contains? shortcuts shortcut) (str shortcut))
  (get-in shortcuts [shortcut :tooltip]))
