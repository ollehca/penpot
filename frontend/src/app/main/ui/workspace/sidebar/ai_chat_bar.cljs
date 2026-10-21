;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ADDED BY KIZUKU (2026-10-21): Phase 1 proof-of-pipeline UI for the AI
;; canvas integration — a single input + send button, not the full chat
;; panel (that's Phase 2's ai_chat.cljs, with history/turn-status/etc).
;; This exists only to prove the whole bridge end to end: type a prompt,
;; watch a real tool call mutate the canvas, confirm one undo removes it.

(ns app.main.ui.workspace.sidebar.ai-chat-bar
  (:require
   [app.main.data.workspace.ai-tools :as ai-tools]
   [app.util.dom :as dom]
   [clojure.string :as str]
   [rumext.v2 :as mf]))

(defn- electron-api
  []
  (unchecked-get js/window "electronAPI"))

(mf/defc ai-chat-bar*
  {::mf/private true}
  []
  (let [prompt   (mf/use-state "")
        status   (mf/use-state nil)  ; nil | "sending" | "error" | "done"
        error    (mf/use-state nil)

        on-change
        (mf/use-fn
         (fn [event]
           (reset! prompt (dom/get-value (dom/get-target event)))))

        on-send
        (mf/use-fn
         (mf/deps @prompt @status)
         (fn [_]
           (let [text (str/trim (or @prompt ""))
                 api  (electron-api)]
             (when (and (seq text) (not= @status "sending") api)
               (reset! status "sending")
               (reset! error nil)
               (-> (.sendPrompt ^js (.-ai api)
                                #js {:provider "anthropic"
                                     :prompt text
                                     :systemPrompt (.stringify js/JSON (ai-tools/describe-canvas-state))
                                     :history #js []})
                   (.then
                    (fn [res]
                      (if (unchecked-get res "success")
                        (do (reset! status "done") (reset! prompt ""))
                        (do (reset! status "error")
                            (reset! error (unchecked-get res "error"))))))
                   (.catch
                    (fn [err]
                      (reset! status "error")
                      (reset! error (or (.-message err) (str err))))))))))]

    [:div {:style {:padding "8px"
                   :display "flex"
                   :gap "6px"
                   :border-top "1px solid rgba(255,255,255,0.1)"}}
     [:input
      {:type "text"
       :placeholder "Ask the AI to edit this file…"
       :value @prompt
       :disabled (= @status "sending")
       :on-change on-change
       :style {:flex "1"}}]
     [:button
      {:on-click on-send
       :disabled (or (= @status "sending") (empty? (str/trim (or @prompt ""))))}
      (if (= @status "sending") "Sending…" "Send")]
     (when @error
       [:span {:style {:color "red" :font-size "11px"}} @error])]))
