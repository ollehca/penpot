;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ============================================================================
;; ADDED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; New file: the AI provider page of the settings (the active provider, and
;; one API key and one model per provider). These controls used to live in
;; a window of their own, opened by Settings… in the application menu and
;; apart from the settings the account menu leads to; the menu item now
;; leads to these pages. Licensed under MPL-2.0 like the rest of the
;; frontend.
;; Date: 2026-11-19
;; ============================================================================

(ns app.main.ui.settings.ai-provider
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.workspace.ai-chat :as ai-chat]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [clojure.string :as str]
   [promesa.core :as p]
   [rumext.v2 :as mf]))

(defn- ai-api
  "The AI side of the desktop bridge (window.electronAPI.ai), nil outside
  the desktop app. A key goes to the main process and never comes back:
  the page only ever learns whether a provider has one."
  []
  (some-> (unchecked-get js/window "electronAPI")
          (unchecked-get "ai")))

(defn- succeeded?
  "True when the main process says a change was made."
  [result]
  (true? (some-> result (unchecked-get "success"))))

(defn- save-failure-text
  "Why a key could not be stored. It is encrypted with the system keyring;
  where there is none (some Linux desktops) the text says so, and on a
  desktop Electron does not recognise it names the start-up switch that is
  the remedy there."
  [result]
  (if (= "unknown-desktop" (some-> result (unchecked-get "reason")))
    (tr "kizuku.settings.ai.save-failed-desktop")
    (tr "kizuku.settings.ai.save-failed")))

;; --- API keys

(mf/defc key-row*
  {::mf/private true}
  [{:keys [provider has-key? on-save on-clear]}]
  (let [{:keys [id label]} provider
        draft* (mf/use-state "")
        draft  (str/trim @draft*)

        on-change
        (mf/use-fn
         (fn [event]
           (reset! draft* (dom/get-value (dom/get-target event)))))

        ;; The field is emptied only once the key is stored: after a
        ;; failure it is still there to be saved again.
        save
        (mf/use-fn
         (mf/deps id draft on-save)
         (fn [_]
           (when (seq draft)
             (-> (on-save id draft)
                 (p/then (fn [stored?]
                           (when stored?
                             (reset! draft* ""))))))))

        on-key-down
        (mf/use-fn
         (mf/deps save)
         (fn [event]
           (when (kbd/enter? event)
             (save event))))

        clear
        (mf/use-fn
         (mf/deps id on-clear)
         (fn [_]
           (on-clear id)))]

    [:div {:class (stl/css :key-row)
           :data-provider id}
     [:span {:class (stl/css :provider-name)} label]
     [:span {:class (stl/css-case :key-status true
                                  :has-key has-key?)}
      (tr (if has-key? "kizuku.ai.key-set" "kizuku.ai.no-key"))]
     [:input {:class (stl/css :text-input)
              :type "password"
              :auto-complete "off"
              :spell-check false
              :aria-label label
              :placeholder (if has-key?
                             "••••••••••••••••"
                             (tr "kizuku.settings.ai.paste-key"))
              :value @draft*
              :on-change on-change
              :on-key-down on-key-down}]
     [:button {:class (stl/css :action-btn)
               :type "button"
               :disabled (empty? draft)
               :data-action "save-key"
               :on-click save}
      (tr "kizuku.settings.ai.save")]
     [:button {:class (stl/css :action-btn :danger)
               :type "button"
               :disabled (not has-key?)
               :data-action "clear-key"
               :on-click clear}
      (tr "kizuku.settings.ai.clear")]]))

;; --- Models

(mf/defc model-row*
  {::mf/private true}
  [{:keys [provider]}]
  (let [{:keys [id label]} provider
        value*  (mf/use-state "")
        ;; the model the main process holds for this provider
        stored* (mf/use-state nil)
        tiers*  (mf/use-state [])
        value   (str/trim @value*)
        title   (tr "kizuku.settings.ai.provider-model" label)

        store
        (mf/use-fn
         (mf/deps id)
         (fn [model-id]
           (when-let [api (ai-api)]
             (-> (.setModel ^js api id model-id)
                 (p/then (fn [result]
                           (when (succeeded? result)
                             (reset! stored* model-id))))))))

        on-change
        (mf/use-fn
         (fn [event]
           (reset! value* (dom/get-value (dom/get-target event)))))

        save
        (mf/use-fn
         (mf/deps value store)
         (fn [_]
           (when (seq value)
             (store value))))

        on-key-down
        (mf/use-fn
         (mf/deps save)
         (fn [event]
           (when (kbd/enter? event)
             (save event))))

        ;; A curated tier is one click: it fills the field and is stored.
        pick-tier
        (mf/use-fn
         (mf/deps store)
         (fn [event]
           (let [model-id (dom/get-data (dom/get-current-target event) "model")]
             (reset! value* model-id)
             (store model-id))))]

    (mf/with-effect [id]
      (when-let [api (ai-api)]
        (-> (.getModel ^js api id)
            (p/then (fn [model-id]
                      (reset! stored* model-id)
                      (reset! value* (or model-id "")))))
        (-> (.getModelTiers ^js api id)
            (p/then (fn [tiers]
                      (reset! tiers* (js->clj tiers :keywordize-keys true)))))))

    [:div {:class (stl/css :model-row)
           :data-provider id}
     [:div {:class (stl/css :model-fields)}
      [:span {:class (stl/css :provider-name :model-name)} title]
      [:input {:class (stl/css :text-input)
               :type "text"
               :auto-complete "off"
               :spell-check false
               :aria-label title
               :placeholder (tr "kizuku.settings.ai.model-id")
               :value @value*
               :on-change on-change
               :on-key-down on-key-down}]
      ;; Nothing to save while the field holds the stored model.
      [:button {:class (stl/css :action-btn)
                :type "button"
                :disabled (or (empty? value) (= value @stored*))
                :data-action "save-model"
                :on-click save}
       (tr "kizuku.settings.ai.save")]]
     [:div {:class (stl/css :model-tiers)}
      (for [tier @tiers*]
        [:button {:key (:id tier)
                  :class (stl/css-case :tier-chip true
                                       :selected (= (:id tier) @stored*))
                  :type "button"
                  :title (:id tier)
                  :data-model (:id tier)
                  :on-click pick-tier}
         (:label tier)])]]))

;; --- The page

(mf/defc ai-provider-settings*
  {::mf/private true}
  []
  (let [provider*   (mf/use-state nil)
        key-status* (mf/use-state nil)
        key-error*  (mf/use-state nil)
        provider    @provider*
        key-status  @key-status*
        key-error   @key-error*

        load-key-status
        (mf/use-fn
         (fn []
           (when-let [api (ai-api)]
             (-> (.getKeyStatus ^js api)
                 (p/then (fn [status]
                           (reset! key-status* (js->clj status :keywordize-keys true))))))))

        select-provider
        (mf/use-fn
         (fn [event]
           (when-let [api (ai-api)]
             (let [provider-id (dom/get-data (dom/get-current-target event) "provider")]
               (-> (.setActiveProvider ^js api provider-id)
                   (p/then (fn [result]
                             (when (succeeded? result)
                               (reset! provider* provider-id)))))))))

        ;; Resolves to true when the key is stored; a row empties its field
        ;; on that, and keeps what was typed otherwise.
        save-key
        (mf/use-fn
         (mf/deps load-key-status)
         (fn [provider-id api-key]
           (if-let [api (ai-api)]
             (-> (.storeKey ^js api provider-id api-key)
                 (p/then (fn [result]
                           (let [stored? (succeeded? result)]
                             (reset! key-error* (when-not stored? (save-failure-text result)))
                             (when stored?
                               (load-key-status))
                             stored?)))
                 (p/catch (fn [_]
                            (reset! key-error* (tr "kizuku.settings.ai.save-failed"))
                            false)))
             (p/resolved false))))

        clear-key
        (mf/use-fn
         (mf/deps load-key-status)
         (fn [provider-id]
           (when-let [api (ai-api)]
             (-> (.clearKey ^js api provider-id)
                 (p/then (fn [_]
                           (reset! key-error* nil)
                           (load-key-status)))))))]

    (mf/with-effect []
      (when-let [api (ai-api)]
        (-> (.getActiveProvider ^js api)
            (p/then (fn [provider-id]
                      (reset! provider* provider-id))))
        (load-key-status)))

    [:*
     [:h3 (tr "kizuku.settings.ai.active")]
     [:div {:class (stl/css :provider-options)
            :role "radiogroup"
            :aria-label (tr "kizuku.settings.ai.active")}
      (for [{:keys [id label]} ai-chat/providers]
        [:button {:key id
                  :class (stl/css-case :provider-option true
                                       :selected (= id provider))
                  :type "button"
                  :role "radio"
                  :aria-checked (= id provider)
                  :data-provider id
                  :on-click select-provider}
         label])]

     [:h3 (tr "kizuku.settings.ai.keys")]
     [:div {:class (stl/css :rows)}
      (for [entry ai-chat/providers]
        [:> key-row* {:key (:id entry)
                      :provider entry
                      :has-key? (boolean (get key-status (keyword (:id entry))))
                      :on-save save-key
                      :on-clear clear-key}])]
     (when key-error
       [:p {:class (stl/css :key-error)
            :role "alert"}
        key-error])

     [:h3 (tr "kizuku.settings.ai.model")]
     [:p {:class (stl/css :hint)} (tr "kizuku.settings.ai.model-hint")]
     [:div {:class (stl/css :rows)}
      (for [entry ai-chat/providers]
        [:> model-row* {:key (:id entry)
                        :provider entry}])]]))

(mf/defc ai-provider-page*
  []
  (mf/with-effect []
    (dom/set-html-title (tr "title.settings.options")))

  [:div {:class (stl/css :dashboard-settings)}
   [:div {:class (stl/css :form-container)
          :data-testid "settings-ai-form"}
    [:h2 (tr "kizuku.settings.ai.title")]
    [:p {:class (stl/css :hint)} (tr "kizuku.settings.ai.desc")]
    ;; The keys and models are kept by the desktop app: there is nothing
    ;; to show without it (the frontend opened in a plain browser).
    (when (ai-api)
      [:> ai-provider-settings*])]])
