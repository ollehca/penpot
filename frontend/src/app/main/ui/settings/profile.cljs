;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

;; ============================================================================
;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; Original file from PenPot (https://github.com/penpot/penpot)
;; Licensed under Mozilla Public License Version 2.0
;; Modifications: Kata polish pass 2 — static local-first notice and "own your
;;   work" card below the profile form (hardcoded English per fork precedent;
;;   no state, no handlers).
;; Date: 2026-07-07
;; Beta hide pass (2026-07-08): removed the "Change email" link and the
;;   remove-account section (cloud account concepts); name + avatar stay, the
;;   email field remains display-only.
;; Copy review (2026-07-09): removed the local-first notice and "own your
;;   work" card again — repetitive messaging; the page is just the profile
;;   form now.
;; Plan section (2026-07-13): added a "Plan" block below the profile form,
;;   driven entirely by the entitlements snapshot (never inspects license
;;   type/tier — see docs/ENTITLEMENTS_PLAN.md). Reads
;;   window.electronAPI.entitlements.get() on mount, subscribes to
;;   entitlements.onChanged, and swaps a license via
;;   window.electronAPI.license.change(token). Renders a tier pill
;;   (Professional / Founder / Private), an "Upgrade" primary action (private
;;   only) opening https://getkizuku.app via dom/open-new-window (same call as
;;   the dashboard sidebar Help center), and an always-visible "Change license"
;;   input + "Validate" button. Success reuses ntf/success; errors render
;;   inline. Degrades to nothing tier-specific when the Electron bridge is
;;   absent (plain browser / dev). Copy: sentence case, no hype, bare URL.
;; ============================================================================

(ns app.main.ui.settings.profile
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.schema :as sm]
   [app.config :as cf]
   [app.main.data.notifications :as ntf]
   [app.main.data.profile :as du]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.file-uploader :refer [file-uploader]]
   [app.main.ui.components.forms :as fm]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [clojure.string :as str]
   [rumext.v2 :as mf]))

(def ^:private schema:profile-form
  [:map {:title "ProfileForm"}
   [:fullname [::sm/text {:max 250}]]
   [:email ::sm/email]])

(defn- on-success
  [_]
  (st/emit! (ntf/success (tr "notifications.profile-saved"))))

(defn- on-submit
  [form _event]
  (let [data  (:clean-data @form)]
    (st/emit! (du/update-profile data)
              (du/persist-profile {:on-success on-success}))))

;; --- Profile Form

(mf/defc profile-form
  {::mf/private true}
  []
  (let [profile (mf/deref refs/profile)
        form    (fm/use-form :schema schema:profile-form
                             :initial profile)]

    [:& fm/form {:on-submit on-submit
                 :form form
                 :class (stl/css :profile-form)}
     [:div {:class (stl/css :fields-row)}
      [:& fm/input
       {:type "text"
        :name :fullname
        :label (tr "dashboard.your-name")}]]

     [:div {:class (stl/css :fields-row)}
      [:& fm/input
       {:type "email"
        :name :email
        :disabled true
        :label (tr "dashboard.your-email")}]]

     [:> fm/submit-button*
      {:label (tr "dashboard.save-settings")
       :disabled (empty? (:touched @form))
       :class (stl/css :btn-primary)}]]))

;; --- Profile Photo Form

(mf/defc profile-photo-form
  {::mf/private true}
  []
  (let [input-ref  (mf/use-ref nil)
        profile    (mf/deref refs/profile)

        photo
        (mf/with-memo [profile]
          (cf/resolve-profile-photo-url profile))

        on-image-click
        (mf/use-fn
         #(dom/click (mf/ref-val input-ref)))

        on-file-selected
        (fn [file]
          (st/emit! (du/update-photo file)))]

    [:form {:class (stl/css :avatar-form)}
     [:div {:class (stl/css :image-change-field)}
      [:span {:class (stl/css :update-overlay)
              :on-click on-image-click} (tr "labels.update")]
      [:img {:src photo}]
      [:& file-uploader {:accept "image/jpeg,image/png"
                         :multi false
                         :ref input-ref
                         :on-selected on-file-selected
                         :data-testid "profile-image-input"}]]]))

;; --- Plan Section
;;
;; Entitlements are the single source of truth for tier/paid/cloud gating
;; (docs/ENTITLEMENTS_PLAN.md). This block never inspects the license type
;; directly — it reads the serialized snapshot exposed on the Electron
;; preload bridge (window.electronAPI.entitlements) and degrades to nothing
;; tier-specific when that bridge is absent (plain browser / dev).

(defn- electron-api
  "The Electron preload bridge, or nil when running outside the desktop app."
  []
  (unchecked-get js/window "electronAPI"))

(defn- entitlements->clj
  "Normalize a JS entitlements snapshot into a small Clojure map."
  [snapshot]
  (when snapshot
    {:tier       (unchecked-get snapshot "tier")
     :founder    (true? (unchecked-get snapshot "founder"))
     :can-upgrade (true? (unchecked-get snapshot "canUpgrade"))
     :is-paid    (true? (unchecked-get snapshot "isPaid"))}))

(defn- tier-label
  "Sentence-case badge label from the entitlements snapshot."
  [{:keys [founder is-paid]}]
  (cond
    founder "Founder"
    is-paid "Professional"
    :else   "Private"))

(mf/defc plan-section
  {::mf/private true}
  []
  (let [ents      (mf/use-state nil)          ; entitlements snapshot (clj map)
        token     (mf/use-state "")           ; change-license input value
        error     (mf/use-state nil)          ; inline error message
        pending   (mf/use-state false)        ; validate in flight

        refresh
        (mf/use-fn
         (fn []
           (when-let [api (electron-api)]
             (when-let [ent (unchecked-get api "entitlements")]
               (-> (.get ^js ent)
                   (.then #(reset! ents (entitlements->clj %)))
                   (.catch (fn [_] nil)))))))

        on-token-change
        (mf/use-fn
         (fn [event]
           (reset! error nil)
           (reset! token (dom/get-value (dom/get-target event)))))

        on-upgrade
        (mf/use-fn
         (fn [_]
           (dom/open-new-window "https://getkizuku.app")))

        on-validate
        (mf/use-fn
         (mf/deps @token)
         (fn [_]
           (let [code (str/trim (or @token ""))]
             (when (and (seq code) (not @pending))
               (reset! pending true)
               (reset! error nil)
               (if-let [api (electron-api)]
                 (if-let [lic (unchecked-get api "license")]
                   (-> (.change ^js lic code)
                       (.then
                        (fn [res]
                          (reset! pending false)
                          (if (unchecked-get res "success")
                            (do
                              (reset! token "")
                              (when-let [snap (unchecked-get res "entitlements")]
                                (reset! ents (entitlements->clj snap)))
                              (st/emit! (ntf/success (tr "notifications.profile-saved"))))
                            (reset! error (or (unchecked-get res "error")
                                              "That license could not be validated.")))))
                       (.catch
                        (fn [_]
                          (reset! pending false)
                          (reset! error "That license could not be validated."))))
                   (do (reset! pending false)
                       (reset! error "License changes are only available in the app.")))
                 (do (reset! pending false)
                     (reset! error "License changes are only available in the app.")))))))]

    ;; Load on mount and subscribe to live changes (broadcast on activation).
    (mf/with-effect []
      (refresh)
      (when-let [api (electron-api)]
        (when-let [ent (unchecked-get api "entitlements")]
          ;; onChanged returns an unsubscribe fn per the preload contract.
          (let [unsub (.onChanged ^js ent
                                  (fn [snapshot]
                                    (reset! ents (entitlements->clj snapshot))))]
            (fn [] (when (fn? unsub) (unsub)))))))

    ;; Nothing tier-specific outside the desktop bridge.
    (when (some? @ents)
      [:div {:class (stl/css :plan-section)}
       [:h2 "Plan"]

       [:div {:class (stl/css :plan-row)}
        [:span {:class (stl/css-case :plan-badge true
                                     :is-paid (:is-paid @ents)
                                     :is-founder (:founder @ents))}
         (tier-label @ents)]]

       (when (:can-upgrade @ents)
         [:div {:class (stl/css :plan-upgrade)}
          [:p {:class (stl/css :plan-upgrade-text)}
           "Unlock cloud sync, sharing and more."]
          [:button {:class (stl/css :plan-upgrade-btn)
                    :type "button"
                    :on-click on-upgrade}
           "Upgrade"]])

       [:div {:class (stl/css :plan-change)}
        [:label {:class (stl/css :plan-change-label)}
         "Change license"]
        [:div {:class (stl/css :plan-change-controls)}
         [:input {:class (stl/css :plan-change-input)
                  :type "text"
                  :placeholder "Paste license"
                  :value @token
                  :on-change on-token-change}]
         [:button {:class (stl/css :plan-validate-btn)
                   :type "button"
                   :disabled (or @pending (empty? (str/trim (or @token ""))))
                   :on-click on-validate}
          (if @pending "Validating" "Validate")]]
        (when @error
          [:p {:class (stl/css :plan-change-error)} @error])]])))

;; --- Profile Page

(mf/defc profile-page
  []
  (mf/with-effect []
    (dom/set-html-title (tr "title.settings.profile")))

  [:div {:class (stl/css :dashboard-settings)}
   [:div {:class (stl/css :form-container)}
    [:h2 (tr "labels.profile")]
    [:& profile-photo-form]
    [:& profile-form]
    [:& plan-section]]])

