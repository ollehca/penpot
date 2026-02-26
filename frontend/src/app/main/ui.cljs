;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

;; ============================================================================
;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; Original file from PenPot (https://github.com/penpot/penpot)
;; Licensed under Mozilla Public License Version 2.0
;; Modifications: Removed onboarding modal imports (newsletter, questions, team-choice)
;; Date: 2025-09-30
;; ============================================================================
(ns app.main.ui
  (:require
   [app.common.data :as d]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.data.common :as dcm]
   [app.main.data.team :as dtm]
   [app.main.errors :as errors]
   [app.main.refs :as refs]
   [app.main.repo :as rp]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.context :as ctx]
   [app.main.ui.debug.icons-preview :refer [icons-preview]]
   [app.main.ui.ds.product.loader :refer [loader*]]
   [app.main.ui.error-boundary :refer [error-boundary*]]
   [app.main.ui.exports.files]
   [app.main.ui.frame-preview :as frame-preview]
   [app.main.ui.notifications :as notifications]
   [app.main.ui.releases :refer [release-notes-modal]]
   [app.main.ui.static :as static]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.theme :as theme]
   [beicon.v2.core :as rx]
   [rumext.v2 :as mf]))

(def auth-page
  (mf/lazy-component app.main.ui.auth/auth))

(def verify-token-page
  (mf/lazy-component app.main.ui.auth.verify-token/verify-token))

(def viewer-page
  (mf/lazy-component app.main.ui.viewer/viewer*))

(def dashboard-page
  (mf/lazy-component app.main.ui.dashboard/dashboard*))

(def settings-page
  (mf/lazy-component app.main.ui.settings/settings))

(def workspace-page
  (mf/lazy-component app.main.ui.workspace/workspace*))

(mf/defc workspace-legacy-redirect*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [project-id file-id page-id layout]}]
  (mf/with-effect []
    (->> (rp/cmd! :get-project {:id project-id})
         (rx/subs! (fn [{:keys [team-id]}]
                     (st/emit! (dcm/go-to-workspace :team-id team-id
                                                    :file-id file-id
                                                    :page-id page-id
                                                    :layout layout)))
                   errors/on-error)))
  [:> loader*
   {:title (tr "labels.loading")
    :overlay true}])

(mf/defc dashboard-legacy-redirect*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [section team-id project-id search-term plugin-url template]}]
  (let [section (case section
                  :dashboard-legacy-search
                  :dashboard-search
                  :dashboard-legacy-projects
                  :dashboard-recent
                  :dashboard-legacy-files
                  :dashboard-files
                  :dashboard-legacy-libraries
                  :dashboard-libraries
                  :dashboard-legacy-fonts
                  :dashboard-fonts
                  :dashboard-legacy-font-providers
                  :dashboard-font-providers
                  :dashboard-legacy-team-members
                  :dashboard-members
                  :dashboard-legacy-team-invitations
                  :dashboard-invitations
                  :dashboard-legacy-team-webhooks
                  :dashboard-webhooks
                  :dashboard-legacy-team-settings
                  :dashboard-settings)]

    (mf/with-effect []
      (let [params {:team-id team-id
                    :project-id project-id
                    :search-term search-term
                    :plugin plugin-url
                    :template template}]
        (st/emit! (rt/nav section (d/without-nils params)))))

    [:> loader*
     {:title (tr "labels.loading")
      :overlay true}]))

(mf/defc viewer-legacy-redirect*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [page-id file-id section index share-id interactions-mode frame-id share]}]
  (mf/with-effect []
    (let [params {:page-id page-id
                  :file-id file-id
                  :section section
                  :index index
                  :share-id share-id
                  :interactions-mode interactions-mode
                  :frame-id frame-id
                  :share share}]
      (st/emit! (rt/nav :viewer (d/without-nils params)))))

  [:> loader*
   {:title (tr "labels.loading")
    :overlay true}])

(mf/defc team-container*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [team-id children]}]
  (mf/with-effect [team-id]
    (st/emit! (dtm/initialize-team team-id))
    (fn []
      (st/emit! (dtm/finalize-team team-id))))

  (let [{:keys [permissions] :as team} (mf/deref refs/team)
        ;; ============================================================================
        ;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
        ;; Original file from PenPot (https://github.com/penpot/penpot)
        ;; Licensed under Mozilla Public License Version 2.0
        ;; Modifications: Check localStorage for Kizu auth to bypass team-id validation
        ;; Date: 2025-10-29
        ;; ============================================================================
        kizu-auth? (some? (.getItem js/localStorage "auth-token"))
        team-matches? (= team-id (:id team))
        ;; For Kizu auth, provide default permissions if team not loaded
        effective-permissions (if (and kizu-auth? (not team-matches?))
                               (hash-map :can-edit true
                                         :can-read true
                                         :is-owner true
                                         :is-admin true)
                               permissions)
        should-render? (or kizu-auth? team-matches?)
        _debug (do
                 (.log js/console "[KIZU-UI] team-container* render check:")
                 (.log js/console "  team-id:" (str team-id))
                 (.log js/console "  team:" (js/JSON.stringify (clj->js team)))
                 (.log js/console "  kizu-auth?:" kizu-auth?)
                 (.log js/console "  team-matches?:" team-matches?)
                 (.log js/console "  should-render?:" should-render?)
                 (.log js/console "  effective-permissions:" (js/JSON.stringify (clj->js effective-permissions))))]
    (when should-render?
      [:> (mf/provider ctx/current-team-id) {:value team-id}
       [:> (mf/provider ctx/permissions) {:value effective-permissions}
        [:> (mf/provider ctx/can-edit?) {:value (get effective-permissions :can-edit true)}
         ;; The `:key` is mandatory here because we want to reinitialize
         ;; all dom tree instead of simple rerender.
         [:* {:key (str team-id)} children]]]])))

(mf/defc page*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [route profile]}]
  (let [{:keys [data params]} route
        props   (get profile :props)
        section (get data :name)
        team    (mf/deref refs/team)

        ;; ============================================================================
        ;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
        ;; Original file from PenPot (https://github.com/penpot/penpot)
        ;; Licensed under Mozilla Public License Version 2.0
        ;; Modifications: Disabled all onboarding modal flows (newsletter, questions, team-choice)
        ;; Date: 2025-09-30
        ;; ============================================================================
        show-question-modal? false
        show-newsletter-modal? false
        show-team-modal? false
        show-release-modal? false]

    [:& (mf/provider ctx/current-route) {:value route}
     (case section
       (:auth-login
        :auth-register
        :auth-register-validate
        :auth-register-success
        :auth-recovery-request
        :auth-recovery)
       [:? [:& auth-page {:route route}]]

       :auth-verify-token
       [:? [:& verify-token-page {:route route}]]

       (:settings-profile
        :settings-password
        :settings-options
        :settings-feedback
        :settings-subscription
        :settings-access-tokens
        :settings-notifications)
       [:? [:& settings-page {:route route}]]

       :debug-icons-preview
       (when *assert*
         [:& icons-preview])

       (:dashboard-search
        :dashboard-recent
        :dashboard-files
        :dashboard-libraries
        :dashboard-fonts
        :dashboard-font-providers
        :dashboard-members
        :dashboard-invitations
        :dashboard-webhooks
        :dashboard-settings)
       (let [params        (get params :query)
             team-id       (some-> params :team-id uuid/parse*)
             project-id    (some-> params :project-id uuid/parse*)
             search-term   (some-> params :search-term)
             plugin-url    (some-> params :plugin)
             template      (some-> params :template)]
         [:?
          #_[:& app.main.ui.releases/release-notes-modal {:version "2.5"}]
          #_[:& app.main.ui.onboarding/onboarding-templates-modal]
          #_[:& app.main.ui.onboarding/onboarding-modal]


          [:> team-container* {:team-id team-id}
           [:> dashboard-page {:profile profile
                               :section section
                               :team-id team-id
                               :search-term search-term
                               :plugin-url plugin-url
                               :project-id project-id
                               :template template}]]])

       :workspace
       (let [params     (get params :query)
             team-id    (some-> params :team-id uuid/parse*)
             file-id    (some-> params :file-id uuid/parse*)
             page-id    (some-> params :page-id uuid/parse*)
             layout     (some-> params :layout keyword)
             ;; ============================================================================
             ;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
             ;; Original file from PenPot (https://github.com/penpot/penpot)
             ;; Licensed under Mozilla Public License Version 2.0
             ;; Modifications: Use team-id as project-id for single-user mode
             ;; Date: 2025-10-30
             ;; ============================================================================
             project-id team-id] ; In single-user mode, project-id = team-id
         [:? {}

          [:> team-container* {:team-id team-id}
           [:> workspace-page {:team-id team-id
                               :project-id project-id
                               :file-id file-id
                               :page-id page-id
                               :layout-name layout
                               :key file-id}]]])

       :viewer
       (let [params   (get params :query)
             index    (some-> (:index params) parse-long)
             share-id (some-> (:share-id params) uuid/parse*)
             section  (or (some-> (:section params) keyword)
                          :interactions)

             file-id  (some-> (:file-id params) uuid/parse*)
             page-id  (some-> (:page-id params) uuid/parse*)
             imode    (or (some-> (:interactions-mode params) keyword)
                          :show-on-click)
             frame-id (some-> (:frame-id params) uuid/parse*)
             share    (:share params)]

         [:? {}
          [:> viewer-page
           {:page-id page-id
            :file-id file-id
            :frame-id frame-id
            :section section
            :index index
            :share-id share-id
            :interactions-mode imode
            :share share}]])


       :workspace-legacy
       (let [project-id (some-> params :path :project-id uuid/parse*)
             file-id    (some-> params :path :file-id uuid/parse*)
             page-id    (some-> params :query :page-id uuid/parse*)
             layout     (some-> params :query :layout keyword)]

         [:> workspace-legacy-redirect*
          {:project-id project-id
           :file-id file-id
           :page-id page-id
           :layout layout}])

       (:dashboard-legacy-search
        :dashboard-legacy-projects
        :dashboard-legacy-files
        :dashboard-legacy-libraries
        :dashboard-legacy-fonts
        :dashboard-legacy-font-providers
        :dashboard-legacy-team-members
        :dashboard-legacy-team-invitations
        :dashboard-legacy-team-webhooks
        :dashboard-legacy-team-settings)
       (let [team-id     (some-> params :path :team-id uuid/parse*)
             project-id  (some-> params :path :project-id uuid/parse*)
             search-term (some-> params :query :search-term)
             plugin-url  (some-> params :query :plugin)
             template    (some-> params :template)]
         [:> dashboard-legacy-redirect*
          {:team-id team-id
           :section section
           :project-id project-id
           :search-term search-term
           :plugin-url plugin-url
           :template template}])

       :viewer-legacy
       (let [{:keys [query-params path-params]} route
             {:keys [index share-id section page-id interactions-mode frame-id share]
              :or {section :interactions interactions-mode :show-on-click}} query-params
             {:keys [file-id]} path-params]

         [:> viewer-legacy-redirect*
          {:page-id page-id
           :file-id file-id
           :section section
           :index index
           :share-id share-id
           :interactions-mode (keyword interactions-mode)
           :frame-id frame-id
           :share share}])

       :frame-preview
       [:& frame-preview/frame-preview]

       nil)]))

(mf/defc app
  []
  (let [route   (mf/deref refs/route)
        edata   (mf/deref refs/exception)
        profile (mf/deref refs/profile)]

    ;; initialize themes
    (theme/use-initialize profile)

    (dom/prevent-browser-gesture-navigation!)

    [:& (mf/provider ctx/current-route) {:value route}
     [:& (mf/provider ctx/current-profile) {:value profile}
      (if edata
        [:> static/exception-page* {:data edata :route route}]
        [:> error-boundary* {:fallback static/internal-error*}
         [:> notifications/current-notification*]
         (when route
           [:> page* {:route route :profile profile}])])]]))
