;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

;; ============================================================================
;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; Original file from PenPot (https://github.com/penpot/penpot)
;; Licensed under Mozilla Public License Version 2.0
;; Modifications: Single-user mode — synthetic team creation, skip team auth
;; Date: 2026-02-15
;; Modifications: Restore fetch-shared-files, shared-files-fetched and the
;;   finalize-team state cleanup that were accidentally removed in 46faa952b;
;;   they are still referenced by dashboard/workspace libraries UI
;; Date: 2026-07-08
;; Modifications: Re-add the team-management public vars removed in 46faa952b
;;   as safe no-op events (see the stub section at the end of this file);
;;   restores a zero-warning release build
;; Date: 2026-07-11
;; ============================================================================

(ns app.main.data.team
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.logging :as log]
   [app.common.schema :as sm]
   [app.common.types.team :as ctt]
   [app.common.uri :as u]
   [app.config :as cf]
   [app.main.data.event :as ev]
   [app.main.data.media :as di]
   [app.main.features :as features]
   [app.main.repo :as rp]
   [app.main.router :as rt]
   [app.util.storage :as storage]
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(log/set-level! :warn)

;; KIZUKU: Check if single-user mode is enabled
(defn- kizuku-single-user-mode? []
  (try
    (= "true" (.getItem js/localStorage "kizuku-single-user-mode"))
    (catch :default _ false)))

(defn get-last-team-id
  "Get last accessed team id"
  []
  (::current-team-id storage/global))

(defn teams-fetched
  [teams]
  (ptk/reify ::teams-fetched
    IDeref
    (-deref [_] teams)

    ptk/UpdateEvent
    (update [_ state]
      (reduce (fn [state {:keys [id] :as team}]
                (update-in state [:teams id] merge team))
              state
              teams))))

(defn fetch-teams
  []
  (ptk/reify ::fetch-teams
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rp/cmd! :get-teams)
           (rx/map teams-fetched)))))

;; --- EVENT: fetch-members

(defn- members-fetched
  [team-id members]
  (ptk/reify ::members-fetched
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (update-in [:teams team-id] assoc :members members)
          (update :profiles merge (d/index-by :id members))))))

(defn fetch-members
  ([] (fetch-members nil))
  ([team-id]
   (ptk/reify ::fetch-members
     ptk/WatchEvent
     (watch [_ state _]
       (when-let [team-id (or team-id (:current-team-id state))]
         (->> (rp/cmd! :get-team-members {:team-id team-id})
              (rx/map (partial members-fetched team-id))))))))

(defn- invitations-fetched
  [team-id invitations]
  (ptk/reify ::invitations-fetched
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:teams team-id] assoc :invitations invitations))))

(defn fetch-invitations
  []
  (ptk/reify ::fetch-invitations
    ptk/WatchEvent
    (watch [_ state _]
      (let [team-id (:current-team-id state)]
        (->> (rp/cmd! :get-team-invitations {:team-id team-id})
             (rx/map (partial invitations-fetched team-id)))))))

;; MODIFIED BY KIZUKU - Skip team auth check in single-user mode
(defn- team-initialized
  [team-id]
  (ptk/reify ::team-initialized
    ptk/WatchEvent
    (watch [_ state _]
      (let [teams (get state :teams)
            team  (get teams team-id)
            kizuku-mode? (kizuku-single-user-mode?)]

        ;; KIZUKU: If single-user mode + no team + valid team-id, create synthetic team
        (if (and kizuku-mode? (not team) (some? team-id))
          (do
            (.log js/console "[KIZUKU] Single-user mode: creating synthetic team for ID:" (str team-id))
            (let [default-permissions {:can-edit true
                                      :can-read true
                                      :is-owner true
                                      :is-admin true}
                  synthetic-team {:id team-id
                                 :name "Kizuku Workspace"
                                 :is-default true
                                 :permissions default-permissions
                                 :is-owner true}]
              (rx/of
               #(do (.log js/console "[KIZUKU] Adding synthetic team to state.teams")
                    (assoc-in % [:teams team-id] synthetic-team))
               #(do (.log js/console "[KIZUKU] Setting permissions")
                    (assoc % :permissions default-permissions)))))
          
          ;; Original PenPot logic
          (if (not team)
            (rx/throw (ex/error :type :authentication))
            (let [permissions (get team :permissions)
                  features    (get team :features)]
              (rx/of #(assoc % :permissions permissions)
                     (features/initialize features)
                     (fetch-members team-id)))))))

    ptk/EffectEvent
    (effect [_ _ _]
      (swap! storage/global assoc ::current-team-id team-id))))

(defn initialize-team
  [team-id]
  (ptk/reify ::initialize-team
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :current-team-id team-id))

    ptk/WatchEvent
    (watch [_ _ stream]
      (let [stopper (rx/filter (ptk/type? ::finalize-team) stream)]
        (rx/merge
         (->> (rx/of (team-initialized team-id))
              (rx/take-until stopper))

         (->> stream
              (rx/filter (ptk/type? ::team-initialized))
              (rx/take 1)
              (rx/mapcat #(rx/of (fetch-invitations)))
              (rx/take-until stopper)))))))

(defn finalize-team
  [team-id]
  (ptk/reify ::finalize-team
    ptk/UpdateEvent
    (update [_ state]
      (let [team-id' (get state :current-team-id)]
        (if (= team-id' team-id)
          (-> state
              (dissoc :current-team-id)
              (dissoc :shared-files)
              (dissoc :fonts))
          state)))))

(defn- shared-files-fetched
  [files]
  (ptk/reify ::shared-files-fetched
    ptk/UpdateEvent
    (update [_ state]
      (let [files (d/index-by :id files)]
        (update state :shared-files merge files)))))

(defn fetch-shared-files
  "Event mainly used for fetch a list of shared libraries for a team,
  this list does not includes the content of the library per se.  It
  is used mainly for show available libraries and a summary of it."
  ([] (fetch-shared-files nil))
  ([team-id]
   (ptk/reify ::fetch-shared-files
     ptk/WatchEvent
     (watch [_ state _]
       (when-let [team-id (or team-id (:current-team-id state))]
         (->> (rp/cmd! :get-team-shared-files {:team-id team-id})
              (rx/map shared-files-fetched)))))))

;; ============================================================================
;; KIZUKU: no-op stubs for team-management events
;;
;; The team-management UI (members, invitations, webhooks, team CRUD) is
;; hidden in Kizuku's single-user mode, but the components that reference
;; these events are still compiled into the release bundle. The original
;; implementations were removed in 46faa952b, which left dangling references
;; and ~20 "use of undeclared var" release warnings, and turned every hidden
;; crash site into a runtime error if it were ever reached.
;;
;; These stubs accept the original argument shapes and emit nothing (an
;; event with no Watch/Update implementation is inert in potok), so the
;; release build compiles with ZERO warnings again — keeping the warning
;; gate meaningful as a regression signal — and any stray call becomes a
;; safe no-op instead of a crash.
;; ============================================================================

(defn create-team
  "KIZUKU no-op stub. Original: [{:keys [name] :as params}]"
  [_params]
  (ptk/reify ::create-team))

(defn create-team-with-invitations
  "KIZUKU no-op stub. Original: [{:keys [name emails role] :as params}]"
  [_params]
  (ptk/reify ::create-team-with-invitations))

(defn update-team
  "KIZUKU no-op stub. Original: [{:keys [id name] :as params}]"
  [_params]
  (ptk/reify ::update-team))

(defn leave-current-team
  "KIZUKU no-op stub. Original: [{:keys [reassign-to] :as params}]"
  [_params]
  (ptk/reify ::leave-current-team))

(defn delete-team
  "KIZUKU no-op stub. Original: [{:keys [id] :as params}]"
  [_params]
  (ptk/reify ::delete-team))

(defn create-invitations
  "KIZUKU no-op stub. Original: [{:keys [emails role team-id resend?] :as params}]"
  [_params]
  (ptk/reify ::create-invitations))

(defn delete-invitation
  "KIZUKU no-op stub. Original: [{:keys [email team-id] :as params}]"
  [_params]
  (ptk/reify ::delete-invitation))

(defn copy-invitation-link
  "KIZUKU no-op stub. Original: [{:keys [email team-id] :as params}]"
  [_params]
  (ptk/reify ::copy-invitation-link))

(defn update-invitation-role
  "KIZUKU no-op stub. Original: [{:keys [email team-id role] :as params}]"
  [_params]
  (ptk/reify ::update-invitation-role))

(defn update-member-role
  "KIZUKU no-op stub. Original: [{:keys [role member-id] :as params}]"
  [_params]
  (ptk/reify ::update-member-role))

(defn delete-member
  "KIZUKU no-op stub. Original: [{:keys [member-id] :as params}]"
  [_params]
  (ptk/reify ::delete-member))

(defn create-webhook
  "KIZUKU no-op stub. Original: [{:keys [uri mtype is-active] :as params}]"
  [_params]
  (ptk/reify ::create-webhook))

(defn update-webhook
  "KIZUKU no-op stub. Original: [{:keys [id uri mtype is-active] :as params}]"
  [_params]
  (ptk/reify ::update-webhook))

(defn delete-webhook
  "KIZUKU no-op stub. Original: [{:keys [id] :as params}]"
  [_params]
  (ptk/reify ::delete-webhook))

(defn fetch-webhooks
  "KIZUKU no-op stub. Original: []"
  []
  (ptk/reify ::fetch-webhooks))

(defn update-team-photo
  "KIZUKU no-op stub. Original: [file]"
  [_file]
  (ptk/reify ::update-team-photo))

(defn fetch-stats
  "KIZUKU no-op stub. Original: []"
  []
  (ptk/reify ::fetch-stats))
