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
  []
  (ptk/reify ::finalize-team))
