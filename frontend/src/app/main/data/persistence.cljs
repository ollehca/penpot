;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; MODIFIED BY KIZUKU (2026-11-15): a commit that changes the design-token
;; library also carries the library itself (with-tokens-lib-snapshot); the
;; desktop app's local backend stores that instead of replaying the token
;; operations. (2026-11-16) The library is taken when the commit is queued
;; (take-tokens-lib-snapshot), not when it is sent.

(ns app.main.data.persistence
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.logging :as log]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.repo :as rp]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(declare ^:private run-persistence-task)

(log/set-level! :warn)

(def running (atom false))
(def revn-data (atom {}))
(def queue-conj (fnil conj #queue []))

(defn- update-status
  [status]
  (ptk/reify ::update-status
    ptk/UpdateEvent
    (update [_ state]
      (update state :persistence (fn [pstate]
                                   (log/trc :hint "update-status"
                                            :from (:status pstate)
                                            :to status)
                                   (let [status (if (and (= status :pending)
                                                         (= (:status pstate) :saving))
                                                  (:status pstate)
                                                  status)]

                                     (-> (assoc pstate :status status)
                                         (cond-> (= status :error)
                                           (dissoc :run-id))
                                         (cond-> (= status :saved)
                                           (dissoc :run-id)))))))))

(defn- update-file-revn
  [file-id revn]
  (ptk/reify ::update-file-revn
    ptk/UpdateEvent
    (update [_ state]
      (log/dbg :hint "update-file-revn" :file-id (dm/str file-id) :revn revn)
      (dsh/update-file state file-id #(update % :revn max revn)))

    ptk/EffectEvent
    (effect [_ _ _]
      (swap! revn-data update file-id (fnil max 0) revn))))

(defn- discard-commit
  [commit-id]
  (ptk/reify ::discard-commit
    ptk/UpdateEvent
    (update [_ state]
      (update state :persistence (fn [pstate]
                                   (-> pstate
                                       (update :queue (fn [queue]
                                                        (if (= commit-id (peek queue))
                                                          (pop queue)
                                                          (throw (ex-info "invalid state" {})))))
                                       (update :index dissoc commit-id)))))))

;; KIZUKU: the desktop app's local backend does not replay design-token
;; operations: their rules live in PenPot's own TokensLib type. So with
;; every commit that changes the token library, the library as it now
;; stands goes along as a closing :set-tokens-lib change, and the backend
;; keeps that snapshot. Without it nothing of the tokens was stored, and
;; they were gone when the file was opened again.

(def ^:private token-change-types
  #{:set-tokens-lib
    :set-token
    :set-token-set
    :set-token-theme
    :update-active-token-themes
    :rename-token-set-group
    :move-token-set
    :move-token-set-group})

(defn- token-changes?
  [changes]
  (some #(contains? token-change-types (:type %)) changes))

(defn- take-tokens-lib-snapshot
  "The commit, with the file's token library as it stands now put on it,
  when the commit changes tokens.

  It is taken when the commit is queued, not when it is sent. A commit
  that waits behind another request is sent later, and by then the
  workspace can have been left (back to the dashboard, another file) and
  the file's data removed from the state: the library read at that point
  was nil, which told the backend that the file has no library any more,
  and it deleted every token of the file.

  With no data of the file in the state nothing is taken, and the backend
  keeps the library it has: a missing library never stands for \"no
  library\"."
  [commit state]
  (let [fdata (when (token-changes? (:changes commit))
                (dsh/lookup-file-data state (:file-id commit)))]
    (cond-> commit
      (some? fdata)
      (assoc ::tokens-lib-snapshot {:tokens-lib (:tokens-lib fdata)}))))

(defn- with-tokens-lib-snapshot
  "The changes of a commit, closed by the token library that was taken for
  it, when one was taken."
  [changes snapshot]
  (cond-> (vec changes)
    (some? snapshot)
    (conj {:type :set-tokens-lib
           :tokens-lib (:tokens-lib snapshot)})))

(defn- append-commit
  "Event used internally to append the current change to the
  persistence queue."
  [{:keys [id] :as commit}]
  (let [run-id (uuid/next)]
    (ptk/reify ::append-commit
      ptk/UpdateEvent
      (update [_ state]
        (log/trc :hint "append-commit" :method "update" :commit-id (dm/str id))
        ;; KIZUKU: the token library goes onto the commit here, when it is
        ;; queued (see take-tokens-lib-snapshot).
        (let [commit (take-tokens-lib-snapshot commit state)]
          (update state :persistence
                  (fn [pstate]
                    (-> pstate
                        (update :run-id d/nilv run-id)
                        (update :queue queue-conj id)
                        (update :index assoc id commit))))))

      ptk/WatchEvent
      (watch [_ state _]
        (let [pstate (:persistence state)]
          (when (= run-id (:run-id pstate))
            (rx/of (run-persistence-task)
                   (update-status :saving))))))))

(defn- discard-persistence-state
  []
  (ptk/reify ::discard-persistence-state
    ptk/UpdateEvent
    (update [_ state]
      (dissoc state :persistence))))

(defn- persist-commit
  [commit-id]
  (ptk/reify ::persist-commit
    ptk/WatchEvent
    (watch [_ state _]
      (log/dbg :hint "persist-commit" :commit-id (dm/str commit-id))
      (when-let [{:keys [file-id file-revn file-vern changes features] :as commit} (dm/get-in state [:persistence :index commit-id])]
        (let [sid      (:session-id state)
              revn     (max file-revn (get @revn-data file-id 0))
              params   {:id file-id
                        :revn revn
                        :vern file-vern
                        :session-id sid
                        :origin (:origin commit)
                        :created-at (:created-at commit)
                        :commit-id commit-id
                        :changes (with-tokens-lib-snapshot changes (::tokens-lib-snapshot commit))
                        :features features}
              permissions (:permissions state)]

          ;; Prevent commit changes by a team member without edition permission
          (when (:can-edit permissions)
            (->> (rp/cmd! :update-file params)
                 (rx/mapcat (fn [{:keys [revn lagged] :as response}]
                              (log/debug :hint "changes persisted" :commit-id (dm/str commit-id) :lagged (count lagged))
                              (rx/of (ptk/data-event ::commit-persisted commit)
                                     (update-file-revn file-id revn))))

                 (rx/catch (fn [cause]
                             (rx/concat
                              (if (= :authentication (:type cause))
                                (rx/empty)
                                (rx/of (ptk/data-event ::error cause)
                                       (update-status :error)))
                              (rx/of (discard-persistence-state))
                              (rx/throw cause)))))))))))


(defn- run-persistence-task
  []
  (ptk/reify ::run-persistence-task
    ptk/WatchEvent
    (watch [_ state stream]
      (let [queue (-> state :persistence :queue)]
        (if-let [commit-id (peek queue)]
          (let [stoper-s (rx/merge
                          (rx/filter (ptk/type? ::run-persistence-task) stream)
                          (rx/filter (ptk/type? ::error) stream))]

            (log/dbg :hint "run-persistence-task" :commit-id (dm/str commit-id))
            (->> (rx/merge
                  (rx/of (persist-commit commit-id))
                  (->> stream
                       (rx/filter (ptk/type? ::commit-persisted))
                       (rx/map deref)
                       (rx/filter #(= commit-id (:id %)))
                       (rx/take 1)
                       (rx/mapcat (fn [_]
                                    (rx/of (discard-commit commit-id)
                                           (run-persistence-task))))))
                 (rx/take-until stoper-s)))
          (rx/of (update-status :saved)))))))

(def ^:private xf-mapcat-undo
  (mapcat :undo-changes))

(def ^:private xf-mapcat-redo
  (mapcat :redo-changes))

(defn- merge-commit
  [buffer]
  (->> (rx/from (group-by :file-id buffer))
       (rx/map (fn [[_ [item :as commits]]]
                 (let [uchg (into [] xf-mapcat-undo commits)
                       rchg (into [] xf-mapcat-redo commits)]
                   (-> item
                       (assoc :undo-changes uchg)
                       (assoc :redo-changes rchg)
                       (assoc :changes rchg)))))))

(defn initialize-persistence
  []
  (ptk/reify ::initialize-persistence
    ptk/WatchEvent
    (watch [_ _ stream]
      (log/debug :hint "initialize persistence")
      (let [stoper-s (rx/filter (ptk/type? ::initialize-persistence) stream)

            local-commits-s
            (->> stream
                 (rx/filter dch/commit?)
                 (rx/map deref)
                 (rx/filter #(= :local (:source %)))
                 (rx/filter (complement empty?))
                 (rx/share))

            notifier-s
            (rx/merge
             (->> local-commits-s
                  (rx/debounce 3000)
                  (rx/tap #(log/trc :hint "persistence beat")))
             (->> stream
                  (rx/filter #(= % ::force-persist))))]

        (rx/merge
         (->> notifier-s
              (rx/map #(ptk/data-event ::persistence-notification)))

         (->> local-commits-s
              (rx/debounce 200)
              (rx/map (fn [_]
                        (update-status :pending)))
              (rx/take-until stoper-s))

         ;; Here we watch for local commits, buffer them in a small
         ;; chunks (very near in time commits) and append them to the
         ;; persistence queue
         (->> local-commits-s
              (rx/buffer-until notifier-s)
              (rx/mapcat merge-commit)
              (rx/map append-commit)
              (rx/take-until (rx/delay 100 stoper-s))
              (rx/finalize (fn []
                             (log/debug :hint "finalize persistence: changes watcher"))))

         ;; Here we track all incoming remote commits for maintain
         ;; updated the local state with the file revn
         (->> stream
              (rx/filter dch/commit?)
              (rx/map deref)
              (rx/filter #(= :remote (:source %)))
              (rx/mapcat (fn [{:keys [file-id file-revn] :as commit}]
                           (rx/of (update-file-revn file-id file-revn))))
              (rx/take-until stoper-s)))))))
