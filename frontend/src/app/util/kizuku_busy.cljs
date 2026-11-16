;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ============================================================================
;; ADDED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; New file — the one place that talks to the desktop shell's busy pill
;; (window.kizukuBusy). Licensed under MPL-2.0 like the rest of the frontend.
;; Date: 2026-11-16
;; ============================================================================

(ns app.util.kizuku-busy
  "The desktop shell puts one object, `window.kizukuBusy`, into every page: a
  small pill at the bottom of the window with a spinning ring, a title and,
  optionally, a second line and a progress bar. It is the app's one sign for
  \"busy, please wait\". This namespace is the only code that touches it.

  Every call is safe when the object is missing (a browser, or a page the
  shell has not prepared): it does nothing and says so in its result, and
  the action that asked for the pill goes on without it."
  (:require
   [beicon.v2.core :as rx]))

(def ^:private max-ms
  "A pill removes itself this long after its last show/update, so one whose
  owner never came back cannot stay on the screen for good."
  180000)

(def ^:private paint-fallback-ms
  "A window that is hidden or covered gets no animation frames; the work
  starts after this long anyway, so it is never held up for good."
  120)

(defn- bus
  []
  (when (exists? js/window)
    (let [b (unchecked-get js/window "kizukuBusy")]
      (when (some? b) b))))

(defn available?
  "Is the shell's pill there? Same answer `show!` gives, without showing."
  []
  (some? (bus)))

(defn- ->opts
  [{:keys [title detail percent]}]
  (let [opts (js-obj "maxMs" max-ms)]
    (when (some? title)
      (unchecked-set opts "title" title))
    (when (some? detail)
      (unchecked-set opts "detail" detail))
    (when (some? percent)
      (unchecked-set opts "percent" percent))
    opts))

(defn- call
  "Call `method` of the shell's object. The result is false only when there
  is no object: the shell's functions do not throw, and if one ever did the
  action that asked must still go on."
  [method args]
  (if-let [b (bus)]
    (do (try
          (.apply ^js (unchecked-get b method) b (to-array args))
          (catch :default _ nil))
        true)
    false))

(defn show!
  "Show the pill `id`, or change it when it is already there. `opts` is a map
  with :title, and optionally :detail (second line) and :percent (0-100).
  Returns false when the shell has no pill, true otherwise."
  [id opts]
  (call "show" [id (->opts opts)]))

(defn update!
  "Change the given fields of the pill `id`; nothing happens when it is not
  showing. Returns false when the shell has no pill, true otherwise."
  [id opts]
  (call "update" [id (->opts opts)]))

(defn hide!
  "Remove the pill `id` (the shell keeps it for a short minimum time first,
  so a fast operation does not flash). Returns false when the shell has no
  pill, true otherwise."
  [id]
  (call "hide" [id]))

(defonce ^:private holders
  ;; How many streams hold each pill: it goes away with the last one.
  (atom {}))

(defn- acquire!
  "Take a hold on the pill and show it. True when the pill is there and
  nobody held it yet, that is, when it still has to be drawn."
  [id opts]
  (let [[before _] (swap-vals! holders update id (fnil inc 0))]
    (and (show! id opts) (not (pos? (get before id 0))))))

(defn- release!
  [id]
  (let [[before after] (swap-vals! holders
                                   (fn [m]
                                     (let [n (dec (get m id 0))]
                                       (if (pos? n) (assoc m id n) (dissoc m id)))))]
    (when (and (pos? (get before id 0)) (not (contains? after id)))
      (hide! id))))

(defn next-paint
  "Observable that emits once and ends after the browser has had the chance
  to draw what was changed so far (two animation frames; a timer stands in
  when the window gets none). Work that blocks the page's thread must start
  after this, or the pill would only be drawn once the work is over."
  []
  (rx/create
   (fn [subs]
     (let [done?    (volatile! false)
           frame-id (volatile! nil)
           timer-id (volatile! nil)
           stop     (fn []
                      (js/clearTimeout @timer-id)
                      (when (and (some? @frame-id) (exists? js/cancelAnimationFrame))
                        (js/cancelAnimationFrame @frame-id)))
           fire     (fn []
                      (when-not @done?
                        (vreset! done? true)
                        (stop)
                        (rx/push! subs true)
                        (rx/end! subs)))]
       (vreset! timer-id (js/setTimeout fire paint-fallback-ms))
       (when (exists? js/requestAnimationFrame)
         (vreset! frame-id
                  (js/requestAnimationFrame
                   (fn []
                     (vreset! frame-id (js/requestAnimationFrame fire))))))
       (fn []
         (vreset! done? true)
         (stop))))))

(defn with-busy
  "The observable `stream`, with the pill `id` showing from the moment it is
  subscribed to until it ends, whatever way: completion, error or unsubscribe.
  `opts` as in `show!`. The stream starts after the pill has been drawn
  (at once when another stream already holds the pill: it is drawn).
  Several streams may share one pill: it is removed with the last of them.
  Without the shell's pill the stream simply runs.

  Beware: the stream is subscribed to after the paint wait, not at once.
  Whatever it reads from the page's live state (the files of a file input
  or of a drop, for one, which the browser empties as soon as the handler
  returns) must be copied before the stream is built, not read inside it."
  [id opts stream]
  (->> (rx/create
        (fn [subs]
          (let [draw? (acquire! id opts)]
            (.subscribe ^js (if draw?
                              (->> (next-paint)
                                   (rx/mapcat (fn [_] stream)))
                              stream)
                        subs))))
       (rx/finalize #(release! id))))
