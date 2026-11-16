;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; KIZUKU (2026-10-25): Paste from Figma.
;;
;; Layers copied in Figma arrive as HTML whose data-buffer attribute is the
;; design itself — the same fig-kiwi binary as a .fig file, base64-encoded.
;; The desktop app (services/figma/figma-clipboard.js) runs it through the
;; .fig importer and hands back PenPot's own :copied-shapes payload, which
;; the regular cross-file paste then places, uploading images and re-keying
;; ids exactly as a paste between two PenPot files would.
;;
;; KIZUKU (2026-11-16): the app's busy pill ("figma-paste") shows from the
;; moment the clipboard is recognised until the shapes are on the canvas,
;; images uploaded included.

(ns app.main.data.workspace.figma-paste
  (:require
   [app.common.data :as d]
   [app.common.logging :as log]
   [app.common.transit :as t]
   [app.main.data.notifications :as ntf]
   [app.util.i18n :refer [tr]]
   [app.util.kizuku-busy :as busy]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(def ^:private buffer-re
  ;; Figma closes the payload with "(/figma)" before the comment end; the
  ;; closing marker is optional so both spellings are read.
  #"data-buffer=\"<!--\(figma\)([A-Za-z0-9+/=]+)(?:\(/figma\))?-->\"")

(defn figma-html?
  "Did Figma put this text/html on the clipboard?"
  [html]
  (and (string? html) (some? (re-find buffer-re html))))

(defn- figma-api
  []
  (some-> (unchecked-get js/globalThis "electronAPI")
          (unchecked-get "figmaAPI")))

(defn- decode-payload
  "The main process encodes the payload with the same Transit writer the
  local backend uses for files; sets come back as vectors.

  :selected keeps its order (an ordered set): the paste stacks the layers
  in that order, and a plain set loses it from the ninth layer on, so a
  background could land on top of what it was behind."
  [result]
  (-> (t/decode-str (unchecked-get result "transit"))
      (update :selected #(into (d/ordered-set) %))
      (update :images set)
      (update :features set)))

(defn- success-message
  [stats]
  (let [layers  (unchecked-get stats "layers")
        missing (unchecked-get stats "missingImages")]
    (if (pos? missing)
      (tr "kizuku.figma.pasted-missing-images" layers missing)
      (tr "kizuku.figma.pasted" layers))))

(defn- busy-opts
  []
  {:title (tr "kizuku.busy.figma-paste")})

(defn- while-busy
  "The paste event, run with the pill showing. The conversion in the main
  process is over by now, but the paste still uploads the images and then
  builds the shapes: the pill stays until the stream of the event ends,
  that is, until the shapes are on the canvas or the paste has failed."
  [event]
  (ptk/reify ::paste-figma-shapes
    ptk/WatchEvent
    (watch [_ state stream]
      (->> (rx/of nil)
           (rx/mapcat (fn [_]
                        (or (ptk/watch event state stream) (rx/empty))))
           (busy/with-busy "figma-paste" (busy-opts))))))

(defn paste-figma
  "Observable of the events that paste Figma clipboard HTML: the shapes
  through `->paste-event` (PenPot's cross-file paste) and then a toast.
  Failures become a toast, never an unhandled error. The busy pill shows
  while the main process converts the clipboard, and goes on showing (see
  `while-busy`) while the shapes are put together."
  [html features in-viewport? ->paste-event]
  (let [api (figma-api)]
    (if (nil? api)
      (rx/of (ntf/error (tr "kizuku.figma.paste-unavailable")))
      (->> (rx/of nil)
           (rx/mapcat
            (fn [_]
              (rx/from (.pasteClipboard ^js api (second (re-find buffer-re html))))))
           (rx/mapcat
            (fn [result]
              (let [stats (unchecked-get result "stats")]
                (if (zero? (unchecked-get stats "layers"))
                  (rx/of (ntf/warn (tr "kizuku.figma.nothing")))
                  (rx/of (while-busy
                          (->paste-event (-> (decode-payload result)
                                             (assoc :features features
                                                    :in-viewport in-viewport?))))
                         (ntf/success (success-message stats)))))))
           (rx/catch
            (fn [cause]
              (log/error :hint "paste from figma failed" :cause cause)
              (rx/of (ntf/error (tr "kizuku.figma.paste-failed")))))
           (busy/with-busy "figma-paste" (busy-opts))))))
