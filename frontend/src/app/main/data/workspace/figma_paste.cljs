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

(ns app.main.data.workspace.figma-paste
  (:require
   [app.common.logging :as log]
   [app.common.transit :as t]
   [app.main.data.notifications :as ntf]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]))

(def ^:private buffer-re
  #"data-buffer=\"<!--\(figma\)([A-Za-z0-9+/=]+)-->\"")

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
  local backend uses for files; sets come back as vectors."
  [result]
  (-> (t/decode-str (unchecked-get result "transit"))
      (update :selected set)
      (update :images set)
      (update :features set)))

(defn- success-message
  [stats]
  (let [layers  (unchecked-get stats "layers")
        missing (unchecked-get stats "missingImages")]
    (if (pos? missing)
      (tr "kizuku.figma.pasted-missing-images" layers missing)
      (tr "kizuku.figma.pasted" layers))))

(defn paste-figma
  "Observable of the events that paste Figma clipboard HTML: the shapes
  through `->paste-event` (PenPot's cross-file paste) and then a toast.
  Failures become a toast, never an unhandled error."
  [html features in-viewport? ->paste-event]
  (let [b64 (second (re-find buffer-re html))
        api (figma-api)]
    (if (nil? api)
      (rx/of (ntf/error (tr "kizuku.figma.paste-unavailable")))
      (->> (rx/from (.pasteClipboard ^js api b64))
           (rx/mapcat
            (fn [result]
              (let [stats (unchecked-get result "stats")]
                (if (zero? (unchecked-get stats "layers"))
                  (rx/of (ntf/warn (tr "kizuku.figma.nothing")))
                  (rx/of (->paste-event (-> (decode-payload result)
                                            (assoc :features features
                                                   :in-viewport in-viewport?)))
                         (ntf/success (success-message stats)))))))
           (rx/catch
            (fn [cause]
              (log/error :hint "paste from figma failed" :cause cause)
              (rx/of (ntf/error (tr "kizuku.figma.paste-failed")))))))))
