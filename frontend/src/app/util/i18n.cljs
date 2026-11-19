;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

;; ============================================================================
;; MODIFIED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; Original file from PenPot (https://github.com/penpot/penpot)
;; Licensed under Mozilla Public License Version 2.0
;; Modifications: The names in the language list (supported-locales) no
;;   longer end in "(community)"; 29 of the 32 entries did.
;; Date: 2026-11-20
;; ============================================================================

(ns app.util.i18n
  "A i18n foundation."
  (:require
   [app.common.data :as d]
   [app.common.logging :as log]
   [app.common.time :as ct]
   [app.config :as cfg]
   [app.util.globals :as globals]
   [app.util.storage :as storage]
   [cuerdas.core :as str]
   [goog.object :as gobj]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(log/set-level! :info)

(def supported-locales
  [{:label "English" :value "en"}
   {:label "Español" :value "es"}
   {:label "Català" :value "ca"}
   {:label "Deutsch" :value "de"}
   {:label "Dutch" :value "nl"}
   {:label "Euskera" :value "eu"}
   {:label "Français" :value "fr"}
   {:label "Gallego" :value "gl"}
   {:label "Hausa" :value "ha"}
   {:label "Hrvatski" :value "hr"}
   {:label "Italiano" :value "it"}
   {:label "Norsk - Bokmål" :value "nb_no"}
   {:label "Polski" :value "pl"}
   {:label "Portuguese - Brazil" :value "pt_br"}
   {:label "Portuguese - Portugal" :value "pt_pt"}
   {:label "Bahasa Indonesia" :value "id"}
   {:label "Rumanian" :value "ro"}
   {:label "Türkçe" :value "tr"}
   {:label "Ελληνική γλώσσα" :value "el"}
   {:label "Русский" :value "ru"}
   {:label "Украї́нська мо́ва" :value "uk"}
   {:label "Český jazyk" :value "cs"}
   {:label "Latviešu valoda" :value "lv"}
   {:label "Српски" :value "sr"}
   {:label "Føroyskt mál" :value "fo"}
   {:label "Korean" :value "ko"}
   {:label "עִבְרִית" :value "he"}
   {:label "عربي/عربى" :value "ar"}
   {:label "فارسی" :value "fa"}
   {:label "日本語" :value "ja_jp"}
   {:label "简体中文" :value "zh_cn"}
   {:label "繁體中文" :value "zh_hant"}])

(defn- parse-locale
  [locale]
  (let [locale (-> (str/lower locale)
                   (str/replace "-" "_"))]
    (cond-> [locale]
      (str/includes? locale "_")
      (conj (subs locale 0 2)))))

(def ^:private browser-locales
  (delay
    (-> (.-language globals/navigator)
        (parse-locale))))

(defn- autodetect
  []
  (let [supported (into #{} (map :value supported-locales))]
    (loop [locales (seq @browser-locales)]
      (if-let [locale (first locales)]
        (if (contains? supported locale)
          locale
          (recur (rest locales)))
        cfg/default-language))))

(defonce translations #js {})
(defonce locale (l/atom nil))

(add-watch locale "common.time"
           (fn [_ _ pv cv]
             (when (not= pv cv)
               (ct/set-default-locale! cv))))

(defn init!
  "Initialize the i18n module with translations.

  The `data` is a javascript object for performance reasons. This code
  is executed in the critical part (application bootstrap) and used in
  many parts of the application."
  [data]
  (reset! locale (or (get storage/global ::locale) (autodetect)))
  (set! translations data))

(defn set-locale!
  [lname]
  (if (or (nil? lname)
          (str/empty? lname))
    (let [lname (autodetect)]
      (swap! storage/global dissoc ::locale)
      (reset! locale lname))
    (let [supported (into #{} (map :value) supported-locales)
          lname     (loop [locales (seq (parse-locale lname))]
                      (if-let [locale (first locales)]
                        (if (contains? supported locale)
                          locale
                          (recur (rest locales)))
                        cfg/default-language))]
      (swap! storage/global assoc ::locale lname)
      (reset! locale lname))))

(deftype C [val]
  IDeref
  (-deref [_] val))

(defn ^boolean c?
  [r]
  (instance? C r))

;; A main public api for translate strings.

;; A marker type that is used just for mark
;; a parameter that represented the counter.

(defn c
  [x]
  (C. x))

(defn empty-string?
  [v]
  (or (nil? v) (empty? v)))

(defn t
  ([locale code]
   (let [code  (name code)
         value (gobj/getValueByKeys translations code locale)]
     (if (empty-string? value)
       (if (= cfg/default-language locale)
         code
         (t cfg/default-language code))
       (if (array? value)
         (aget value 0)
         value))))
  ([locale code & args]
   (let [code   (name code)
         value  (gobj/getValueByKeys translations code locale)]
     (if (empty-string? value)
       (if (= cfg/default-language locale)
         code
         (apply t cfg/default-language code args))
       (let [plural (first (filter c? args))
             value  (if (array? value)
                      (if (= @plural 1) (aget value 0) (aget value 1))
                      value)]
         (apply str/fmt value (map #(if (c? %) @% %) args)))))))

(defn tr
  ([code] (t @locale code))
  ([code & args] (apply t @locale code args)))

(mf/defc tr-html*
  {::mf/props :obj}
  [{:keys [content class tag-name on-click]}]
  (let [tag-name (d/nilv tag-name "p")]
    [:> tag-name {:dangerouslySetInnerHTML #js {:__html content}
                  :className class
                  :on-click on-click}]))

;; DEPRECATED
(defn use-locale
  []
  (mf/deref locale))

