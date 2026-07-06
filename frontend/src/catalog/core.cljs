;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Kata 型 — Kizuku component catalog — Entry Point
;; Mounts the catalog app into #catalog-app.

(ns catalog.core
  (:require
   [catalog.layout :as layout]
   [rumext.v2 :as mf]))

(defonce app-root
  (let [element (js/document.getElementById "catalog-app")]
    (mf/create-root element)))

(defn ^:export init
  "Initialize and mount the catalog application."
  []
  (mf/render! app-root (mf/element layout/catalog-app)))
