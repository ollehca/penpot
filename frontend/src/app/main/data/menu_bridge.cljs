;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ============================================================================
;; ADDED BY KIZUKU (https://github.com/ollehca/Kizuku)
;; New file — bridge between the native Electron application menu and the
;; app's own actions. Licensed under MPL-2.0 like the rest of the frontend.
;; Date: 2026-10-24
;; ============================================================================

(ns app.main.data.menu-bridge
  "Native menu → PenPot action bridge for the desktop app.

  The Electron menu (src/menu-builder.js) sends a command name over
  window.electronAPI.menu whenever a menu item is clicked with the mouse.
  Commands are resolved against the *currently active* shortcut set — the
  last entry pushed by app.main.data.shortcuts/push-shortcuts, i.e.
  workspace shortcuts in the workspace, dashboard shortcuts on the
  dashboard, path-editing shortcuts while editing a path — so a menu click
  behaves exactly like pressing that item's keyboard shortcut would in the
  same context. A few menu-only actions with no shortcut live in
  `extra-commands`.

  In the other direction the bridge publishes a small context snapshot
  (current route + workspace layout flags) so the native menu can enable
  workspace-only items and tick the checkbox toggles."
  (:require
   [app.common.logging :as log]
   [app.main.data.profile :as du]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.ai-chat :as ai-chat]
   [app.main.store :as st]))

(log/set-level! :warn)

(defn- menu-api
  []
  (when (exists? js/window)
    (some-> (unchecked-get js/window "electronAPI")
            (unchecked-get "menu"))))

(def ^:private extra-commands
  "Menu-only actions that have no keyboard shortcut of their own."
  {:toggle-ai-chat         #(st/emit! (ai-chat/toggle-panel))
   :scale-text             #(st/emit! (dw/toggle-layout-flag :scale-text))
   :display-artboard-names #(st/emit! (dw/toggle-layout-flag :display-artboard-names))})

(defn active-shortcuts
  "The shortcut set currently bound to the keyboard (see push-shortcuts)."
  [state]
  (second (last (:shortcuts state))))

(defn resolve-command
  "The function to run for a menu command in the given state, or nil."
  [state command]
  (let [command (keyword command)]
    (or (:fn (get (active-shortcuts state) command))
        (get extra-commands command))))

(defn dispatch!
  [command]
  (if-let [handler (resolve-command @st/state command)]
    (handler #js {})
    (log/warn :hint "native menu command has no handler in this context"
              :command command)))

(defn context-snapshot
  "What the native menu needs to know about the app: whether a file is
  open (workspace route) and which layout toggles are on."
  [state]
  #js {:workspace (= :workspace (get-in state [:route :data :name]))
       :layout (into-array (map name (:workspace-layout state)))})

(defn- context-changed?
  [old new]
  (or (not= (get-in old [:route :data :name])
            (get-in new [:route :data :name]))
      (not= (:workspace-layout old) (:workspace-layout new))))

(defn- i18n-api
  []
  (when (exists? js/window)
    (some-> (unchecked-get js/window "electronAPI")
            (unchecked-get "i18n"))))

(defn init!
  "Register the command listener and start publishing context. No-op
  outside the Electron shell."
  []
  ;; The shell changed the UI language (its own Settings window, or the OS
  ;; default) — re-fetch the profile so set-profile re-applies the locale.
  (when-let [api (i18n-api)]
    (.onLocaleChange ^js api (fn [_] (st/emit! (du/refresh-profile)))))
  (when-let [api (menu-api)]
    (.onCommand ^js api (fn [command] (dispatch! command)))
    (.setContext ^js api (context-snapshot @st/state))
    (add-watch st/state ::menu-context
               (fn [_ _ old new]
                 (when (context-changed? old new)
                   (.setContext ^js api (context-snapshot new)))))))
