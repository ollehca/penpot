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
   [app.common.files.helpers :as cfh]
   [app.common.logging :as log]
   [app.common.uuid :as uuid]
   [app.main.data.exports.assets :as de]
   [app.main.data.helpers :as dsh]
   [app.main.data.persistence :as dps]
   [app.main.data.profile :as du]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.ai-chat :as ai-chat]
   [app.main.data.workspace.table-import :as table-import]
   [app.main.store :as st]))

(log/set-level! :warn)

(defn- menu-api
  []
  (when (exists? js/window)
    (some-> (unchecked-get js/window "electronAPI")
            (unchecked-get "menu"))))

(defn- root-frames
  "The boards of the current page, top layer first."
  []
  (let [objects (dsh/lookup-page-objects @st/state)]
    (->> (cfh/get-immediate-children objects uuid/zero)
         (filter cfh/frame-shape?)
         (remove :hidden)
         (reverse)
         (vec))))

(def ^:private extra-commands
  "Menu-only actions that have no keyboard shortcut of their own."
  {:export-frames-pdf      #(st/emit! (de/show-workspace-export-frames-dialog (root-frames)))
   :toggle-ai-chat         #(st/emit! (ai-chat/toggle-panel))
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

(def ^:private flush-settle
  "How long flush-persistence! waits for an edit that is still on its way,
  in ms. A shape is committed about 120 ms after the mouse is released, so
  a flush asked for right at mouse-up would otherwise find nothing to send."
  300)

(def ^:private flush-timeout
  "How long flush-persistence! waits for the queue to drain, in ms."
  4000)

(defn- persistence-idle?
  "True when no edit is queued for, or on its way to, the backend."
  [state]
  (let [pstate (:persistence state)]
    (and (empty? (:queue pstate))
         (not= :saving (:status pstate)))))

(defn- commit-text-being-edited!
  "Text that is still being typed lives in the text editor and only
  becomes part of the file when the editor loses focus. Take the focus
  away so it is committed, as clicking outside the text would."
  []
  (when-let [el (some-> js/document .-activeElement)]
    (when (.-isContentEditable ^js el)
      (.blur ^js el))))

(defn flush-persistence!
  "Send the edits the editor is still holding to the backend now. Returns
  a promise of true once none are queued or in flight, or of false when
  they have not drained in time.

  The editor batches edits and only sends them after three idle seconds
  (app.main.data.persistence). The shell reads the backend's copy when it
  saves a .kiz and tears the page down on quit, close and reload, so it
  calls this first (src/services/editor-flush.js); without it the last
  seconds of work were missing from the saved file."
  []
  (js/Promise.
   (fn [resolve _]
     (let [deadline (+ (js/Date.now) flush-settle flush-timeout)
           ;; The held edits reach the queue over the next turns of the
           ;; event loop, so the queue has to be seen empty twice in a row.
           check    (fn check [idle-before?]
                      (let [idle? (persistence-idle? @st/state)]
                        (cond
                          (and idle? idle-before?)    (resolve true)
                          (> (js/Date.now) deadline) (resolve false)
                          :else (js/setTimeout #(check idle?) 30))))]
       ;; Once for what is already held, and once more after the settle
       ;; time for an edit that had not been committed yet.
       (commit-text-being-edited!)
       (st/emit! ::dps/force-persist)
       (js/setTimeout (fn []
                        (st/emit! ::dps/force-persist)
                        (js/setTimeout #(check false) 30))
                      flush-settle)))))

(defn init!
  "Register the command listener and start publishing context. No-op
  outside the Electron shell."
  []
  ;; The shell changed the UI language (its own Settings window, or the OS
  ;; default) — re-fetch the profile so set-profile re-applies the locale.
  (when-let [api (i18n-api)]
    (.onLocaleChange ^js api (fn [_] (st/emit! (du/refresh-profile)))))
  ;; Tables pushed by the shell (File → Import table from CSV, dropped files).
  (table-import/init!)
  ;; Called by the shell before it saves, quits, closes or reloads.
  (when (exists? js/window)
    (unchecked-set js/window "kizukuFlushPersistence" flush-persistence!))
  (when-let [api (menu-api)]
    (.onCommand ^js api (fn [command] (dispatch! command)))
    (.setContext ^js api (context-snapshot @st/state))
    (add-watch st/state ::menu-context
               (fn [_ _ old new]
                 (when (context-changed? old new)
                   (.setContext ^js api (context-snapshot new)))))))
