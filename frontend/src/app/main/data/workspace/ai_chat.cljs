;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ADDED BY KIZUKU (2026-10-22): reactive state for the AI chat panel.
;;
;; Deliberately separate from ai_tools.cljs's IPC ownership — the
;; window.electronAPI.ai.onToolCall/onTurnStatus bridge only supports one
;; registered listener at a time (ipcRenderer.removeAllListeners is called
;; before each (re)registration — see src/preload.js), and ai_tools.cljs's
;; init! is already that sole, idempotently-guarded owner, wrapping every
;; AI turn in a single undo transaction. Rather than fight that for a
;; second listener, this namespace is just app-state: ai_tools.cljs calls
;; into the events below from the same handlers it already has, and the
;; chat panel component only ever reads a reactive lens over that state —
;; the same pattern app.main.ui.workspace.sidebar.history uses for the
;; undo-history panel (a lens over :workspace-undo via okulary).
;;
;; Two parallel logs are kept, never conflated:
;; - :display-log — human-readable entries for rendering the conversation.
;; - :provider-history — the opaque, provider-shaped array
;;   ai-provider-client.js's runTurn returns verbatim (Anthropic content
;;   blocks / OpenAI tool_calls messages / Google's stateful markers,
;;   depending on the active provider) — replayed as-is on the next turn,
;;   never parsed or derived from the display log.

(ns app.main.data.workspace.ai-chat
  (:require
   [app.common.time :as ct]
   [app.main.store :as st]
   [okulary.core :as l]
   [potok.v2.core :as ptk]))

(def ^:private default-state
  {:open? false
   :display-log []
   :provider-history []
   :status nil   ; nil | "sending" | "error" | "done"
   :error nil
   :loaded? false})

(def chat-lens
  "Reactive lens over the chat panel's state — mf/deref this in the UI
  component instead of local-only mf/use-state, so the same state is
  shared with ai_tools.cljs's turn-status/tool-call handlers."
  (l/derived #(get % :ai-chat default-state) st/state))

(defn open-panel
  []
  (ptk/reify ::open-panel
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:ai-chat :open?] true))))

(defn close-panel
  []
  (ptk/reify ::close-panel
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:ai-chat :open?] false))))

(defn toggle-panel
  []
  (ptk/reify ::toggle-panel
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:ai-chat :open?] not))))

(defn set-history
  "Replace both logs wholesale — used only when loading persisted history
  on mount (a real snapshot of both, read back from disk together)."
  [{:keys [display-log provider-history]}]
  (ptk/reify ::set-history
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:ai-chat :display-log] (or display-log []))
          (assoc-in [:ai-chat :provider-history] (or provider-history []))
          (assoc-in [:ai-chat :loaded?] true)))))

(defn set-provider-history
  "Replace ONLY the opaque provider-history after a turn completes — never
  display-log, which is built incrementally via append-display-entry calls
  from both this component and ai-tools.cljs's handlers as the turn
  progresses. provider-history is always a full replacement, per
  ai-provider-client.js's contract: result.history is the complete
  accumulated conversation, not a delta."
  [provider-history]
  (ptk/reify ::set-provider-history
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:ai-chat :provider-history] (or provider-history [])))))

(defn reset-history
  []
  (ptk/reify ::reset-history
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:ai-chat :display-log] [])
          (assoc-in [:ai-chat :provider-history] [])
          (assoc-in [:ai-chat :status] nil)
          (assoc-in [:ai-chat :error] nil)))))

(defn append-display-entry
  "Append one entry to the display log. entry is a plain map, e.g.
  {:role :user :text \"...\"} or {:role :tool-call :tool-name \"...\"}."
  [entry]
  (ptk/reify ::append-display-entry
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:ai-chat :display-log] (fnil conj []) (assoc entry :ts (ct/now))))))

(defn set-status
  "Update the panel's turn-lifecycle status. phase mirrors
  ai-provider-client.js's onStatus pushes: started | tool-executing |
  text | done | error. An error phase also appends an :error entry to
  display-log — rendered as its own bordered bubble in the conversation
  (not a separate floating banner disconnected from the message flow),
  and persisted with the rest of the log so a failed turn stays visible
  on reload."
  [{:keys [phase message]}]
  (ptk/reify ::set-status
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        :always (assoc-in [:ai-chat :status] phase)
        (= phase "error") (assoc-in [:ai-chat :error] message)
        (not= phase "error") (assoc-in [:ai-chat :error] nil)
        (= phase "error") (update-in [:ai-chat :display-log] (fnil conj [])
                                     {:role :error :text message :ts (ct/now)})))))
