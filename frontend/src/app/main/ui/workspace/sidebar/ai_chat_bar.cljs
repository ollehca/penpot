;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ADDED BY KIZUKU (2026-10-21, rebuilt 2026-10-22): the real AI chat
;; panel — a floating FAB + resizable overlay with a scrollable
;; conversation (not a single-shot input), live turn-status, provider and
;; model pickers, per-file persistence, and keyboard/ARIA support.
;;
;; State lives in app.main.data.workspace.ai-chat, fed by
;; app.main.data.workspace.ai-tools's onToolCall/onTurnStatus handlers
;; (the IPC bridge only supports one registered listener at a time — see
;; ai-tools.cljs's own header comment — so this component never registers
;; its own; it only reads the shared reactive lens).
;;
;; The file reads top to bottom: the desktop bridge and labels, the panel
;; size hook, the provider-settings hook, persisted history, the turn's
;; send/reply plumbing, the sub-components (messages, header with its two
;; pickers, composer, resize handles) and finally ai-chat-bar*, which only
;; wires them together.
;;
;; All user-facing copy goes through tr (kizuku.ai.* keys in the .po
;; catalogues) so the panel follows the UI language like the rest of the app.
;;
;; Mounted as a fixed-position overlay (sibling to modal-container* in
;; workspace.cljs) so it's independent of any sidebar's box model — the
;; original docked-in-sidebar attempt got clipped by the sidebar's own
;; layout/overflow rules depending on selection state.

(ns app.main.ui.workspace.sidebar.ai-chat-bar
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.workspace.ai-chat :as ai-chat]
   [app.main.data.workspace.ai-tools :as ai-tools]
   [app.main.store :as st]
   [app.main.ui.workspace.sidebar.ai-markdown :refer [markdown*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [clojure.string :as str]
   [okulary.core :as l]
   [promesa.core :as p]
   [rumext.v2 :as mf]))

;; --- The desktop bridge -----------------------------------------------------

(defn- electron-api
  []
  (unchecked-get js/window "electronAPI"))

(defn- ai-api
  "The AI side of the desktop bridge (window.electronAPI.ai), nil outside
  the desktop app."
  []
  (some-> (electron-api) (unchecked-get "ai")))

(defn- mock-backend
  "The file-store side of the desktop bridge, where chat history is kept."
  []
  (some-> (electron-api) (unchecked-get "mockBackend")))

;; A plain (:current-file-id @st/state) read at render time is NOT
;; reactive — it only reflects whatever the value was on whichever render
;; pass first evaluated it, and never updates on its own. This component
;; mounts as a sibling very early in workspace.cljs, often before
;; dw/initialize-workspace has set :current-file-id (that event's own
;; mount-effect may not have fired yet) — a bare read like that permanently
;; captured nil, so the "load persisted history on mount" effect, gated on
;; file-id, silently never ran and the whole feature appeared broken. A
;; proper l/derived lens re-renders this component once the real id
;; becomes available, same pattern ai_chat.cljs's own chat-lens uses.
(def ^:private file-id-ref
  (l/derived :current-file-id st/state))

(defn- file-still-open?
  "True while the file a turn was sent from is the one on screen. A turn
  outlives a tab switch, and the conversation in state is then the other
  file's: its result must neither be shown there nor saved under it."
  [file-id]
  (= file-id (some-> (:current-file-id @st/state) str)))

;; --- Labels -----------------------------------------------------------------

(def ^:private known-providers
  [{:id "anthropic" :label "Anthropic"}
   {:id "openai" :label "OpenAI"}
   {:id "google" :label "Google"}])

(defn- provider-label
  [provider-id]
  (:label (first (filter #(= (:id %) provider-id) known-providers)) provider-id))

(defn- model-label
  "The tier label for a model id, from the tier list for its provider —
  falls back to the raw model id (e.g. a free-typed Settings override not
  in the curated list) when no match is found."
  [model-tiers model-id]
  (or (:label (first (filter #(= (:id %) model-id) model-tiers))) model-id))

(defn- short-model-label
  "Trim a tier label down to just its parenthetical qualifier (e.g.
  'fastest'/'balanced'/'most capable') for the compact badge — the full
  model name is still in the dropdown and the aria-label."
  [label]
  (if-let [[_ qualifier] (re-find #"\(([^)]+)\)" (or label ""))]
    qualifier
    label))

(defn- status-label
  [status]
  (case status
    "started" (tr "kizuku.ai.status.thinking")
    "tool-executing" (tr "kizuku.ai.status.editing")
    "text" (tr "kizuku.ai.status.thinking")
    "sending" (tr "kizuku.ai.status.sending")
    nil))

;; --- Panel size -------------------------------------------------------------
;; nil means "auto": the panel sizes itself to its content via CSS
;; (grid-template-rows: auto 1fr auto, capped at max-height: 90vh), so a
;; short conversation stays compact and a long one grows to show more
;; context, without the user having to drag anything. Dragging a resize
;; zone sets an explicit {:width :height}, which from then on overrides
;; the auto behaviour and is remembered in localStorage across files and
;; restarts — standard "auto until the user takes control" sizing.

(def ^:private min-panel-size {:width 300 :height 280})
(def ^:private max-panel-width 640)
(def ^:private panel-size-storage-key "kizuku.ai.panel-size")

(defn- read-saved-panel-size
  "The {:width :height} the user last dragged the panel to, or nil."
  []
  (try
    (when-let [raw (.getItem js/localStorage panel-size-storage-key)]
      (let [parsed (js/JSON.parse raw)
            width  (unchecked-get parsed "width")
            height (unchecked-get parsed "height")]
        (when (and (number? width) (number? height))
          {:width width :height height})))
    (catch :default _ nil)))

(defn- save-panel-size!
  [{:keys [width height]}]
  (try
    (.setItem js/localStorage panel-size-storage-key
              (js/JSON.stringify #js {:width width :height height}))
    (catch :default _ nil)))

(defn- max-panel-height
  "90% of the real app window height — mirrors .panel's own CSS max-height
  (90vh), since the resize drag's clamp runs in JS and can't read a CSS
  custom property. Computed fresh each call rather than cached once, so a
  window resize while the panel happens to be open doesn't leave a stale
  ceiling from whatever size the window was at mount."
  []
  (* (.-innerHeight js/window) 0.9))

(defn- clamp
  [v lo hi]
  (max lo (min hi v)))

(defn- drag-start
  "Where a resize drag begins: the pointer, the axes the grabbed zone
  drives (its data-axes) and the panel's current box — the stored size,
  or in auto mode the box actually rendered, so the drag starts from
  wherever the panel is on screen rather than snapping to a default."
  [event panel-node size]
  (let [axes (or (dom/get-data (dom/get-current-target event) "axes") "xy")
        {:keys [width height]}
        (or size
            (when panel-node
              (let [rect (.getBoundingClientRect ^js panel-node)]
                {:width (.-width rect) :height (.-height rect)})))]
    {:x (.-clientX ^js event)
     :y (.-clientY ^js event)
     :axes axes
     :width width
     :height height}))

(defn- dragged-size
  "The panel size for the pointer's position during a drag. The panel is
  pinned to its bottom-right corner (next to the FAB), so only the top and
  left edges move: pulling the pointer up or left grows the panel, which
  is why the deltas are inverted."
  [{:keys [x y axes width height]} event]
  (let [dx (if (str/includes? axes "x") (- x (.-clientX ^js event)) 0)
        dy (if (str/includes? axes "y") (- y (.-clientY ^js event)) 0)]
    {:width (clamp (+ width dx) (:width min-panel-size) max-panel-width)
     :height (clamp (+ height dy) (:height min-panel-size) (max-panel-height))}))

(defn- use-panel-resize
  "Hook: the panel's size (nil until the user drags) and the pointer-down
  handler shared by the three drag zones. The pointer is captured so the
  drag keeps reporting even when it crosses the canvas, which has pointer
  handlers of its own; the window-level move/up listeners live for the
  component's lifetime and act only while a drag is on."
  [panel-ref]
  (let [size* (mf/use-state (read-saved-panel-size))
        ;; the drag-start map while a drag is on, nil otherwise
        drag* (mf/use-ref nil)
        on-pointer-down
        (mf/use-fn
         (mf/deps @size*)
         (fn [event]
           (dom/prevent-default event)
           (dom/stop-propagation event)
           (dom/capture-pointer event)
           (mf/set-ref-val! drag* (drag-start event (mf/ref-val panel-ref) @size*))))]
    (mf/with-effect []
      (let [on-move (fn [event]
                      (when-let [drag (mf/ref-val drag*)]
                        (reset! size* (dragged-size drag event))))
            on-up   (fn [_]
                      (when (mf/ref-val drag*)
                        (mf/set-ref-val! drag* nil)
                        (some-> @size* save-panel-size!)))]
        (.addEventListener js/window "pointermove" on-move)
        (.addEventListener js/window "pointerup" on-up)
        (fn []
          (.removeEventListener js/window "pointermove" on-move)
          (.removeEventListener js/window "pointerup" on-up))))
    {:size @size*
     :on-pointer-down on-pointer-down}))

;; --- Provider settings ------------------------------------------------------

(defn- fetch-model!
  "Read a provider's current model and its tier list into the two states.
  Model tiers are per provider, so both follow a provider switch."
  [api provider-id model* model-tiers*]
  (-> (.getModel ^js api provider-id)
      (.then (fn [model-id] (reset! model* model-id))))
  (-> (.getModelTiers ^js api provider-id)
      (.then (fn [tiers] (reset! model-tiers* (js->clj tiers :keywordize-keys true))))))

(defn- use-provider-settings
  "Hook: the active provider and model, the model tiers and the key status
  per provider, read from the main process whenever the panel opens (so a
  change made in Settings meanwhile shows), with the handlers of the two
  pickers and a setter for a key status read elsewhere."
  [open?]
  (let [provider*    (mf/use-state nil)
        model*       (mf/use-state nil)
        model-tiers* (mf/use-state [])
        key-status*  (mf/use-state nil)
        set-key-status!
        (mf/use-fn
         (fn [status] (reset! key-status* status)))
        on-select-provider
        (mf/use-fn
         (fn [event]
           (when-let [api (ai-api)]
             (let [provider-id (dom/get-data (dom/get-current-target event) "option-id")]
               (-> (.setActiveProvider ^js api provider-id)
                   (.then (fn [_] (reset! provider* provider-id))))
               (fetch-model! api provider-id model* model-tiers*)
               ;; A provider switch starts a fresh resumption thread — the
               ;; old provider-history is shaped for the old provider
               ;; (Anthropic content blocks / OpenAI tool_calls messages /
               ;; Google's stateful interaction markers) and replaying it
               ;; against a different provider's API would be meaningless.
               ;; display-log (the visible transcript) is left untouched.
               (st/emit! (ai-chat/set-provider-history []))))))
        on-select-model
        (mf/use-fn
         (mf/deps @provider*)
         (fn [event]
           (when-let [api (ai-api)]
             (when-let [provider-id @provider*]
               (let [model-id (dom/get-data (dom/get-current-target event) "option-id")]
                 (-> (.setModel ^js api provider-id model-id)
                     (.then (fn [_] (reset! model* model-id)))))))))]
    (mf/with-effect [open?]
      (when-let [api (and open? (ai-api))]
        (p/let [provider-id (.getActiveProvider ^js api)]
          (reset! provider* provider-id)
          (fetch-model! api provider-id model* model-tiers*))
        (p/let [status (.getKeyStatus ^js api)]
          (reset! key-status* (js->clj status :keywordize-keys true)))))
    {:provider @provider*
     :model @model*
     :model-tiers @model-tiers*
     :key-status @key-status*
     :set-key-status! set-key-status!
     :on-select-provider on-select-provider
     :on-select-model on-select-model}))

;; --- Persisted history ------------------------------------------------------

(defn- load-persisted-history!
  "Put the conversation saved for a file into the panel's state."
  [file-id]
  (when-let [backend (mock-backend)]
    (-> (.command ^js backend "get-ai-chat-history" #js {:fileId file-id})
        (.then
         (fn [res]
           (let [raw (unchecked-get res "raw")
                 ;; js->clj :keywordize-keys only keywordizes map KEYS,
                 ;; never string VALUES — a round-tripped entry's :role
                 ;; lands as the string "user", not the keyword :user that
                 ;; freshly appended entries use, and message-log*'s role
                 ;; filter would drop it. Re-keywordize :role explicitly.
                 display-log      (->> (js->clj (unchecked-get raw "displayLog") :keywordize-keys true)
                                       (mapv #(update % :role keyword)))
                 provider-history (js->clj (unchecked-get raw "providerHistory") :keywordize-keys true)]
             (st/emit! (ai-chat/set-history {:display-log display-log
                                             :provider-history provider-history})))))
        (.catch (fn [err] (js/console.error "Failed to load AI chat history" err))))))

(defn- persist-history!
  "Save the conversation under its file: the display log as it is in state
  right now (ai-tools.cljs appends tool-call entries mid-turn, after the
  sender's closure captured its view of it) and the given provider
  history. Nothing is saved once the file has left the screen: the state
  then holds another file's conversation."
  [file-id provider-history]
  (when-let [backend (and file-id (mock-backend))]
    (if (file-still-open? file-id)
      (.command ^js backend "save-ai-chat-history"
                #js {:fileId file-id
                     :displayLog (clj->js (get-in @st/state [:ai-chat :display-log]))
                     :providerHistory (clj->js provider-history)})
      (js/console.warn "AI chat: the file this turn was sent from is no longer open;"
                       "nothing saved for" file-id))))

(defn- clear-history!
  "Empty the conversation on screen and on disk."
  [file-id]
  (st/emit! (ai-chat/reset-history))
  (when-let [backend (and file-id (mock-backend))]
    (.command ^js backend "save-ai-chat-history"
              #js {:fileId file-id :displayLog #js [] :providerHistory #js []})))

;; --- Sending a prompt -------------------------------------------------------

(defn- settle!
  "Set the panel's status for a finished turn. While the turn's undo entry
  is still being committed (text layout settling) the status push from
  ai-tools.cljs settles the panel itself, once that is done; and a bubble
  for a turn whose file has left the screen would land in another file's
  conversation, so that case only unlocks the panel."
  [file-id status]
  (when-not (ai-tools/turn-commit-pending?)
    (st/emit! (ai-chat/set-status (if (file-still-open? file-id) status {:phase "done"})))))

(defn- on-turn-reply!
  "The IPC reply to a prompt. It is the one signal that always arrives, so
  it always settles the panel: the real-time ai:turn-status pushes normally
  get there first (\"done\" with the reply text, \"error\" with its bubble),
  repeating the phase here is harmless, and set-status drops a duplicate
  error bubble (ai_chat.cljs). What only this reply can report: a failure
  before the turn's first push (no key, unknown provider, a renderer the
  main process could no longer reach) and a push that never arrived — both
  used to leave \"Sending…\" on screen for good."
  [file-id provider-history res]
  (if (unchecked-get res "success")
    (let [new-history (js->clj (unchecked-get res "history") :keywordize-keys true)]
      (settle! file-id {:phase "done"})
      ;; After a tab switch the state holds the other file's conversation:
      ;; leave it alone.
      (when (file-still-open? file-id)
        (st/emit! (ai-chat/set-provider-history new-history)))
      (persist-history! file-id new-history))
    (do
      (settle! file-id {:phase "error"
                        :message (or (not-empty (unchecked-get res "error"))
                                     (tr "kizuku.ai.error.generic"))})
      ;; provider-history is unchanged: the turn produced nothing to resume from.
      (persist-history! file-id provider-history))))

(defn- on-turn-failure!
  "The IPC call itself failed (no handler registered, a payload that could
  not be cloned): nothing else will ever unlock the panel."
  [file-id provider-history err]
  (st/emit! (ai-chat/set-status
             (if (file-still-open? file-id)
               {:phase "error"
                :message (or (some-> err .-message not-empty)
                             (tr "kizuku.ai.error.generic"))}
               {:phase "done"})))
  (persist-history! file-id provider-history))

(defn- send-prompt!
  "Show the prompt, lock the panel and hand the turn to the main process."
  [api file-id provider-id text provider-history]
  (st/emit! (ai-chat/append-display-entry {:role :user :text text}))
  (st/emit! (ai-chat/set-status {:phase "sending"}))
  (-> (.sendPrompt ^js api
                   #js {:provider provider-id
                        :prompt text
                        :systemPrompt (ai-tools/build-system-prompt)
                        :history (clj->js provider-history)})
      (.then (fn [res] (on-turn-reply! file-id provider-history res)))
      (.catch (fn [err] (on-turn-failure! file-id provider-history err)))))

;; --- Sub-components ---------------------------------------------------------

(mf/defc message-entry*
  {::mf/private true}
  [{:keys [entry]}]
  (case (:role entry)
    :user [:div {:class (stl/css :message-row :message-row-user)}
           [:div {:class (stl/css :message-bubble :message-bubble-user)} (:text entry)]]
    ;; Assistant replies are Markdown (lists, bold, code) — rendered as
    ;; hiccup, never as raw HTML, so the model can't inject markup.
    :assistant [:div {:class (stl/css :message-row :message-row-assistant)}
                [:> markdown* {:text (:text entry)
                               :class (stl/css :message-bubble :message-bubble-assistant :message-bubble-markdown)}]]
    :error [:div {:class (stl/css :message-row :message-row-assistant)}
            [:div {:class (stl/css :message-bubble :message-bubble-error) :role "alert"}
             (:text entry)]]
    nil))

(mf/defc message-log*
  {::mf/private true}
  [{:keys [display-log status has-key? provider-id on-open-settings]}]
  (let [log-ref (mf/use-ref nil)
        ;; Tool-call entries (create_rectangle, delete_shape, etc.) are
        ;; kept in display-log for a future detail view, but aren't shown
        ;; here — the live status spinner already signals "working", and
        ;; the assistant's own final text explains what it did in plain
        ;; language, so a chip per raw tool call was just noise. Errors
        ;; render as their own bordered bubble (message-entry*'s :error
        ;; case), in the flow of the conversation, not a separate banner
        ;; disconnected from what prompt caused them.
        visible-entries (filterv #(contains? #{:user :assistant :error} (:role %)) display-log)]
    (mf/with-effect [(count visible-entries) status]
      (when-let [node (mf/ref-val log-ref)]
        (set! (.-scrollTop node) (.-scrollHeight node))))
    [:div {:ref log-ref
           :class (stl/css :message-log)
           :role "log"
           :aria-live "polite"
           :aria-label (tr "kizuku.ai.conversation")}
     (cond
       (seq visible-entries)
       (for [[idx entry] (map-indexed vector visible-entries)]
         [:> message-entry* {:key idx :entry entry}])

       ;; A brand-new user otherwise only learns a key is missing by
       ;; sending a prompt and getting an error bubble back — this nudge
       ;; catches that before they have to fail once to discover it. Only
       ;; shown once has-key? has actually resolved (not nil, i.e. not
       ;; still loading) and come back false, so this never flashes
       ;; incorrectly while the real status is still in flight.
       (false? has-key?)
       [:div {:class (stl/css :message-log-empty :message-log-onboarding)}
        [:p (tr "kizuku.ai.onboarding.title")]
        [:button {:class (stl/css :onboarding-settings-btn)
                  :on-click on-open-settings}
         (tr "kizuku.ai.onboarding.add-key"
             (or (provider-label provider-id) (tr "kizuku.ai.onboarding.your-provider")))]]

       :else
       [:div {:class (stl/css :message-log-empty)}
        (tr "kizuku.ai.empty")])
     (when-let [label (status-label status)]
       [:div {:class (stl/css :status-row) :aria-label label}
        [:div {:class (stl/css :status-spinner)}]
        [:span label]])]))

;; A badge button that opens a listbox. The option clicked reaches
;; on-select through its data-option-id attribute; an option may carry a
;; status (the provider menu shows whether a key is stored).
(mf/defc option-menu*
  {::mf/private true}
  [{:keys [badge aria-label options selected-id open? on-toggle on-select]}]
  [:div {:class (stl/css :provider-picker)}
   [:button {:class (stl/css :provider-badge)
             :on-click on-toggle
             :aria-haspopup "listbox"
             :aria-expanded open?
             :aria-label aria-label}
    badge
    [:span {:class (stl/css :provider-badge-caret)} "▾"]]
   (when open?
     [:ul {:class (stl/css :provider-menu) :role "listbox"}
      (for [{:keys [id label status]} options]
        [:li {:key id
              :role "option"
              :aria-selected (= id selected-id)
              :class (stl/css-case :provider-menu-item true
                                   :provider-menu-item-active (= id selected-id))
              :data-option-id id
              :on-click on-select}
         [:span {:class (stl/css :provider-menu-item-label)} label]
         (when status
           [:span {:class (stl/css-case :provider-menu-item-status true
                                        :has-key (:has-key? status))}
            (:text status)])])])])

(defn- provider-options
  "The provider menu's options, each with its key status."
  [key-status]
  (mapv (fn [{:keys [id label]}]
          (let [has-key? (boolean (get key-status (keyword id)))]
            {:id id
             :label label
             :status {:has-key? has-key?
                      :text (tr (if has-key? "kizuku.ai.key-set" "kizuku.ai.no-key"))}}))
        known-providers))

(mf/defc panel-header*
  {::mf/private true}
  [{:keys [settings open-menu on-toggle-provider-menu on-toggle-model-menu
           on-select-provider on-select-model on-clear on-close]}]
  (let [{:keys [provider model model-tiers key-status]} settings
        model-name (model-label model-tiers model)]
    [:div {:class (stl/css :panel-header)}
     [:span {:class (stl/css :panel-title)} (tr "kizuku.ai.title")]
     [:> option-menu* {:badge (or (provider-label provider) "…")
                       :aria-label (tr "kizuku.ai.provider-aria" (provider-label provider))
                       :options (provider-options key-status)
                       :selected-id provider
                       :open? (= open-menu :provider)
                       :on-toggle on-toggle-provider-menu
                       :on-select on-select-provider}]
     [:> option-menu* {:badge (short-model-label model-name)
                       :aria-label (tr "kizuku.ai.model-aria" model-name)
                       :options model-tiers
                       :selected-id model
                       :open? (= open-menu :model)
                       :on-toggle on-toggle-model-menu
                       :on-select on-select-model}]
     [:button {:class (stl/css :header-button)
               :on-click on-clear
               :aria-label (tr "kizuku.ai.clear-aria")
               :title (tr "kizuku.ai.clear-title")}
      (tr "kizuku.ai.clear")]
     [:button {:class (stl/css :header-button)
               :on-click on-close
               :aria-label (tr "kizuku.ai.close-aria")
               :title (tr "kizuku.ai.close")}
      "✕"]]))

(mf/defc composer*
  {::mf/private true}
  [{:keys [prompt sending? composer-ref on-change on-key-down on-send]}]
  [:div {:class (stl/css :composer)}
   [:textarea
    {:ref composer-ref
     :class (stl/css :composer-input)
     :placeholder (tr "kizuku.ai.placeholder")
     :value prompt
     :disabled sending?
     :on-change on-change
     :on-key-down on-key-down
     :aria-label (tr "kizuku.ai.prompt-aria")
     :rows 2}]
   [:button
    {:class (stl/css :send-button)
     :on-click on-send
     :disabled (or sending? (str/blank? prompt))
     :aria-label (if sending? (tr "kizuku.ai.sending-aria") (tr "kizuku.ai.send-aria"))}
    (if sending? (tr "kizuku.ai.sending") (tr "kizuku.ai.send"))]])

;; The three drag zones: the free edges and their corner. data-axes says
;; which dimensions a zone drives (see dragged-size).
(mf/defc resize-handles*
  {::mf/private true}
  [{:keys [on-pointer-down]}]
  [:*
   [:div {:class (stl/css :resize-left)
          :data-axes "x"
          :on-pointer-down on-pointer-down
          :aria-hidden true}]
   [:div {:class (stl/css :resize-top)
          :data-axes "y"
          :on-pointer-down on-pointer-down
          :aria-hidden true}]
   [:div {:class (stl/css :resize-corner)
          :data-axes "xy"
          :on-pointer-down on-pointer-down
          :aria-hidden true}]])

;; --- The panel --------------------------------------------------------------

(defn- toggle-menu!
  "Open one of the two header pickers, or close it when it is the open one."
  [open-menu* menu]
  (swap! open-menu* #(if (= % menu) nil menu)))

(mf/defc ai-chat-bar*
  {::mf/private true}
  []
  (let [{:keys [open? display-log provider-history status]}
        (mf/deref ai-chat/chat-lens)

        ;; :current-file-id is a real uuid record (app.common.uuid), not a
        ;; plain string — embedding it directly in a #js {...} literal
        ;; doesn't coerce it, so it reached the main process as a CLJS
        ;; object, which JSON.stringify then rendered as the literal
        ;; string "[object Object]" as the persisted-history file's top-
        ;; level key. (str uuid) here, once, makes every downstream :fileId
        ;; already a plain string.
        file-id      (some-> (mf/deref file-id-ref) str)
        sending?     (= status "sending")

        prompt*      (mf/use-state "")
        ;; nil, :provider or :model — which header picker is open
        open-menu*   (mf/use-state nil)
        fab-ref      (mf/use-ref nil)
        composer-ref (mf/use-ref nil)
        panel-ref    (mf/use-ref nil)

        settings     (use-provider-settings open?)
        resize       (use-panel-resize panel-ref)
        {:keys [provider key-status set-key-status!]} settings
        has-key?     (when (and provider key-status)
                       (boolean (get key-status (keyword provider))))

        on-toggle
        (mf/use-fn
         (fn [_] (st/emit! (ai-chat/toggle-panel))))

        on-close
        (mf/use-fn
         (fn [_]
           (st/emit! (ai-chat/close-panel))
           (when-let [node (mf/ref-val fab-ref)]
             (.focus ^js node))))

        on-open-settings
        (mf/use-fn
         (fn [_]
           (when-let [api (electron-api)]
             (.open ^js (.-settings api)))))

        on-change
        (mf/use-fn
         (fn [event]
           (reset! prompt* (dom/get-value (dom/get-target event)))))

        on-clear
        (mf/use-fn
         (mf/deps file-id)
         (fn [_] (clear-history! file-id)))

        on-toggle-provider-menu
        (mf/use-fn
         (fn [_] (toggle-menu! open-menu* :provider)))

        on-toggle-model-menu
        (mf/use-fn
         (fn [_] (toggle-menu! open-menu* :model)))

        on-select-provider
        (mf/use-fn
         (mf/deps (:on-select-provider settings))
         (fn [event]
           (reset! open-menu* nil)
           ((:on-select-provider settings) event)))

        on-select-model
        (mf/use-fn
         (mf/deps (:on-select-model settings))
         (fn [event]
           (reset! open-menu* nil)
           ((:on-select-model settings) event)))

        ;; The composer keeps its text when the active provider has no key,
        ;; so the user can add one and send again.
        on-send
        (mf/use-fn
         (mf/deps @prompt* sending? provider-history file-id set-key-status!)
         (fn [_]
           (let [text (str/trim (or @prompt* ""))]
             (when-let [api (and (seq text) (not sending?) (ai-api))]
               (p/let [provider-id (.getActiveProvider ^js api)
                       raw-status  (.getKeyStatus ^js api)]
                 (let [key-status (js->clj raw-status :keywordize-keys true)]
                   (set-key-status! key-status)
                   (if (get key-status (keyword provider-id))
                     (do (reset! prompt* "")
                         (send-prompt! api file-id provider-id text provider-history))
                     (st/emit! (ai-chat/set-status
                                {:phase "error"
                                 :message (tr "kizuku.ai.error.no-key"
                                              (provider-label provider-id))})))))))))

        on-composer-keydown
        (mf/use-fn
         (mf/deps on-send)
         (fn [event]
           (let [native (.-nativeEvent ^js event)]
             (cond
               (and (kbd/mod? native) (= (.-key native) "Enter"))
               (do (dom/prevent-default event) (on-send event))

               (= (.-key native) "Escape")
               (do (dom/stop-propagation event) (on-close event))))))

        ;; Escape closes an open picker first, then the panel.
        on-panel-keydown
        (mf/use-fn
         (mf/deps @open-menu*)
         (fn [event]
           (when (= (.-key (.-nativeEvent ^js event)) "Escape")
             (dom/stop-propagation event)
             (if @open-menu*
               (reset! open-menu* nil)
               (on-close event)))))]

    ;; Load persisted history once per file, on mount / file switch.
    (mf/with-effect [file-id]
      (when file-id
        (load-persisted-history! file-id)))

    ;; Focus the composer on open.
    (mf/with-effect [open?]
      (when open?
        (when-let [node (mf/ref-val composer-ref)]
          (.focus ^js node))))

    ;; Close an open picker on any click outside the panel, standard
    ;; dropdown behaviour — only attached while a picker is open.
    (mf/with-effect [@open-menu*]
      (when @open-menu*
        (let [on-doc-click (fn [event]
                             (let [panel (mf/ref-val panel-ref)]
                               (when-not (and panel (.contains ^js panel (dom/get-target event)))
                                 (reset! open-menu* nil))))]
          (.addEventListener js/document "pointerdown" on-doc-click)
          (fn [] (.removeEventListener js/document "pointerdown" on-doc-click)))))

    [:*
     (when open?
       [:div
        {:ref panel-ref
         :class (stl/css :panel :panel-fade-in)
         :role "dialog"
         :aria-label (tr "kizuku.ai.panel-label")
         :on-key-down on-panel-keydown
         ;; No size yet (never resized) means no inline width/height at
         ;; all; the panel's own CSS sizes it to its content. A JS object,
         ;; not a map: rumext only converts style maps it can see at
         ;; compile time, and a map built at runtime reaches React as an
         ;; opaque object that sets nothing — which is why the drag once
         ;; changed the state but never the panel.
         :style (when-let [{:keys [width height]} (:size resize)]
                  #js {:width (str width "px") :height (str height "px")})}

        [:> panel-header* {:settings settings
                           :open-menu @open-menu*
                           :on-toggle-provider-menu on-toggle-provider-menu
                           :on-toggle-model-menu on-toggle-model-menu
                           :on-select-provider on-select-provider
                           :on-select-model on-select-model
                           :on-clear on-clear
                           :on-close on-close}]

        [:> message-log* {:display-log (or display-log [])
                          :status status
                          :has-key? has-key?
                          :provider-id provider
                          :on-open-settings on-open-settings}]

        [:> composer* {:prompt @prompt*
                       :sending? sending?
                       :composer-ref composer-ref
                       :on-change on-change
                       :on-key-down on-composer-keydown
                       :on-send on-send}]

        [:> resize-handles* {:on-pointer-down (:on-pointer-down resize)}]])

     [:button
      {:ref fab-ref
       :class (stl/css-case :fab true :fab-open open?)
       :on-click on-toggle
       :aria-label (if open? (tr "kizuku.ai.fab-close") (tr "kizuku.ai.fab-open"))
       :aria-expanded open?
       :title (tr "kizuku.ai.panel-label")}
      (if open? "✕" "✨")]]))
