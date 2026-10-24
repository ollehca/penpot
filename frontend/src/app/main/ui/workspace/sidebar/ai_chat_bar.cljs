;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC
;;
;; ADDED BY KIZUKU (2026-10-21, rebuilt 2026-10-22): the real AI chat
;; panel — a floating FAB + resizable overlay with a scrollable
;; conversation (not a single-shot input), live turn-status, a provider
;; badge, per-file persistence, and keyboard/ARIA support. Supersedes the
;; Phase 1 proof-of-pipeline version (single input, no history, no
;; accessibility, hardcoded dark-only colors).
;;
;; State lives in app.main.data.workspace.ai-chat, fed by
;; app.main.data.workspace.ai-tools's existing onToolCall/onTurnStatus
;; handlers (the IPC bridge only supports one registered listener at a
;; time — see ai-tools.cljs's own header comment — so this component
;; never registers its own; it only reads the shared reactive lens).
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
   [rumext.v2 :as mf]))

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

(defn- electron-api
  []
  (unchecked-get js/window "electronAPI"))

(def ^:private min-panel-size {:width 300 :height 280})
(def ^:private max-panel-width 640)

(defn- max-panel-height
  "90% of the real app window height — mirrors .panel's own CSS max-height
  (90vh), since the resize drag's clamp runs in JS and can't read a CSS
  custom property. Computed fresh each call rather than cached once, so a
  window resize while the panel happens to be open doesn't leave a stale
  ceiling from whatever size the window was at mount."
  []
  (* (.-innerHeight js/window) 0.9))

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

(defn- clamp
  [v lo hi]
  (max lo (min hi v)))

(defn- status-label
  [status]
  (case status
    "started" (tr "kizuku.ai.status.thinking")
    "tool-executing" (tr "kizuku.ai.status.editing")
    "text" (tr "kizuku.ai.status.thinking")
    "sending" (tr "kizuku.ai.status.sending")
    nil))

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
       (not (empty? visible-entries))
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

(mf/defc ai-chat-bar*
  {::mf/private true}
  []
  (let [{:keys [open? display-log provider-history status]}
        (mf/deref ai-chat/chat-lens)

        prompt       (mf/use-state "")
        provider     (mf/use-state nil)
        key-status   (mf/use-state nil)
        provider-menu-open? (mf/use-state false)
        model        (mf/use-state nil)
        model-tiers  (mf/use-state [])
        model-menu-open? (mf/use-state false)
        ;; nil means "auto" — the panel sizes itself to its content via CSS
        ;; (grid-template-rows: auto 1fr auto, capped at max-height: 90vh),
        ;; so a short conversation stays compact and a long one grows to
        ;; show more context, without the user having to drag anything.
        ;; Dragging the resize handle sets an explicit {:width :height}
        ;; here, which from then on overrides the auto behavior — standard
        ;; "auto until the user takes control" sizing.
        panel-size   (mf/use-state nil)
        resizing?    (mf/use-ref false)
        resize-start (mf/use-ref nil)
        fab-ref      (mf/use-ref nil)
        composer-ref (mf/use-ref nil)
        panel-ref    (mf/use-ref nil)

        ;; :current-file-id is a real uuid record (app.common.uuid), not a
        ;; plain string — embedding it directly in a #js {...} literal
        ;; doesn't coerce it, so it reached the main process as a CLJS
        ;; object, which JSON.stringify then rendered as the literal
        ;; string "[object Object]" as the persisted-history file's top-
        ;; level key. Every save and load landed under that same wrong
        ;; key, so history never actually round-tripped. (str uuid) here,
        ;; once, makes every downstream :fileId already a plain string.
        file-id (some-> (mf/deref file-id-ref) str)

        sending? (= status "sending")

        on-toggle
        (mf/use-fn
         (mf/deps open?)
         (fn [_]
           (st/emit! (ai-chat/toggle-panel))))

        on-close
        (mf/use-fn
         (fn [_]
           (st/emit! (ai-chat/close-panel))
           (when-let [node (mf/ref-val fab-ref)]
             (.focus node))))

        on-open-settings
        (mf/use-fn
         (fn [_]
           (when-let [api (electron-api)]
             (.open ^js (.-settings api)))))

        on-change
        (mf/use-fn
         (fn [event]
           (reset! prompt (dom/get-value (dom/get-target event)))))

        on-clear
        (mf/use-fn
         (mf/deps file-id)
         (fn [_]
           (st/emit! (ai-chat/reset-history))
           (when (and file-id (electron-api))
             (.command ^js (.-mockBackend (electron-api))
                       "save-ai-chat-history"
                       #js {:fileId file-id :displayLog #js [] :providerHistory #js []}))))

        on-toggle-provider-menu
        (mf/use-fn
         (fn [_]
           (swap! provider-menu-open? not)))

        on-select-provider
        (mf/use-fn
         (fn [event]
           (let [provider-id (dom/get-data (dom/get-current-target event) "provider-id")
                 api (electron-api)]
             (reset! provider-menu-open? false)
             (when api
               (-> (.setActiveProvider ^js (.-ai api) provider-id)
                   (.then (fn [_] (reset! provider provider-id))))
               ;; Model tiers are per-provider — refresh both the current
               ;; selection and the tier list for the newly active one.
               (-> (.getModel ^js (.-ai api) provider-id)
                   (.then (fn [m] (reset! model m))))
               (-> (.getModelTiers ^js (.-ai api) provider-id)
                   (.then (fn [tiers] (reset! model-tiers (js->clj tiers :keywordize-keys true)))))
               ;; A provider switch starts a fresh resumption thread — the
               ;; old provider-history is shaped for the old provider
               ;; (Anthropic content blocks / OpenAI tool_calls messages /
               ;; Google's stateful interaction markers) and replaying it
               ;; against a different provider's API would be meaningless.
               ;; display-log (the visible transcript) is left untouched.
               (st/emit! (ai-chat/set-provider-history []))))))

        on-toggle-model-menu
        (mf/use-fn
         (fn [_]
           (swap! model-menu-open? not)))

        on-select-model
        (mf/use-fn
         (mf/deps @provider)
         (fn [event]
           (let [model-id (dom/get-data (dom/get-current-target event) "model-id")
                 api (electron-api)]
             (reset! model-menu-open? false)
             (when (and api @provider)
               (-> (.setModel ^js (.-ai api) @provider model-id)
                   (.then (fn [_] (reset! model model-id))))))))

        persist-history!
        (mf/use-fn
         (mf/deps file-id)
         (fn [provider-history]
           (when (and file-id (electron-api))
             ;; Read display-log fresh at persist time, not from any
             ;; closure — ai-tools.cljs's dispatch-tool-call may have
             ;; appended tool-call entries mid-turn, after on-send's own
             ;; closure captured its view of the log.
             (let [fresh-log (get-in @st/state [:ai-chat :display-log])]
               (.command ^js (.-mockBackend (electron-api))
                         "save-ai-chat-history"
                         #js {:fileId file-id
                              :displayLog (clj->js fresh-log)
                              :providerHistory (clj->js provider-history)})))))

        ;; Actually dispatch a prompt once we know the active provider has a
        ;; stored key. Split out of on-send so the key-status gate above it
        ;; stays flat and easy to read.
        dispatch-prompt!
        (mf/use-fn
         (mf/deps provider-history file-id)
         (fn [api provider-id text]
           (st/emit! (ai-chat/append-display-entry {:role :user :text text}))
           (st/emit! (ai-chat/set-status {:phase "sending"}))
           (reset! prompt "")
           (-> (.sendPrompt ^js (.-ai api)
                            #js {:provider provider-id
                                 :prompt text
                                 :systemPrompt (ai-tools/build-system-prompt)
                                 :history (clj->js provider-history)})
               (.then
                (fn [res]
                  (if (unchecked-get res "success")
                    (let [new-history (js->clj (unchecked-get res "history") :keywordize-keys true)]
                      (st/emit! (ai-chat/set-provider-history new-history))
                      (persist-history! new-history))
                    ;; Do NOT emit set-status here too — ai_tools.cljs's
                    ;; handle-turn-status already received this same error
                    ;; via the real-time ai:turn-status push (which fires
                    ;; synchronously inside runTurn, before this IPC
                    ;; response resolves) and has already appended the
                    ;; error bubble. Doing it again here produced two
                    ;; identical error bubbles for one failure. A failed
                    ;; turn still needs persisting (the user message +
                    ;; error bubble ai_tools.cljs just added) — provider-
                    ;; history is unchanged, the turn never produced a new
                    ;; one to resume from.
                    (persist-history! provider-history))))
               (.catch
                (fn [_err]
                  ;; Same reasoning as above: a request that reaches the
                  ;; provider and then fails already goes through
                  ;; ai:turn-status's error phase. This catch only exists
                  ;; for a failure before any status push could fire (e.g.
                  ;; the IPC call itself rejecting) — still worth
                  ;; persisting whatever display-log already has.
                  (persist-history! provider-history))))))

        on-send
        (mf/use-fn
         (mf/deps @prompt sending? dispatch-prompt!)
         (fn [_]
           (let [text (str/trim (or @prompt ""))
                 api  (electron-api)]
             (when (and (seq text) (not sending?) api)
               (-> (.getActiveProvider ^js (.-ai api))
                   (.then
                    (fn [provider-id]
                      (-> (.getKeyStatus ^js (.-ai api))
                          (.then
                           (fn [raw-status]
                             (let [status (js->clj raw-status :keywordize-keys true)]
                               (reset! key-status status)
                               (if (get status (keyword provider-id))
                                 (dispatch-prompt! api provider-id text)
                                 (st/emit!
                                  (ai-chat/set-status
                                   {:phase "error"
                                    :message (tr "kizuku.ai.error.no-key"
                                                 (provider-label provider-id))}))))))))))))))

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

        on-panel-keydown
        (mf/use-fn
         (mf/deps @provider-menu-open? @model-menu-open?)
         (fn [event]
           (when (= (.-key (.-nativeEvent ^js event)) "Escape")
             (dom/stop-propagation event)
             (cond
               @provider-menu-open? (reset! provider-menu-open? false)
               @model-menu-open? (reset! model-menu-open? false)
               :else (on-close event)))))

        on-resize-pointer-down
        (mf/use-fn
         (mf/deps @panel-size)
         (fn [event]
           (dom/prevent-default event)
           (mf/set-ref-val! resizing? true)
           ;; In auto mode (panel-size nil, size driven purely by CSS) there's
           ;; no stored {:width :height} to drag from — read the panel's real
           ;; rendered box instead, so the drag starts from wherever it
           ;; actually is on screen rather than snapping to some default.
           (let [{:keys [width height]}
                 (or @panel-size
                     (when-let [node (mf/ref-val panel-ref)]
                       (let [rect (.getBoundingClientRect ^js node)]
                         {:width (.-width rect) :height (.-height rect)})))]
             (mf/set-ref-val! resize-start
                               {:x (.-clientX ^js event) :y (.-clientY ^js event)
                                :width width :height height}))))]

    ;; Load persisted history once per file, on mount / file switch.
    (mf/with-effect [file-id]
      (when (and file-id (electron-api))
        (-> (.command ^js (.-mockBackend (electron-api)) "get-ai-chat-history" #js {:fileId file-id})
            (.then (fn [res]
                     (let [raw (unchecked-get res "raw")
                           ;; js->clj :keywordize-keys only keywordizes map
                           ;; KEYS, never string VALUES — a round-tripped
                           ;; entry's :role lands as the string "user", not
                           ;; the keyword :user that freshly-appended
                           ;; entries use (ai_tools.cljs/ai_chat_bar.cljs
                           ;; construct those directly as CLJS maps). The
                           ;; message-log*'s role filter checks against a
                           ;; keyword set, so every loaded entry silently
                           ;; failed it — the data was correctly loaded
                           ;; into state the whole time, just invisible to
                           ;; that filter. Re-keywordize :role explicitly.
                           dl  (->> (js->clj (unchecked-get raw "displayLog") :keywordize-keys true)
                                    (mapv #(update % :role keyword)))
                           ph  (js->clj (unchecked-get raw "providerHistory") :keywordize-keys true)]
                       (st/emit! (ai-chat/set-history
                                   {:display-log dl :provider-history ph})))))
            (.catch (fn [err] (js/console.error "Failed to load AI chat history" err))))))

    ;; Resolve the active provider + each provider's key status + the
    ;; active provider's model/tiers once on mount and whenever the panel
    ;; opens, so the badges/pickers reflect a change made in Settings
    ;; meanwhile.
    (mf/with-effect [open?]
      (when (and open? (electron-api))
        (-> (.getActiveProvider ^js (.-ai (electron-api)))
            (.then (fn [p]
                     (reset! provider p)
                     (-> (.getModel ^js (.-ai (electron-api)) p)
                         (.then (fn [m] (reset! model m))))
                     (-> (.getModelTiers ^js (.-ai (electron-api)) p)
                         (.then (fn [tiers] (reset! model-tiers (js->clj tiers :keywordize-keys true))))))))
        (-> (.getKeyStatus ^js (.-ai (electron-api)))
            (.then (fn [s] (reset! key-status (js->clj s :keywordize-keys true)))))))

    ;; Focus the composer on open.
    (mf/with-effect [open?]
      (when (and open? (mf/ref-val composer-ref))
        (.focus (mf/ref-val composer-ref))))

    ;; Close the provider/model menus on any outside click, standard
    ;; dropdown behavior — only attached while a menu is actually open.
    (mf/with-effect [@provider-menu-open? @model-menu-open?]
      (when (or @provider-menu-open? @model-menu-open?)
        (let [on-doc-click (fn [event]
                              (let [target (dom/get-target event)]
                                (when-not (and (mf/ref-val panel-ref)
                                               (.contains ^js (mf/ref-val panel-ref) target))
                                  (reset! provider-menu-open? false)
                                  (reset! model-menu-open? false))))]
          (.addEventListener js/document "pointerdown" on-doc-click)
          (fn [] (.removeEventListener js/document "pointerdown" on-doc-click)))))

    ;; Window-level pointer listeners for the resize drag — only attached
    ;; while a resize is in progress.
    (mf/with-effect []
      (let [on-move (fn [event]
                      (when (mf/ref-val resizing?)
                        (let [{:keys [x y width height]} (mf/ref-val resize-start)
                              dx (- (.-clientX ^js event) x)
                              dy (- (.-clientY ^js event) y)]
                          (reset! panel-size
                                  {:width (clamp (+ width dx) (:width min-panel-size) max-panel-width)
                                   :height (clamp (+ height dy) (:height min-panel-size) (max-panel-height))}))))
            on-up (fn [_] (mf/set-ref-val! resizing? false))]
        (.addEventListener js/window "pointermove" on-move)
        (.addEventListener js/window "pointerup" on-up)
        (fn []
          (.removeEventListener js/window "pointermove" on-move)
          (.removeEventListener js/window "pointerup" on-up))))

    [:*
     (when open?
       [:div
        {:ref panel-ref
         :class (stl/css :panel :panel-fade-in)
         :role "dialog"
         :aria-label (tr "kizuku.ai.panel-label")
         :on-key-down on-panel-keydown
         ;; nil panel-size (not yet manually resized) means no inline
         ;; width/height at all — the panel's own CSS (grid-template-rows:
         ;; auto 1fr auto, max-height: 90vh) sizes it to its content.
         :style (when @panel-size
                  {:width (str (:width @panel-size) "px")
                   :height (str (:height @panel-size) "px")})}

        [:div {:class (stl/css :panel-header)}
         [:span {:class (stl/css :panel-title)} (tr "kizuku.ai.title")]
         [:div {:class (stl/css :provider-picker)}
          [:button {:class (stl/css :provider-badge)
                    :on-click on-toggle-provider-menu
                    :aria-haspopup "listbox"
                    :aria-expanded @provider-menu-open?
                    :aria-label (tr "kizuku.ai.provider-aria" (provider-label @provider))}
           (or (provider-label @provider) "…")
           [:span {:class (stl/css :provider-badge-caret)} "▾"]]
          (when @provider-menu-open?
            [:ul {:class (stl/css :provider-menu) :role "listbox"}
             (for [{:keys [id label]} known-providers]
               (let [has-key? (get @key-status (keyword id))]
                 [:li {:key id
                       :role "option"
                       :aria-selected (= id @provider)
                       :class (stl/css-case :provider-menu-item true
                                             :provider-menu-item-active (= id @provider))
                       :data-provider-id id
                       :on-click on-select-provider}
                  [:span {:class (stl/css :provider-menu-item-label)} label]
                  [:span {:class (stl/css-case :provider-menu-item-status true
                                                :has-key has-key?)}
                   (if has-key? (tr "kizuku.ai.key-set") (tr "kizuku.ai.no-key"))]]))])]
         [:div {:class (stl/css :provider-picker)}
          [:button {:class (stl/css :provider-badge)
                    :on-click on-toggle-model-menu
                    :aria-haspopup "listbox"
                    :aria-expanded @model-menu-open?
                    :aria-label (tr "kizuku.ai.model-aria" (model-label @model-tiers @model))}
           (short-model-label (model-label @model-tiers @model))
           [:span {:class (stl/css :provider-badge-caret)} "▾"]]
          (when @model-menu-open?
            [:ul {:class (stl/css :provider-menu) :role "listbox"}
             (for [{:keys [id label]} @model-tiers]
               [:li {:key id
                     :role "option"
                     :aria-selected (= id @model)
                     :class (stl/css-case :provider-menu-item true
                                           :provider-menu-item-active (= id @model))
                     :data-model-id id
                     :on-click on-select-model}
                [:span {:class (stl/css :provider-menu-item-label)} label]])])]
         [:button {:class (stl/css :header-button)
                   :on-click on-clear
                   :aria-label (tr "kizuku.ai.clear-aria")
                   :title (tr "kizuku.ai.clear-title")}
          (tr "kizuku.ai.clear")]
         [:button {:class (stl/css :header-button)
                   :on-click on-close
                   :aria-label (tr "kizuku.ai.close-aria")
                   :title (tr "kizuku.ai.close")}
          "✕"]]

        [:> message-log* {:display-log (or display-log [])
                           :status status
                           :has-key? (when (and @provider @key-status)
                                       (boolean (get @key-status (keyword @provider))))
                           :provider-id @provider
                           :on-open-settings on-open-settings}]

        [:div {:class (stl/css :composer)}
         [:textarea
          {:ref composer-ref
           :class (stl/css :composer-input)
           :placeholder (tr "kizuku.ai.placeholder")
           :value @prompt
           :disabled sending?
           :on-change on-change
           :on-key-down on-composer-keydown
           :aria-label (tr "kizuku.ai.prompt-aria")
           :rows 2}]
         [:button
          {:class (stl/css :send-button)
           :on-click on-send
           :disabled (or sending? (empty? (str/trim (or @prompt ""))))
           :aria-label (if sending? (tr "kizuku.ai.sending-aria") (tr "kizuku.ai.send-aria"))}
          (if sending? (tr "kizuku.ai.sending") (tr "kizuku.ai.send"))]]

        [:div {:class (stl/css :resize-handle)
               :on-pointer-down on-resize-pointer-down
               :aria-hidden true}]])

     [:button
      {:ref fab-ref
       :class (stl/css-case :fab true :fab-open open?)
       :on-click on-toggle
       :aria-label (if open? (tr "kizuku.ai.fab-close") (tr "kizuku.ai.fab-open"))
       :aria-expanded open?
       :title (tr "kizuku.ai.panel-label")}
      (if open? "✕" "✨")]]))
