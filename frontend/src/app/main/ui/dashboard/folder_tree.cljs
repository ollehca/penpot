;; Copyright (c) 2026 Kizuku. All rights reserved.
;; This file is NOT part of the original PenPot project.
;; Proprietary — see /LICENSE in the project root.
;;
;; Kizuku Folder Tree Component
;; Folders inside projects with context menu, inline rename,
;; expand/collapse to show files, and drag-drop support.

(ns app.main.ui.dashboard.folder-tree
  (:require-macros
   [app.common.data.macros :as dm]
   [app.main.style :as stl])
  (:require
   [app.main.refs :as refs]
   [app.main.repo :as rp]
   [app.main.ui.dashboard.grid :refer [grid*]]
   [app.util.dom :as dom]
   [app.util.dom.dnd :as dnd]
   [app.util.keyboard :as kbd]
   [beicon.v2.core :as rx]
   [rumext.v2 :as mf]))

(def ^:private folder-svg
  "M2 3.5A1.5 1.5 0 0 1 3.5 2h2.879a1.5 1.5 0 0 1 1.06.44l.622.62a.5.5 0 0 0 .354.147H12.5A1.5 1.5 0 0 1 14 4.707V12.5a1.5 1.5 0 0 1-1.5 1.5h-9A1.5 1.5 0 0 1 2 12.5v-9z")

(defn- fval [folder k]
  (or (get folder (keyword k)) (get folder k)))

;; ---------------------------------------------------------------
;; Inline rename
;; ---------------------------------------------------------------

(mf/defc folder-rename*
  {::mf/private true}
  [{:keys [name on-end]}]
  (let [val* (mf/use-state name)
        ref  (mf/use-ref)]
    (mf/use-effect
     (fn []
       (let [n (mf/ref-val ref)]
         (dom/focus! n)
         (dom/select-text! n))))
    [:input {:class (stl/css :folder-rename-input)
             :ref ref
             :value (deref val*)
             :on-change #(reset! val* (dom/get-target-val %))
             :on-blur #(on-end (dom/get-target-val %))
             :on-key-down (fn [e]
                            (dom/stop-propagation e)
                            (when (kbd/enter? e)
                              (on-end (dom/get-target-val e))))
             :on-click #(dom/stop-propagation %)}]))

;; ---------------------------------------------------------------
;; Context menu
;; ---------------------------------------------------------------

(mf/defc folder-menu*
  {::mf/private true}
  [{:keys [show pos on-rename on-delete on-subfolder on-close]}]
  (when show
    [:*
     [:div {:class (stl/css :folder-menu-backdrop)
            :on-click (fn [e]
                        (dom/prevent-default e)
                        (dom/stop-propagation e)
                        (on-close))
            :on-context-menu (fn [e]
                               (dom/prevent-default e)
                               (dom/stop-propagation e)
                               (on-close))}]
     [:div {:class (stl/css :folder-context-menu)
            :style {:left (str (:x pos) "px")
                    :top (str (:y pos) "px")}}
      [:div {:class (stl/css :folder-menu-item)
             :on-click (fn [e]
                         (dom/stop-propagation e)
                         (on-close)
                         (on-rename))}
       "Rename"]
      [:div {:class (stl/css :folder-menu-item)
             :on-click (fn [e]
                         (dom/stop-propagation e)
                         (on-close)
                         (on-subfolder))}
       "New Subfolder"]
      [:div {:class (stl/css :folder-menu-item :folder-menu-danger)
             :on-click (fn [e]
                         (dom/stop-propagation e)
                         (on-close)
                         (on-delete))}
       "Delete"]]]))

;; ---------------------------------------------------------------
;; Single folder node (no hooks that depend on dynamic state)
;; ---------------------------------------------------------------

(mf/defc folder-node*
  {::mf/private true}
  [{:keys [folder-id folder expanded-ids project
           on-toggle on-rename on-delete on-refresh
           on-create-subfolder project-id all-folders
           all-files selected-files depth]}]
  (let [name       (fval folder "name")
        file-ids   (or (fval folder "fileIds") [])
        children   (or (fval folder "children") [])
        has-kids   (seq children)
        expanded   (contains? expanded-ids folder-id)
        editing*   (mf/use-state false)
        drag-over* (mf/use-state false)
        menu*      (mf/use-state {:show false :pos nil})
        indent     (* depth 16)

        folder-files
        (mf/with-memo [file-ids all-files]
          (let [id-set (set (map str file-ids))]
            (->> (vals all-files)
                 (filter #(id-set (str (:id %))))
                 (sort-by :modified-at)
                 (reverse))))

        on-drop
        (mf/use-fn
         (mf/deps project-id folder-id on-refresh)
         (fn [e]
           (reset! drag-over* false)
           (when (dnd/has-type? e "penpot/files")
             (dom/prevent-default e)
             (dom/stop-propagation e)
             ;; selected-files is a set of UUID keys
             (let [sel (deref refs/selected-files)
                   file-ids (if (set? sel) sel (set (keys sel)))]
               (doseq [fid file-ids]
                 (->> (rp/cmd! :add-file-to-folder
                               {:project-id (dm/str project-id)
                                :folder-id (dm/str folder-id)
                                :file-id (dm/str fid)})
                      (rx/subs! (fn [_] (on-refresh)))))))))

        on-ctx
        (mf/use-fn
         (fn [e]
           (dom/prevent-default e)
           (dom/stop-propagation e)
           (let [vw (.-innerWidth js/window)
                 vh (.-innerHeight js/window)
                 mx (min (.-clientX e) (- vw 160))
                 my (min (.-clientY e) (- vh 120))]
             (reset! menu* {:show true
                            :pos {:x mx :y my}}))))

        on-rename-end
        (mf/use-fn
         (mf/deps folder-id on-rename)
         (fn [nm]
           (reset! editing* false)
           (when (and nm (pos? (count nm)))
             (on-rename folder-id nm))))

        on-delete-click
        (mf/use-fn
         (mf/deps folder-id file-ids on-delete name)
         (fn []
           (if (pos? (count file-ids))
             (js/alert "Move or remove files before deleting.")
             (when (js/confirm (str "Delete \"" name "\"?"))
               (on-delete folder-id)))))

        on-subfolder-click
        (mf/use-fn
         (mf/deps folder-id on-create-subfolder)
         (fn []
           (when on-create-subfolder
             (on-create-subfolder folder-id))))]

    [:div {:class (stl/css :folder-node)}
     [:div {:class (stl/css-case :folder-row true
                                 :folder-drag-over (deref drag-over*))
            :style {:padding-left (str (+ 12 indent) "px")}
            :on-click (fn [e]
                        (dom/stop-propagation e)
                        (on-toggle folder-id))
            :on-context-menu on-ctx
            :on-drag-over (fn [e]
                            (when (dnd/has-type? e "penpot/files")
                              (dom/prevent-default e)
                              (reset! drag-over* true)))
            :on-drag-leave #(reset! drag-over* false)
            :on-drop on-drop}

      [:span {:class (stl/css-case :folder-expand true
                                   :folder-expanded expanded)}
       "\u25B6"]
      [:svg {:class (stl/css :folder-icon)
             :viewBox "0 0 16 16" :width "12" :height "12"
             :fill "currentColor"}
       [:path {:d folder-svg}]]
      (if (deref editing*)
        [:> folder-rename* {:name name :on-end on-rename-end}]
        [:span {:class (stl/css :folder-name)} name])
      (when (pos? (count file-ids))
        [:span {:class (stl/css :folder-badge)} (count file-ids)])
      [:button {:class (stl/css :folder-dots-btn)
                :on-click on-ctx :title "Options"} "\u22EE"]]

     [:> folder-menu*
      {:show (:show (deref menu*))
       :pos (:pos (deref menu*))
       :on-rename #(reset! editing* true)
       :on-delete on-delete-click
       :on-subfolder on-subfolder-click
       :on-close #(reset! menu* {:show false :pos nil})}]

     (when (and expanded (pos? (count file-ids)))
       [:div {:class (stl/css :folder-files)}
        (if (seq folder-files)
          [:> grid* {:project project
                     :files folder-files
                     :selected-files selected-files
                     :can-edit true
                     :origin :files
                     :limit 4}]
          [:div {:class (stl/css :folder-empty-msg)}
           (str (count file-ids) " file(s) — open project to view")])])

     (when (and has-kids expanded)
       (for [cid children]
         (let [child (get all-folders cid)]
           (when child
             [:> folder-node*
              {:key (str cid)
               :folder-id (str cid)
               :folder child
               :expanded-ids expanded-ids
               :project project
               :on-toggle on-toggle
               :on-rename on-rename
               :on-delete on-delete
               :on-refresh on-refresh
               :on-create-subfolder on-create-subfolder
               :project-id project-id
               :all-folders all-folders
               :all-files all-files
               :selected-files selected-files
               :depth (inc depth)}]))))]))

;; ---------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------

(defn- fetch-tree! [pid atom]
  (->> (rp/cmd! :get-folder-tree {:project-id pid})
       (rx/subs! #(reset! atom %))))

(defn- next-name [base folders]
  (let [names (set (map #(fval (val %) "name") (or folders {})))
        try-n (fn [n] (let [nm (str base " " n)]
                        (when-not (names nm) nm)))]
    (or (some try-n (range 1 100))
        (str base " " (random-uuid)))))

;; ---------------------------------------------------------------
;; Panel (deref hooks live here, passed down as props)
;; ---------------------------------------------------------------

(mf/defc folder-tree-panel*
  [{:keys [project-id project on-folder-change on-assigned-files]}]
  (let [tree*      (mf/use-state nil)
        tree       (deref tree*)
        expanded*  (mf/use-state #{})
        all-files  (mf/deref refs/files)
        sel-files  (mf/deref refs/selected-files)

        folders    (or (get tree :folders) (get tree "folders"))

        refresh
        (mf/use-fn
         (mf/deps project-id)
         (fn [] (fetch-tree! project-id tree*)))

        on-create
        (mf/use-fn
         (mf/deps project-id folders)
         (fn [e]
           (dom/stop-propagation e)
           (let [nm (next-name "New Folder" folders)]
             (->> (rp/cmd! :create-folder
                           {:project-id project-id :name nm})
                  (rx/subs! (fn [_] (refresh)))))))

        on-toggle
        (mf/use-fn
         (fn [fid]
           (swap! expanded*
                  #(if (contains? % fid)
                     (disj % fid) (conj % fid)))))

        on-rename
        (mf/use-fn
         (mf/deps project-id)
         (fn [fid nm]
           (->> (rp/cmd! :rename-folder
                         {:project-id project-id
                          :folder-id fid :name nm})
                (rx/subs! (fn [_] (refresh))))))

        on-delete
        (mf/use-fn
         (mf/deps project-id)
         (fn [fid]
           (->> (rp/cmd! :delete-folder
                         {:project-id project-id
                          :folder-id fid})
                (rx/subs! (fn [_] (refresh))))))

        on-create-subfolder
        (mf/use-fn
         (mf/deps project-id folders)
         (fn [parent-id]
           (let [nm (next-name "Subfolder" folders)]
             (->> (rp/cmd! :create-folder
                           {:project-id project-id
                            :name nm
                            :parent-id parent-id})
                  (rx/subs! (fn [_]
                              (refresh)
                              (swap! expanded*
                                     conj parent-id)))))))]

    (mf/with-effect [project-id]
      (fetch-tree! project-id tree*))

    ;; Report all file IDs assigned to any folder
    (mf/with-effect [folders]
      (when on-assigned-files
        (let [ids (when folders
                    (->> (vals folders)
                         (mapcat #(or (fval % "fileIds") []))
                         (map str)
                         (set)))]
          (on-assigned-files (or ids #{})))))

    (let [roots (when (and folders (pos? (count folders)))
                  (->> folders
                       (filter (fn [[_ v]]
                                 (nil? (fval v "parentId"))))
                       (sort-by (fn [[_ v]] (fval v "name")))
                       (map first)))]

      [:div {:class (stl/css :folder-panel)}
       [:div {:class (stl/css :folder-header)}
        [:span {:class (stl/css :folder-header-title)} "Folders"]
        [:button {:class (stl/css :folder-add-btn)
                  :on-click on-create :title "New Folder"}
         "+"]]
       (when (seq roots)
         [:div {:class (stl/css :folder-tree)}
          (for [fid roots]
            (let [folder (get folders fid)]
              (when folder
                [:> folder-node*
                 {:key (str fid)
                  :folder-id (str fid)
                  :folder folder
                  :expanded-ids (deref expanded*)
                  :project project
                  :on-toggle on-toggle
                  :on-rename on-rename
                  :on-delete on-delete
                  :on-refresh refresh
                  :on-create-subfolder on-create-subfolder
                  :project-id project-id
                  :all-folders folders
                  :all-files all-files
                  :selected-files sel-files
                  :depth 0}])))])])))
