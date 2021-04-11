(ns danilofmesquita.minesweeper.app
 (:require [danilofmesquita.minesweeper.game :as mnsw]
           [reagent.core :as r]
           [reagent.dom :as rdom]))

(def g-height (r/atom 9))
(def g-width (r/atom 9))
(def g-bombs (r/atom 10))

(defn empty-game []
  (mnsw/create-game @g-height @g-width @g-bombs))

(def game-state (r/atom (empty-game)))
(def timer (r/atom 0))
(def timer-id (r/atom 0))

(defn start-timer []
  (js/clearInterval @timer-id)
  (reset! timer 0)
  (reset! timer-id (js/setInterval #(swap! timer inc) 1000)))

(defn stop-timer []
  (js/clearInterval @timer-id))

(defn handle-cell-click [point]
  (when-not (mnsw/game-ended? @game-state)
    (if (:started? @game-state)
      (do
        (reset! game-state (mnsw/open-cell @game-state point))
        (when (:lost? @game-state) (stop-timer)))
      (do
        (start-timer)
        (reset! game-state (mnsw/start-game @game-state point))))))

(defn handle-cell-context-menu [event point]
   (when (:started? @game-state)
     (.preventDefault event)
     (reset! game-state (mnsw/flag-cell @game-state point))
     false))

(defn cell
  ([x y]
   (cell (mnsw/new-point x (mod y (:width @game-state)))))
  ([point]
   [:a.column
    {:class           [(when (mnsw/open? @game-state point) "opened")
                       (when (mnsw/show-bomb-count? @game-state point)
                         (str "level-" (mnsw/bomb-count @game-state point)))]
     :on-click        #(handle-cell-click point)
     :on-context-menu #(handle-cell-context-menu % point)}
    (cond
      (mnsw/exploded? @game-state point)
      "💥"
      (mnsw/show-bomb? @game-state point)
      "💣"
      (mnsw/show-flag? @game-state point)
      "🚩"
      (mnsw/show-bomb-count? @game-state point)
      (mnsw/bomb-count @game-state point))]))

(defn status []
  [:div#indicator
   {:on-click (fn []
                (stop-timer) 
                (reset! timer 0)
                (reset! game-state (empty-game)))}
   [:a (cond
            (:lost? @game-state)
            "😵"
            (:won? @game-state)
            "😎"
            :else "😀")]])

(defn timer-indicator []
  [:div#timer @timer])

(defn used-flags []
  [:div#flag-counter (mnsw/count-used-flags @game-state)])

(defn row [x]
  (let [width (:width @game-state)]
    [:div.row (for [y (range (* x width) (+ (* x width) width))]
                ^{:key y} [cell x y])]))
  
(defn grid []
  (let [height (:height @game-state)]
    [:div#grid.grid (for [i (range 0 height)]
                      ^{:key i} [row i])]))

(defn g-input [label g-field]
  [:div [:label label]
   [:input {:default-value @g-field
            :type 'number
            :min 4
            :on-change (fn [event]
                         (reset! g-field (-> event .-target .-value int))
                         (reset! game-state (empty-game)))}]])

(defn minesweeper []
  [:div#wrapper
   [:div#form
    [g-input "Height:" g-height]
    [g-input "Width:" g-width]
    [g-input "Bombs:" g-bombs]]
   [:div#game
    [:div#header
     [used-flags]
     [status]
     [timer-indicator]]
    [grid]]])

(defn init []
  (rdom/render [minesweeper]
               (.getElementById js/document "root")))