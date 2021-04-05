(ns minesweeper.core
  (:require [minesweeper.game :as mnsw]))

(def empty-game (mnsw/create-game 9 9 1))
(def timer (atom 0))
(def timer? (atom false))
(def timer-id (atom 0))
(def polling-id (atom 0))

(declare render)

(defn start-timer []
  (when-not @timer?
    (js/clearInterval @timer-id)
    (reset! timer? true)
    (reset! timer 0)
    (reset! timer-id (js/setInterval #(swap! timer inc) 1000))))

(defn stop-timer []
  (reset! timer? false)
  (js/clearInterval @timer-id))

(defn append-cell [game i j row width]
  (let [column (.createElement js/document "div")
        point (mnsw/new-point i (mod j width)) ]
    (.add (.-classList column) "column")
    (when (mnsw/open? game point)
      (.add (.-classList column) "opened"))
    (cond 
      (mnsw/exploded? game point) 
        (set! (.-innerHTML column) "💥")
      (and (:lost? game) (mnsw/bomb? game point)) 
        (set! (.-innerHTML column) "💣")
      (or (mnsw/flag? game point) 
          (and (:won? game) (mnsw/bomb? game point)))
        (set! (.-innerHTML column) "🚩")
      (and(mnsw/open? game point) (> (mnsw/bomb-count game point) 0))
        (do (set! (.-innerHTML column) (mnsw/bomb-count game point))
          (.add (.-classList column) 
                (str "level-" (mnsw/bomb-count game point)))))
    (when-not (or (:lost? game) (:won? game))
      (if (:started? game)
        (do (.addEventListener 
              column 
              "click"
              #(render (mnsw/open-cell game point)))
            (.addEventListener
              column
              "contextmenu"
              #(do (.preventDefault %)
                   (render (mnsw/flag-cell game point))
                   false)))
        (.addEventListener column 
                           "click"
                           #(render (mnsw/start-game game point)))))
    (.appendChild row column)))

(defn status [game]
  (cond (:lost? game) "😵"
        (:won? game)  "😎"
        :else             "😀"))

(defn append-status [game node]
  (let [indicator (.createElement js/document "div")]
    (.add (.-classList indicator) "indicator")
    (set! (.-innerHTML indicator) (str "<span>" (status game) "</span>"))
    (.addEventListener indicator 
                       "click"
                       #(do (stop-timer) (render empty-game)))
    (.appendChild node indicator)))

(defn append-grid [{:keys [width height] :as game} node]
  (let [grid (.getElementById js/document "grid")]
    (set! (.-innerHTML grid) "")
    (.add (.-classList grid) "grid")
    (doseq [i (range 0 height)
            :let [row (.createElement js/document "div")]]
      (doseq [j (range (* i width) (+ (* i width) width))]
        (append-cell game i j row width))
      (.add (.-classList row) "row")
      (.appendChild node row))))

(defn append-timer [{:keys [started?]} node]
  (js/clearInterval @polling-id)
  (when started? 
    (set! (.-innerHTML node) @timer)
    (reset! polling-id (js/setInterval 
                         #(set! (.-innerHTML node) @timer)
                         300))))

(defn append-used-flags [game node]
  (set! (.-innerHTML node) (mnsw/count-used-flags game)))
    
(defn render [{:keys [won? lost? started?] :as game}]
  (set! (.-innerHTML (.-body js/document)) "
    <div id=\"game\">
      <div id=\"header\">
        <div id=\"flag-counter\">0</div>
        <div id=\"status\"></div>
        <div id=\"timer\">0</div>
      </div>
      <div id=\"grid\"></div>
    </div>")
  (if (or won? lost?) 
    (stop-timer) 
    (when started? (start-timer)))
  (append-timer game (.getElementById js/document "timer"))
  (append-used-flags game (.getElementById js/document "flag-counter"))
  (append-status game (.getElementById js/document "status"))
  (append-grid game (.getElementById js/document "grid")))

(render empty-game)
