(ns danilofmesquita.minesweeper.game)

(defn new-point [row column]
  {:row row :column column})

(defn- new-cell [width, index]
  {:point (new-point (int (/ index width)) (mod index width))})

(defn- new-grid [height, width]
  (->> (range)
       (map #(new-cell width %))
       (take (* height width))
       (into [])))

(defn create-game [height, width, number-of-bombs]
  {:height height
   :width width
   :number-of-bombs number-of-bombs
   :grid (new-grid height width)})

(defn count-cells [{:keys [width height]}]
  (* width height))

(defn count-bombs-in [cells]
  (->> cells
       (filter #(:bomb? %))
       (count)))

(defn point->index [{:keys [height width]} {:keys [row column]}]
  (if (and (>= row 0)
           (< row height)
           (>= column 0)
           (< column width))
    (+ column (* row width))
    -1))

(defn- neighbors-of [game {:keys [row column]}]
  (->> [[-1 -1] [-1  0] [-1  1]
        [0 -1]         [0  1]
        [1 -1] [1  0] [1  1]]
       (map #(apply (fn [x y] (new-point (+ row x) (+ column y))) %))
       (map #(point->index game %))
       (map #(get-in game [:grid %]))
       (filter #(some? %))))

(defn- count-neighbors-bombs [game point]
  (count-bombs-in (neighbors-of game point)))

(defn- add-neighbors-bombs-count [game]
  (->> (:grid game)
       (mapv #(if (nil? (:bomb? %))
                (assoc %
                       :neighbors-bombs-count
                       (count-neighbors-bombs game (:point %)))
                %))
       (assoc game :grid)))

(defn- rand-bomb-spot [game trigger-point]
  (let [index (rand-int (count-cells game))]
    (if (= index (point->index game trigger-point))
      (recur game trigger-point)
      index)))

(defn- add-bombs [game trigger-point amount]
  (if (> amount (count-bombs-in (:grid game)))
    (recur (assoc-in game
                     [:grid (rand-bomb-spot game trigger-point) :bomb?] true)
           trigger-point
           amount)
    game))

(defn bomb? [{:keys [grid] :as game} point]
  (:bomb? (grid (point->index game point))))

(defn flag? [{:keys [grid] :as game} point]
  (:flag? (grid (point->index game point))))

(defn open? [{:keys [grid] :as game} point]
  (:open? (grid (point->index game point))))

(defn exploded? [{:keys [grid] :as game} point]
  (:exploded? (grid (point->index game point))))

(defn bomb-count [{:keys [grid] :as game} point]
  (:neighbors-bombs-count (grid (point->index game point))))

(defn show-bomb-count? [game point]
  (and (open? game point) (> (bomb-count game point) 0)))

(defn show-bomb? [game point]
  (and (:lost? game) (bomb? game point)))

(defn show-flag? [game point]
  (or (flag? game point)
      (and (:won? game) (bomb? game point))))

(defn open-neighbors [game point]
  (if (bomb? game point)
    game
    (let [game (assoc-in game [:grid (point->index game point) :open?] true)]
      (if (zero? (bomb-count game point))
        (->> (neighbors-of game point)
             (filter #(not (:open? %)))
             (reduce #(open-neighbors %1 (:point %2)) game))
        game))))

(defn- explode-if-bomb [game point]
  (if (bomb? game point)
    (assoc-in game [:grid (point->index game point) :exploded?] true)
    game))

(defn- lost? [{:keys [grid]}] (pos? (->> grid (filter #(:exploded? %)) count)))

(defn- won? [{:keys [grid number-of-bombs]}]
  (= number-of-bombs (->> grid (filter #(not (:open? %))) count)))

(defn game-ended? [game]
  (or (:won? game) (:lost? game)))

(defn assoc-game-status [game]
  (-> game
      (assoc :won? (won? game))
      (assoc :lost? (lost? game))))

(defn open-cell [game point]
  (-> (assoc-in game [:grid (point->index game point) :open?] true)
      (open-neighbors point)
      (explode-if-bomb point)
      (assoc-game-status)))

(defn start-game [game trigger-point]
  (-> (assoc game :started? true)
      (add-bombs trigger-point (:number-of-bombs game))
      (add-neighbors-bombs-count)
      (open-cell trigger-point)))

(defn flag-cell [game point]
  (if (or (not (open? game point)) (flag? game point))
    (assoc-in game
              [:grid (point->index game point) :flag?]
              (not (flag? game point)))
    game))

(defn count-used-flags [{:keys [number-of-bombs grid]}]
  (- number-of-bombs
     (->> grid (filter #(:flag? %)) (count))))