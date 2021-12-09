(ns white-elephant
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.core.async :as async]
    [cheshire.core :as json]
    [see :as see])
  (:import
    [javax.imageio ImageIO]
    [java.util Base64 Base64$Decoder Random]
    [java.io ByteArrayInputStream Writer]
    [java.awt.image BufferedImage]
    [java.awt Graphics2D Image Color Font BasicStroke]
    [java.lang Math$RandomNumberGeneratorHolder]
    [java.time Instant]))

;;; TODO: ;;;
; 2. check the data is valid (more in webpage?)
; 6. persist game state to disk in case of accidents?


(def ^Base64$Decoder decoder (Base64/getDecoder))
(def encoding-prefix "data:image/png;base64,")

(defn import-image [^String data-url]
  (when data-url
    (let [data-string (subs data-url (count encoding-prefix))
          image (ImageIO/read (ByteArrayInputStream. (.decode decoder data-string)))]
      image)))

(defmethod print-method BufferedImage [value ^Writer w]
  (.write w (str "Image[" (.getWidth value) "x" (.getHeight value) "]")))

(defn- uniquify [names]
  (loop [seen {}
         result []
         [this-name & more-names] names]
    (let [this-name' (if-let [n (seen this-name)]
                       (str this-name "-" n)
                       this-name)
          result' (conj result this-name')]
      (if more-names
        (recur (update seen this-name (fnil inc 1)) result' more-names)
        result'))))

(defn import-data [directory-name]
  (let [elephants (->> (io/file directory-name)
                       (file-seq)
                       (filter #(re-find #"/white-elephant-.*\.json" (str %)))
                       (map #(do
                               (println "Loading:" (str %))
                               (let [data (json/parse-string (slurp %) keyword)]
                                 #_(println (string/join (take 20 (:image1 data))))
                                 data))))]
    (zipmap (uniquify (map :name elephants))
            elephants)))

(declare data)

(defn tabulate [data]
  (let [lengths (for [n (range (count (first data)))]
                  (apply max 1 (for [item (map #(nth % n) data)]
                                 (count (str item)))))
        fmt (str "%" (string/join "s %-" lengths) "s")]
    (mapv #(apply format fmt (map str %)) data)))

(defn wrapped? [present]
  (-> present :status (= "Wrapped")))

(defn locked? [present]
  (-> present :status (= "Locked")))

(defn opened? [present]
  (-> present :status (= "Opened")))

(defn remaining-string [data]
  (-> data :remaining pr-str))

(defn status-string [data]
  (->> data
       :presents
       (sort-by :index)
       (sort-by locked?)
       (map (fn [{:keys [index status owner wrapped opened] :as present}]
              [index status owner (if (wrapped? present) wrapped opened)]))
       tabulate
       (string/join "\n")))

(defn next-player-string [data]
  (some-> data :next-player (str " to play!")))

(defonce data (add-watch (atom {}) :save!
                         (fn [_k _r _o n]
                           (spit (io/file (str "save-data-" (Instant/now) ".txt"))
                                 (string/join "\n"
                                   [(remaining-string n)
                                    (status-string n)
                                    (next-player-string n)])))))

(defn set-random-seed [seed]
  (let [field (.getDeclaredField Math$RandomNumberGeneratorHolder "randomNumberGenerator")]
    (.setAccessible field true)
    (.setSeed ^Random (.get field nil) seed)))

(defn seeded-shuffle [coll]
  (sort-by (fn [_] (rand)) coll))

(defn prepare-data [elephants]
  (set-random-seed 0)
  (let [not-shared (filter #(= "notshared" (:shared %)) (vals elephants))
        shared (filter #(= "shared" (:shared %)) (vals elephants))
        shared-descriptons (seeded-shuffle (map :wrapped shared))
        shared-redescribed (map (fn [el desc] (assoc el :wrapped desc)) shared shared-descriptons)]
    (reset! data
            {:remaining   (sort (keys elephants))
             :next-player nil
             :presents    (mapv (fn [index {:keys [wrapped opened opened-long image1 image2 image3] :as x}]
                                  {:index       index
                                   :status      "Wrapped"
                                   :owner       nil
                                   :wrapped     wrapped
                                   :opened      opened
                                   :opened-long opened-long
                                   :images      (->> [image1 image2 image3]
                                                     (remove nil?)
                                                     (map import-image))})
                                (range)
                                (seeded-shuffle (concat shared-redescribed not-shared)))})))

(defn check-data []
  (let [players (:remaining @data)]
    ; check in web page really?
    (println (count players) "Players:" (string/join ", " players))))



(defn to-next-status [current-status]
  (->> ["Wrapped" "Opened" "Stolen" "Locked"]
       (drop-while #(not= % current-status))
       (drop 1)
       first))

(defn status []
  (-> @data status-string println)
  (some-> @data next-player-string println)
  nil)

(def see-width 600)
(def see-height 600)
(def elephant (ImageIO/read (io/file "white-elephant.png")))
(defonce rendering-data (atom {}))
(defn refresh-fn []
  (when-let [f (:refresh-fn @rendering-data)]
    (f)))
(defn image-queue []
  (:queue @rendering-data))
(defn ^Graphics2D get-graphics []
  (some-> (:image @rendering-data) (.getGraphics)))

(defn draw-image [^Graphics2D g ^Image image
                  source-x source-y source-width source-height
                  destination-x destination-y destination-width destination-height]
  (.drawImage g image
              destination-x destination-y (+ destination-x destination-width) (+ destination-y destination-height)
              source-x source-y (+ source-x source-width) (+ source-y source-height)
              nil))

(defn- enter-image [^Graphics2D g ^BufferedImage image frame-delay]
  (let [source-x 0
        source-y 0
        source-width (.getWidth image)
        source-height (.getHeight image)
        scale-factor (min (/ see-width (.getWidth image)) (/ see-height (.getHeight image)))
        destination-width (* source-width scale-factor)
        destination-height (* source-height scale-factor)
        destination-x (/ (- see-width destination-width) 2)
        destination-y (/ (- see-height destination-height) 2)]
    (doseq [y-offset (concat (range see-height 0 -5) [0])]
      (.setColor g Color/BLACK)
      (.fillRect g 0 y-offset see-width see-height)
      (draw-image g image
                  source-x source-y source-width source-height
                  destination-x (+ destination-y y-offset) destination-width destination-height)
      (refresh-fn)
      (Thread/sleep frame-delay))))

(defn reset-rendering-data [existing-rendering-data]
  (some-> existing-rendering-data :stop-fn .invoke)
  (let [image (BufferedImage. see-width see-height BufferedImage/TYPE_INT_ARGB)
        queue (async/chan 1000)]
    {:image         image
     :refresh-fn    (see/see image :fps 25 :only-draw-when-updated? true)
     :queue         queue
     :render-thread (future
                      (try
                        (let [g ^Graphics2D (.getGraphics image)]
                          (loop [{:keys [image delay pause] :as item} nil]
                            ;(println (dissoc item :image))
                            (cond
                              image (enter-image g image delay)
                              pause (Thread/sleep pause))
                            (if-let [next-item (async/<!! queue)]
                              (recur next-item)
                              (println "shutting down rendering"))))
                        (catch Exception e
                          (.printStackTrace e))))
     :stop-fn       #(async/close! queue)}))

(defn enqueue-pause [pause]
  (when-let [queue (image-queue)]
    (let [max-pause 1000
          pauses (repeat (int (/ pause max-pause)) {:pause max-pause})
          remnant (rem pause max-pause)]
      (async/<!! (async/onto-chan!! queue pauses false))
      (when (pos? remnant) (async/>!! queue {:pause remnant})))))

(defn enqueue-image
  ([image] (enqueue-image image 10))
  ([image delay]
   (when-let [queue (image-queue)]
     (async/>!! queue {:image image
                       :delay delay}))))

(defn clear-transition-queue []
  (when-let [queue (image-queue)]
    (while (async/poll! queue))))

(defn initialise [directory-name]
  (prepare-data (import-data directory-name))
  (check-data)
  (swap! rendering-data reset-rendering-data)
  (enqueue-image elephant))

(defn show [present-number]
  (let [{:keys [status opened opened-long images]} (get-in @data [:presents present-number])
        g (get-graphics)]
    (.setColor g Color/BLACK)
    (if (= status "Wrapped")
      (println (str "Present " present-number " has not yet been unwrapped!"))
      (do
        (println opened)
        (println (or opened-long "No more detailed description available."))
        (if (= 1 (count images))
          (do
            (enqueue-image (first images) 10)
            (enqueue-pause 5000))
          (doseq [image (take (* 2 (count images)) (cycle images))]
            (enqueue-image image)
            (enqueue-pause 3000)))
        (enqueue-image elephant)))))

(defn pick [present-number]
  (let [{:keys [next-player last-thief presents]} @data
        present (nth presents present-number)
        owner (:owner present)
        error (cond
                (locked? present) (str "Present " present-number " has been stolen too many times already!")
                (and owner (= last-thief owner)) (str "You can't steal it straight back!"))]
    (cond
      error (println error)
      (nil? next-player) "No one is currently playing!"
      :else (do
              (swap! data
                     (fn [gd]
                       (-> gd
                           (update-in [:presents present-number :status] to-next-status)
                           (assoc-in [:presents present-number :owner] next-player)
                           (assoc :next-player owner)
                           (assoc :last-thief (when (opened? present) next-player)))))
              (show present-number)))))

(defn- horizontal-line [g p]
  (let [fudge -15]
    (.drawLine g
               0 (int (+ (* see-height p) fudge))
               see-width (int (+ (* see-height p) fudge)))))

(defn- wheel-of-random [coll]
  (if (< 1 (count coll))
    (let [g (get-graphics)]
      (clear-transition-queue)
      (.setFont g (Font. "Helvetica" Font/BOLD (/ see-height 20)))
      (loop [position 0
             speed 0.3
             items (cycle (shuffle coll))
             item (first items)
             linger 0]
        (let [[items' position'] (if (< 1 position)
                                   [(rest items) (dec (mod position (count coll)))]
                                   [items position])
              item' (first items')
              point-acceleration (* (- 0.5 (mod position 1)) 1/110)
              fast-friction (Math/abs (double (* speed 1/150)))
              slow-friction (rand 0.002)
              friction (- (* (Math/signum speed) (min (+ fast-friction slow-friction) (Math/abs speed))))
              acceleration (+ point-acceleration friction)
              linger' (if (= item item') (inc linger) 0)]
          (.setColor g Color/BLACK)
          (.fillRect g 0 0 see-width see-height)

          (.setColor g Color/GRAY)
          (.setStroke g (BasicStroke. (* see-height 1/8)))
          (horizontal-line g 11/16)
          (.setColor g Color/GREEN)
          (.setStroke g (BasicStroke. 3))
          (horizontal-line g 10/16)
          (horizontal-line g 12/16)
          (.setColor g Color/WHITE)
          (.setStroke g (BasicStroke. 1))
          (horizontal-line g 10/16)
          (horizontal-line g 12/16)

          (doseq [[idx item] (->> items'
                               (take 9)
                               (map-indexed vector))
                  :let [offset (->> idx (- 4) (* (/ see-height 8)))]]
            (if (= idx 3)
              (do
                (.setColor g Color/BLACK)
                (doseq [[x y] [[-2 -2] [2 2] [2 -2] [-2 2]]]
                  (.drawString g (str item "!!!")
                               (int (+ (/ see-width 32) x))
                               (int (+ offset y
                                       (* see-height 1/2)
                                       (* (/ see-height 8) position')))))
                (.setColor g (Color. 255 255 100))
                (.drawString g (str item "!!!")
                             (int (/ see-width 32))
                             (int (+ offset
                                     (* see-height 1/2)
                                     (* (/ see-height 8) position')))))
              (do
                (.setColor g (Color. 200 200 200))
                (.drawString g (str item)
                             (int (/ see-width 32))
                             (int (+ offset
                                     (* see-height 1/2)
                                     (* (/ see-height 8) position')))))))
          (refresh-fn)
          (if (< linger' 80)
            (do
              (Thread/sleep 30)
              (recur (+ position' speed)
                     (+ speed acceleration)
                     items' item' linger'))
            (first (drop 3 items))))))
    (do (println "And last but by no means least...")
        (first coll))))

(defn who's-next? []
  (let [{:keys [next-player remaining]} @data]
    (if (or next-player (seq remaining))
      (do (when-not next-player
            (let [next-player (wheel-of-random remaining)]
              (swap!
                data
                (fn [d]
                  (-> d
                      (assoc :next-player next-player)
                      (assoc :remaining (remove #(= next-player %) remaining)))))))
          #_(println (:next-player @data) "to play!")
          (-> @data next-player-string println))
      (println "The game is over!"))))

(defn help []
  (println "(initialise \"path/to/files\") to start a game")
  (println "(status) to see the current status of the game")
  (println "(who's-next?) to see who's turn it is")
  (println "(pick n) to pick present 'n'")
  (println "(show n) to have a look at present 'n'")
  (println "(help) to show this message"))

(help)


(defn update-remaining [f]
  (swap! data update :remaining f)
  (println (-> @data :remaining pr-str)))

(defn add-remaining [v]
  (update-remaining (fn [r] (cons v r))))

(defn remove-remaining [v]
  (update-remaining (fn [r] (remove #(= % v) r))))

(defn set-next-player [v]
  (swap! data assoc :next-player v)
  (who's-next?))

(defn set-present-status [index new-status]
  (swap! data update :presents
         #(map (fn [present]
                 (if (= index (:index present))
                   (assoc present :status new-status)
                   present))
               %))
  (status))

(defn set-present-owner [index owner]
  (swap! data update :presents
         #(map (fn [present]
                 (if (= index (:index present))
                   (assoc present :owner owner)
                   present))
               %))
  (status))

(comment
  (initialise "/path/to/directory/containing/files")

  ; see wrapped presents
  (->> data deref
       :presents
       (map :wrapped)
       shuffle
       (string/join "\n"))

  ; see data safely
  (-> data
      deref
      (update :presents (fn [presents]
                          (map #(select-keys % [:index :opened :opened-long]) presents)))))
