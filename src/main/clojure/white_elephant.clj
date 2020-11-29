(ns white-elephant
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [cheshire.core :as json]
    [see :as see])
  (:import
    [javax.imageio ImageIO]
    [java.util Base64 Base64$Decoder]
    [java.io ByteArrayInputStream Writer]
    [java.awt.image BufferedImage]
    [java.awt Graphics2D Image Color Font]))

;;; TODO: ;;;
; 1. load the data
; 2. check the data is valid (more in webpage?)
; 4. status page - display the present descriptions, sort by locked? then number
;      n. Status Name Descriptionl
;      1. Stolen  / Bob / A Book
;      2. Wrapped / Bob / A Book
;      3. Opened  /
;      4. Locked  /
; 3. pick a random player
; 5. let player pick present
; 5. detailed display of a single present then back to status page
; 6. persist game state to disk in case of accidents?


(def ^Base64$Decoder decoder (Base64/getDecoder))
(def encoding-prefix "data:image/png;base64,")

(defn import-image [^String data-url]
  (when data-url
    (let [data-string (subs data-url (count encoding-prefix))
          image (ImageIO/read (ByteArrayInputStream. (.decode decoder data-string)))]
      ;(ImageIO/write image "png" (io/file "test.png"))
      image)))

(defmethod print-method BufferedImage [_value ^Writer w]
  (.write w "<<Image>>"))

(defn import-data [directory-name]
  (let [elephants (->> (io/file directory-name)
                       (file-seq)
                       (filter #(re-find #"/white-elephant-.*\.json" (str %)))
                       (map #(do
                               (println "Loading:" (str %))
                               (json/parse-string (slurp %) keyword))))]
    (zipmap (map :name elephants)
            elephants)))

(defonce data (atom {:static-data nil
                     :game-data   nil}))

(defn load-data []
  (let [static-data (import-data "/home/peter/Downloads")]
    (swap! data assoc :static-data static-data)))

(defn check-data []
  (let [players (:remaining (:game-data @data))]
    ; check in web page really?
    (println (count players) "Players:" (string/join ", " players))))

(defn prepare-data []
  (let [elephants (vals (:static-data @data))
        shared (filter #(= "shared" (:shared %)) elephants)
        shared-descriptons (shuffle (map :wrapped shared))
        shared-redescribed (map (fn [el desc] (assoc el :wrapped desc)) shared shared-descriptons)
        not-shared (filter #(= "notshared" (:shared %)) elephants)]
    (swap! data assoc :game-data
           {:remaining   (sort (map :name elephants))
            :next-player nil
            :presents    (mapv (fn [index {:keys [wrapped opened opened-long image1 image2 image3]}]
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
                             (shuffle (concat shared-redescribed not-shared)))})))

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

(defn to-next-status [current-status]
  (->> ["Wrapped" "Opened" "Stolen" "Locked"]
       (drop-while #(not= % current-status))
       (drop 1)
       first))

(defn status []
  (->> @data :game-data :presents
       (sort-by :index)
       (sort-by locked?)
       (map (fn [{:keys [index status owner wrapped opened] :as present}]
              [index status owner (if (wrapped? present) wrapped opened)]))
       tabulate
       (mapv println))
  nil)

(defn initialise []
  (load-data)
  (prepare-data)
  (check-data)
  (status)
  )

(def see-width 400)
(def see-height 400)
(defonce image (BufferedImage. see-width see-height BufferedImage/TYPE_INT_ARGB))
(defonce refresh-fn (see/see image :fps 25 :only-draw-when-updated? true))
(def elephant (ImageIO/read (io/file "white-elephant.png")))


(defrecord Transition [image speed pause])

(def transition-queue (atom []))

(defn enqueue-transition
  ([image] (enqueue-transition image 500 3000))
  ([image speed pause]
   (swap! transition-queue conj (->Transition image speed pause))))

(defonce _render_thread_
         (future
           (try
             (swap! transition-queue
                    (fn [[{:keys [image speed pause]} & more]]
                      ;WIP...
                      more))
             (catch Exception e
               ))))

(defn draw-image [^Graphics2D g ^Image image
                  source-x source-y source-width source-height
                  destination-x destination-y destination-width destination-height]
  (.drawImage g image
              destination-x destination-y (+ destination-x destination-width) (+ destination-y destination-height)
              source-x source-y (+ source-x source-width) (+ source-y source-height)
              nil))

(defn show [present-number]
  (let [{:keys [index status owner wrapped opened opened-long images]} (get-in @data [:game-data :presents present-number])
        g ^Graphics2D (.getGraphics image)]
    (.setColor g Color/BLACK)
    (if (= status "Wrapped")
      (println (str "Present " present-number " has not yet been unwrapped!"))
      (future
        (loop [[image & more-images] (concat (take (* 2 (count images)) (cycle images)) [elephant])]
          (when image
            (let [source-x 0
                  source-y 0
                  source-width (.getWidth image)
                  source-height (.getHeight image)
                  scale-factor (min (/ see-width (.getWidth image)) (/ see-height (.getHeight image)))
                  destination-width (* source-width scale-factor)
                  destination-height (* source-height scale-factor)
                  destination-x (/ (- see-width destination-width) 2)
                  destination-y (/ (- see-height destination-height) 2)]
              ;(println source-x source-y source-width source-height "->" destination-x destination-y destination-width destination-height " | " scale-factor (/ see-width (.getWidth image)) (/ see-height (.getHeight image)))
              (doseq [y-offset (concat (range see-height 0 -5) [0])]
                (.fillRect g 0 y-offset see-width see-height)
                #_(.drawImage g ^Image image
                              destination-x (+ destination-y y-offset) dx2 (+ dy2 y-offset)
                              source-x source-y sx2 sy2 nil)
                (draw-image g image
                            source-x source-y source-width source-height
                            destination-x (+ destination-y y-offset) destination-width destination-height)
                (refresh-fn)
                (Thread/sleep (if (seq more-images) 10 50))))
            (Thread/sleep (if (second more-images) 1500 5000))
            (recur more-images)))))
    (println opened)
    (println opened-long))

  ; image 1
  ; short desc
  ; image 2
  ; long desc
  ; image 3
  ; OR wrapped desc
  )

(defn pick [present-number]
  (let [{:keys [next-player last-thief] :as gd} (:game-data @data)
        present (get-in gd [:presents present-number])
        owner (:owner present)
        ;_ (println owner last-thief)
        error (cond
                (locked? present) (str "Present " present-number " has been stolen too many times already!")
                (and owner (= last-thief owner)) (str "You can't steal it straight back!"))]
    (if error
      (println error)
      (do
        (swap! data assoc :game-data
               (-> gd
                   (update-in [:presents present-number :status] to-next-status)
                   (assoc-in [:presents present-number :owner] next-player)
                   (assoc :next-player owner)
                   (assoc :last-thief (when (opened? present) next-player))))
        (show present-number)))))

(defn wheel-of-random [coll]
  (if (< 1 (count coll))
    (loop [xs (cycle (shuffle coll))
           delay 0.1]
      (if (< delay 1200)
        (let [delay' (+ (* delay 1.2) 1)]
          (println (second xs))
          ;(println (second xs))
          (Thread/sleep (int (+ delay 100)))
          (recur (rest xs) delay'))
        (first xs)))
    (do (println "And last but by no means least...")
        (first coll))))

(defn wheel-of-random-2 [coll]
  #_(let [g ^Graphics2D (.getGraphics image)
        length (count coll)]
    (.setFont g (Font. "Helvetica" Font/BOLD (/ see-height 10)))
    (loop [position 0
           speed 0.1
           item (nth coll 0)
           linger 0]

      (.setColor g Color/BLACK)
      (.fillRect g 0 0 see-width see-height)

      (.setColor g Color/WHITE)

      (doseq [[idx x] (->> (cycle coll)
                           (drop (+ (int position) length -3))
                           (take 7)
                           (map-indexed vector))
              :let [offset (-> idx (- 3) (* (/ see-height 6)))]]
        (.drawString g (str x)
                     (int (/ see-width 8))
                     (int (+ offset
                             (* see-height (mod position 1))))))

      (refresh-fn)

      (Thread/sleep 50)

      (let [item' (nth coll (int position))
            point-acceleration (* (- 0.5 (mod position 1)) 1/10)
            fast-friction (* speed -1/100)
            slow-friction (* (Math/signum speed) -0.006)
            acceleration (+ point-acceleration fast-friction slow-friction)
            linger' (if (= item item') (inc linger) 0)]
        (println item position)
        (if (< linger' 100)
          (recur (rem (+ position speed) length)
                 (+ speed acceleration)
                 item' linger')
          item))))


  (let [g ^Graphics2D (.getGraphics image)]
    (.setFont g (Font. "Helvetica" Font/BOLD (/ see-height 10)))
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

      (.setColor g Color/WHITE)
      (.drawString g ">"
                   (int (/ see-width 32))
                   (int (* see-height 7/12)))
      (doseq [[idx x] (->> items'
                           (take 7)
                           (map-indexed vector))
              :let [offset (->> idx (- 3) (* (/ see-height 6)))]]
        (if (= idx 3)
          (.setColor g Color/WHITE)
          (.setColor g Color/GRAY))
        (.drawString g (str x)
                     (int (/ see-width 8))
                     (int (+ offset
                             (* see-height 1/2)
                             (* (/ see-height 6) position')))))

      ;(println (take 7 items) position speed)

      (refresh-fn)

      ;(println (take 7 items) position)


        (if (< linger' 100)
          (do
            (Thread/sleep 30)
            (recur (+ position' speed)
                     (+ speed acceleration)
                     items' item' linger'))
          (first (drop 3 items)))))))

(defn who's-next? []
  (let [{:keys [next-player remaining]} (:game-data @data)]
    (if (or next-player (seq remaining))
      (do (when-not next-player
            (let [next-player (wheel-of-random remaining)] ; TODO: wheel effect
              (swap!
                data
                (fn [d]
                  (-> d
                      (assoc-in [:game-data :next-player] next-player)
                      (assoc-in [:game-data :remaining] (remove #(= next-player %) remaining)))))))
          (println (:next-player (:game-data @data)) "to play!"))
      (println "The game is over!"))))