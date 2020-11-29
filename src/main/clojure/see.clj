(ns see
  (:import [java.awt.image BufferedImage]
           [javax.swing JFrame WindowConstants]
           [java.awt Graphics Color Image Dimension]
           [java.util.concurrent Executors TimeUnit]
           [java.awt.event KeyAdapter KeyEvent WindowAdapter]))

(defn see
  "See a visual representation of an image in a java.awt Window.
  Returns a function to call when the image has been changed.

  Example usage:

  (require '[see :as s])

  (-> (s/definition-for my-java-awt-image)
      (s/with-title \"My Image\")
      (s/with-background-colour Color/YELLOW)
      (s/see))"
  [^Image image & {:keys [^String title
                          ^Color background-colour
                          ^Long fps
                          ^Boolean only-draw-when-updated?
                          key-handler-fn]
                   :or   {title                   "See!"
                          background-colour       (Color. 0 0 0 0)
                          fps                     25
                          only-draw-when-updated? false}}]
  (let [width (.getWidth image nil)
        height (.getHeight image nil)
        buffer (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        frame ^JFrame (proxy [JFrame] []
                        (paint [^Graphics graphics]
                          (let [insets (-> this .getInsets)
                                container (-> this .getContentPane)]
                            #_(.setBackground ^Graphics2D graphics background-colour)
                            #_(.clearRect graphics
                                          (.left insets) (.top insets)
                                          (.getWidth container) (.getHeight container))
                            (.drawImage graphics buffer (.left insets) (.top insets) this))))
        changed? (volatile! true)
        executor (Executors/newSingleThreadScheduledExecutor)]
    (when key-handler-fn
      (.addKeyListener frame (proxy [KeyAdapter] []
                               (keyPressed [^KeyEvent ke]
                                 (key-handler-fn (.getKeyCode ke))))))
    (.scheduleAtFixedRate executor
                          #(when (or @changed? (not only-draw-when-updated?))
                             (vreset! changed? false)
                             (.repaint frame))
                          0 (long (/ 1000 fps)) TimeUnit/MILLISECONDS)
    (doto frame
      (.setTitle title)
      (-> .getContentPane (.setPreferredSize (Dimension. width height)))
      (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
      (.addWindowListener (proxy [WindowAdapter] []
                            (windowClosed [_window-event]
                              (.shutdown executor))))
      (.pack)
      (.setVisible true))
    (fn []
      (.drawImage (.getGraphics buffer) image 0 0 nil)
      (vreset! changed? true))))