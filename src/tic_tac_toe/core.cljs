(ns tic-tac-toe.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :as async :refer [chan >! <!]]))

(enable-console-print!)

(defn new-board []
  {1 nil 2 nil 3 nil
   4 nil 5 nil 6 nil
   7 nil 8 nil 9 nil})

(def app-state (atom {:board (new-board)
                      :turn "x"}))

(def next-turn {"x" "o"
                "o" "x"})

(defn draw-piece! [canvas text]
  (let [ctx  (.getContext canvas "2d")
        mark (if (nil? text)
               ""
               text)
        mark-color {"x" "black"
                    "o" "red"
                    ""  "black"}]
    (set! (.-fillStyle ctx) (mark-color mark))
    (set! (.-textAlign ctx) "center")
    (set! (.-font ctx) "48px sans-serif")
    (.fillText ctx mark 40 60)))

(defn make-play [n s state owner]
  (when (nil? s)
    (let [c (om/get-state owner :moves)
          turn (:turn state)]
      (async/put! c [n turn]))))

(defn- matches? [board positions]
  (let [board-vals     (map #(map board %) positions)
        column-matches-x (map (fn [column-vals] (every? #(= "x" %) column-vals))
                              board-vals)
        column-matches-o (map (fn [column-vals] (every? #(= "o" %) column-vals))
                              board-vals)]
    (or (some true? column-matches-x)
        (some true? column-matches-o))))

(defn horizontal? [board]
  (let [positions (partition 3 (range 1 10))]
    (matches? board positions)))

(defn vertical? [board]
  (let [positions [[1 4 7]
                   [2 5 8]
                   [3 6 9]]]
    (matches? board positions)))

(defn diagonal? [board]
  (let [positions [[1 5 9]
                   [3 5 7]]]
    (matches? board positions)))

(defn winner? [board]
  (or (horizontal? board)
      (vertical? board)
      (diagonal? board)))

(defn draw? [board]
  (every? (comp not nil?) (vals board)))

(defn reset-game [app]
  (om/update! app :board (new-board))
  (om/update! app :turn "x"))

(defn piece [[[n s] state] owner]
  (reify
    om/IDidUpdate
    (did-update [_ _ _]
      (let [canvas (om/get-node owner)]
        (draw-piece! canvas s)))
    om/IRender
    (render [_]
      (html [:canvas {:width 100 :height 100 :on-click #(make-play n s state owner)}]))))

(defn board [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:moves (chan)})
    om/IWillMount
    (will-mount [_]
      (go (let [c (om/get-state owner :moves)]
            (while true
              (let [[n turn] (<! c)]
                (om/transact! app :board #(assoc % n turn))
                (om/transact! app :turn #(next-turn turn)))))))
    om/IRenderState
    (render-state [_ {:keys [moves]}]
      (html
       (cond
         (winner? (:board app))
         [:div
          [:h1 (str "Player " (next-turn (:turn app)) " wins!")]
          [:button {:on-click #(reset-game app)} "Restart"]]

         (draw? (:board app))
         [:div
          [:h1 "Draw"]
          [:button {:on-click #(reset-game app)} "Restart"]]

         :else
         [:div
          [:h1 "Tic Tac Toe"]
          [:div#board {:style {:background-image "url(img/board.png)"
                               :background-size "cover"
                               :background-repeat "no-repeat"
                               :background-position "center center"
                               :width 300 :height 300}}
           (for [[n s] (:board app)]
             (om/build piece [[n s] app] {:init-state {:moves moves :turn (:turn app)}}))]])))))

(om/root
 board
 app-state
 {:target (. js/document (getElementById "app"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
