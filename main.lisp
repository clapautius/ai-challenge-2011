;;;; main.lisp

(load "ants.lisp")

;; release
(defparameter *debug* nil)

;;; Functions

;; This is the actual 'AI'.  Very simple currently: loops through each of your
;; ants and issues an order to go either north, east, south or west if the tile
;; in the direction is not a water tile.
(defun do-turn ()
  (loop ;; You don't have to reverse the MY-ANTS list, but we did this to get
        ;; the same output as most other starter bots when called with example
        ;; input from the wiki.
     for ant in (my-ants *state*)
     for row = (elt ant 0)
     for col = (elt ant 1)
       
     do (do-ant row col)

     ;;do (cond ((not (water? row col :north)) (issue-order row col :north))
     ;;         ((not (water? row col :east))  (issue-order row col :east))
     ;;         ((not (water? row col :south)) (issue-order row col :south))
     ;;         ((not (water? row col :west))  (issue-order row col :west)))
       )
  (incf *cur-turn*))


(defun main ()
  "Main game loop: parses the (initial) game state and calls DO-TURN and
  FINISH-TURN."
  (if *debug*
    (setf *log-output* (open "output.log" :direction :output :if-exists :append
                             :if-does-not-exist :create))
    (setf *log-output* nil))
  (handler-bind ((sb-sys:interactive-interrupt #'user-interrupt))
    (loop while (handler-case (peek-char nil *standard-input* nil)
                  (sb-int:simple-stream-error nil))
       for end-of-game-p = (parse-game-state)
       when end-of-game-p do (loop-finish)
       do (do-turn)
         (finish-turn))))
