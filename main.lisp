;;;; main.lisp

;;; :release:
(defparameter *debug* t)

(load "bot.lisp")
(load "ants.lisp")

;;; main loop

(defun main ()
  "Main game loop: parses the (initial) game state and calls DO-TURN and
  FINISH-TURN."
  (if *debug*
    (setf *log-output* (open "output.log" :direction :output :if-exists :append
                             :if-does-not-exist :create))
    (setf *log-output* nil))
  (log-output "I'm alive~%")
  (log-output-finish)
  (handler-bind ((sb-sys:interactive-interrupt #'user-interrupt))
    (loop while (handler-case (peek-char nil *standard-input* nil)
                  (sb-int:simple-stream-error nil))
       for end-of-game-p = (parse-game-state)
       when end-of-game-p do (loop-finish)
       do (do-turn)
         (finish-turn))))
