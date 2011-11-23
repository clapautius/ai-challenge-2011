;;;; MyBot.lisp
;;;;
;;;; A wrapper to compile mybot.lisp and ants.lisp into MyBot.

;; This seems to work for now.

;(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
;(declaim (sb-ext:muffle-conditions style-warning))  ; doesn't work
;(declaim (sb-ext:muffle-conditions warning))        ; idem
;(setf sb-ext:*muffled-warnings* 'style-warning)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "main.lisp"))

(defun run-test (rows cols)
  "..."
  ;; init ant data
  (setf (slot-value *state* 'rows) rows)
  (setf (slot-value *state* 'cols) cols)
  (setf (slot-value *state* 'game-map)
        (make-array (list rows cols) :element-type 'fixnum :initial-element 0))
  (setf (aref (slot-value *state* 'game-map) 3 3) 100)
  (setf (slot-value *state* 'my-ants) (list (list 3 3)))

  (init-internal-data rows cols)
  (loop for i from 1 to 5
       do (do-turn)))
