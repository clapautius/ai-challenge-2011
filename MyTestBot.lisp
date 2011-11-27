;;;; MyTestBot.lisp - test functions
;;;;

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


;;;; Tests

(defun test-dfs ()
  "Run tests for depth-first search."
  (setf (rows *state*) 3)
  (setf (cols *state*) 3)
  (setf (game-map *state*) (make-array (list 3 3)
                                       :initial-contents
                                       '((0 0 0)
                                         (0 0 0)
                                         (0 0 0))))
  (format t "arr1=~a~%test1=~a~%" (game-map *state*) (find-path 0 0 2 2))
 
  (setf (game-map *state*) (make-array (list 3 3)
                                       :initial-contents
                                       '((0 1 0)
                                         (0 1 0)
                                         (0 0 0))))
  (format t "arr2=~a~%test2=~a~%" (game-map *state*) (find-path 0 0 2 2))

  (setf (game-map *state*) (make-array (list 3 3)
                                       :initial-contents
                                       '((0 1 0)
                                         (0 0 0)
                                         (0 1 0))))
  (format t "arr3=~a~%test3=~a~%" (game-map *state*) (find-path 0 0 2 2))
  )


(defun test-bfs ()
  "Run tests for breadth-first search."
  (setf (rows *state*) 3)
  (setf (cols *state*) 3)
  (setf (game-map *state*) (make-array (list 3 3)
                                       :initial-contents
                                       '((0 0 0)
                                         (0 0 0)
                                         (0 0 100))))
  (let ((ret (bfs 0 0 'own-ant-p)))
    (format t "test1: bfs=(~a, ~a)~%" (first ret) (second ret)))
  ;; limit search size
  (let ((ret (bfs 0 0 'own-ant-p 5)))
    (format t "test2: bfs=(~a, ~a)~%" (first ret) (second ret)))

  )


(defun test-run-1 ()
  "Test a sigle loop."
  ;; init ant data
  (setf (slot-value *state* 'rows) 8)
  (setf (slot-value *state* 'cols) 8)
  (setf (slot-value *state* 'game-map)
        (make-array (list (rows *state*) (cols *state*)) :element-type 'fixnum
                    :initial-contents '((0 0 0 0 0 0 0 0)
                                        (0 0 0 0 0 0 0 0)
                                        (0 0 0 0 0 0 0 0)
                                        (0 0 0 0 0 0 0 0)
                                        (0 0 0 0 0 0 0 0)
                                        (0 0 0 0 0 0 0 0) ; r 5
                                        (0 0 0 0 0 0 0 0)
                                        (0 0 0 0 0 0 0 0))))
  (setf (slot-value *state* 'turn-start-time) (wall-time))
  (reset-some-state)
  (init-internal-data (rows *state*) (cols *state*))
  (set-food "f 5 3")
  (set-ant "a 4 4 0");

  ;; test furthest point
  (format t "furthest point from (0 0) with max-search 16 is ~a~%"
          (furthest-point-from 0 0 16))
  ;; do a turn
  (do-turn))
