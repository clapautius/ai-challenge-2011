;;;; functions/classes related to ants and maps

(defclass ant ()
  ((row :accessor row :initform nil)
   (col :accessor col :initform nil)
   (has-task-p :accessor has-task-p :initform nil)))


(defun distance (row1 col1 row2 col2)
  "Returns the shortest distance between ROW1,COL1 and ROW2,COL2 for a grid
  that wraps around."
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (- (rows *state*) drow)))
         (mincol (min dcol (- (cols *state*) dcol))))
    (sqrt (+ (* minrow minrow) (* mincol mincol)))))


;; TODO needs better docstring (needs better code as well!)
(defun new-location (row col direction)
  "Returns '(NEW-ROW NEW-COL) for ROW,COL and DIRECTION for a grid that
  wraps around."
  (let ((dst-row (cond ((equal direction :north)
                        (if (<= row 0)
                            (- (rows *state*) 1)
                            (- row 1)))
                       ((equal direction :south)
                        (if (>= (+ row 1) (rows *state*))
                            0
                            (+ row 1)))
                       (t row)))
        (dst-col (cond ((equal direction :east)
                        (if (>= (+ col 1) (cols *state*))
                            0
                            (+ col 1)))
                       ((equal direction :west)
                        (if (<= col 0)
                            (- (cols *state*) 1)
                            (- col 1)))
                       (t col))))
    (list dst-row dst-col)))


;;; :fixme:
(defun new-loc-row (row col direction)
  (first (new-location row col direction)))
(defun new-loc-col (row col direction)
  (second (new-location row col direction)))


(defun normalize-loc (row col)
  "Normalize the row and col to values between 0 and max width / height."
  (let ((new-row row)
        (new-col col))
    (when (minusp row)
      (setf new-row (+ (rows *state*) row)))
    (when (>= row (rows *state*))
      (setf new-row (- row (rows *state*))))
    (when (minusp col)
      (setf new-col (+ (cols *state*) col)))
    (when (>= col (cols *state*))
      (setf new-col (- col (cols *state*))))
    (list new-row new-col)))


(defun waterp (row col &optional (direction nil))
  "Returns T if the tile in the DIRECTION of ROW,COL is water, otherwise
  returns NIL."
  (if direction
      (let ((nl (new-location row col direction)))
        (= 1 (aref (game-map *state*) (elt nl 0) (elt nl 1))))
      (= 1 (aref (game-map *state*) row col))))


(defun own-ant-p (row col &optional (direction nil))
  "Returns T if the tile in the DIRECTION of ROW,COL is own ant, otherwise
  returns NIL."
  (let* ((nl (if direction (new-location row col direction) (list row col)))
         (r (first nl)) (c (second nl))
         (elt (aref (game-map *state*) r c)))
    (or (= 100 elt)
        (= 101 elt))))


(defun own-free-ant-p (row col &optional (direction nil))
  "Returns T if the tile in the DIRECTION of ROW,COL is own ant and the ant is
  free, otherwise returns NIL."
  (let* ((nl (if direction (new-location row col direction) (list row col)))
         (r (first nl)) (c (second nl))
         (elt (aref (game-map *state*) r c)))
    (= 100 elt)))


(defun get-entity-at (row col &optional (direction nil))
  (if direction
      (let ((nl (new-location row col direction)))
        (aref (game-map *state*) (elt nl 0) (elt nl 1)))
      (aref (game-map *state*) row col)))



(defun explore-potential (r c)
  "Return a value representing the explore potential of the cell (small is
better)."
  (let* ((e (get-entity-at r c))
         (p (cond
              ((= e 1) (1- *cur-turn*)) ; water
              ((own-hill-p r c) (* 2 *cur-turn*)) ; own hill
              ((enemy-hill-p r c) (- *cur-turn*)) ; enemy hill - destroy
              ((= e 200) (1+ *cur-turn*)) ; enemy ant - run, forrest, run
              ;((= e 100) (1+ *cur-turn*)) ; own ant
              ((= e 2) (- *cur-turn*)) ; food
              (t (aref *internal-state* r c)))))
    ;(log-output "potential of (~a ~a)=~a~%" r c p) ; :tmp:
    p))


(defun for-each-dir-sum (row col direction func
                         &optional (incl-current nil))
  "Call func with r & c params each direction: north, south, east, west. 
Returns a list with results."
  (let ((r row) (c col))
    (when direction
      (let ((nl (new-location row col direction)))
        (setf r (first nl))
        (setf c (second nl))))
    (+ (funcall func (new-loc-row r c :north) (new-loc-col r c :north))
       (funcall func (new-loc-row r c :south) (new-loc-col r c :south))
       (funcall func (new-loc-row r c :east) (new-loc-col r c :east))
       (funcall func (new-loc-row r c :west) (new-loc-col r c :west))
       (if incl-current
           (funcall func r c)
           0))))


(defun for-each-dir-do (row col func
                        &optional (incl-current nil))
  "Call func with row and col params each direction: north, south, east, west.
If func return non-nil value, the for-each-dir-do returns with that value."
  (dolist (dir '(:north :south :east :west))
    (let ((ret (funcall func (new-loc-row row col dir)
                        (new-loc-col row col dir))))
      (when ret
        (return-from for-each-dir-do ret))))
  (when incl-current
    (let ((ret (funcall func row col)))
      (when ret
        (return-from for-each-dir-do ret)))))


(defun move-ant (row col dir)
  "Move ant from (row, col) to the specified dir. Remove it from my-ants list."
  (log-output "cmd ~a from (~a ~a), internal-state at dest: ~a~%"
              dir row col *cur-turn*)
  (issue-order row col dir)
  (set-value-at *internal-state* row col dir *cur-turn*)
  (set-value-at (game-map *state*) row col nil 101) ; make ant busy
  (remove-if (lambda (elt) (and (= row (first elt)) (= col (second elt))))
             (my-ants *state*)))


(defun do-ant (r c near-home-area)
  "Do something with ant at coord. (r, c)."
  (log-output "doing something with ant at (~a ~a)~%" r c)
  (let* ((near-home (nearest-own-hill r c))
         (dir (if near-home
                  (move-near-home r c near-home near-home-area)
                  (move-explore r c))))
    (if dir
        (move-ant r c dir)
        (set-value-at *internal-state* r c dir *cur-turn*))))


(defun value-at (array row col dir)
  "..."
  (let ((nl (new-location row col dir)))
    (aref array (first nl) (second nl))))


(defun set-value-at (array row col dir value)
  "dir may be nil"
  (let ((r row) (c col))
    (when dir
      (let ((nl (new-location row col dir)))
        (setf r (first nl))
        (setf c (second nl))))
    (setf (aref array r c) value)))


(defun own-hill-p (row col &optional direction)
  "Returns t if the tile in the DIRECTION of ROW,COL is own hill, otherwise
  returns NIL."
  (if direction
      (let ((nl (new-location row col direction)))
        (= 300 (aref (game-map *state*) (elt nl 0) (elt nl 1))))
      (= 300 (aref (game-map *state*) row col))))


(defun enemy-hill-p (row col &optional dir)
  "..."
  (let* ((r row) (c col))
    (when dir
      (setf r (new-loc-row row col dir))
      (setf c (new-loc-col row col dir)))
    (some (lambda (hill) (and (= r (first hill))
                              (= c (second hill))
                              (plusp (third hill))))
          (hills *state*))))


(defun nearest-own-hill (row col)
  "Return the hill if the specified coord. are close to some own hill. Uses
*policy-array*." 
  (log-output "Checking if (~a ~a) is near home~%" row col)
  (value-at *policy-array* row col nil))


(defun furthest-point-from (row col search-area)
  "Return the furthest point from (row, col)"
  (let* ((max-distance 0)
         max-r max-c)
    (bfs row col (lambda (r c)
                   (when (< max-distance (distance r c row col))
                     (setf max-distance (distance r c row col))
                     (setf max-r r)
                     (setf max-c c))
                   nil) ; return nil to continue search
         search-area t)
    (when (plusp max-distance)
      (list max-r max-c))))


(defun move-acceptable-p (row col dir)
  "Return true if the move is acceptable"
  (let ((ent (get-entity-at row col dir)))
    (and (/= 1 ent) ; not water
         ;(not (own-hill-p row col dir)) ; not own hill
         (/= (value-at *internal-state* row col dir) *cur-turn*))))
  

(defun move-explore (r c)
  "Select a cell that was not visited before or was visited a long time ago"
  (log-output "explore from (~a ~a), *cur-turn*=~a~%" r c *cur-turn*)
  ;; :tmp:
  ;;(log-output "map:~%~a~%internal-state:~%~a~%"
  ;;        (game-map *state*)
  ;;        *internal-state*)

  (let* ((pref-dir-list nil)
         (pref-dir (mod (truncate (/ *cur-turn* 64)) 4)))
    (cond
      ((= pref-dir 0) (setf pref-dir-list '(:north :east :south :west)))
      ((= pref-dir 1) (setf pref-dir-list '(:east :south :west :north)))
      ((= pref-dir 2) (setf pref-dir-list '(:south :west :north :east)))
      ((= pref-dir 3) (setf pref-dir-list '(:west :north :east :south))))

    (log-output "pref-dir=~a, pref-dir-list=~a ~%" pref-dir pref-dir-list)

    (let ((min (* 16 *cur-turn*))
          (dir nil))
      (dolist (try-dir pref-dir-list)
        (when (move-acceptable-p r c try-dir)
          (let ((val (for-each-dir-sum r c try-dir 'explore-potential t)))
            (log-output "potential at ~a: ~a~%" try-dir val)
            (when (< val min)
              (setf dir try-dir)
              (setf min val)))))
      (log-output "best direction is ~a~%" dir)
      dir)))


(defun setup-home-area (search-area)
  "Compute furthest points, setup local policy around home."
  (log-output "setting up home area (search area=~a)~%" search-area)
  (dolist (hill (hills *state*))
    (when (own-hill-p (first hill) (second hill)) ; :fixme: - optimize
      (compute-furthest-points hill (round (* 0.9 search-area)))

      ;; mark area close to home
      (let ((hill-r (first hill))
            (hill-c (second hill)))
        (bfs hill-r hill-c
             (lambda (r c)
               (set-value-at *policy-array* r c :none hill)
               nil)
             search-area)
        (log-output-array *policy-array* hill-r hill-c
                          (round (* 0.25 search-area)))))))


(defun compute-furthest-points (hill search-area)
  "Compute furthest points from hill. Return the furthest point."
      (let* ((hill-r (first hill)) (hill-c (second hill))
             (furthest-point (furthest-point-from hill-r hill-c search-area))
             (r (first furthest-point))
             (c (second furthest-point))
             (ret (if (and r c) (list r c) nil)))
        (when furthest-point
          (log-output "furthest point from (~a ~a) is (~a ~a) ~%"
                      hill-r hill-c r c)
          (setf (nth 3 hill) r)
          (setf (nth 4 hill) c))
        ret))


(defun move-near-home (r c hill near-home-area)
  "Move an ant near home (tricky)."
  (log-output "explore near home, from (~a ~a), hill=~a~%" r c hill)
  ;; Move towards the furthest point or the usual way if no such point
  ;; computed yet.
  (if (nth 3 hill) ; we have a furthest point
      (let ((path (find-path r c (nth 3 hill) (nth 4 hill)
                             (* 0.6 near-home-area))))
        (cond
         ((and path (move-acceptable-p r c (first path)))
          (log-output "Path from ant to furthest point is ~a~%" path)
          (first path))
         (t
          (log-output "Cannot find path to furthest point.~%")
          (move-explore r c))))
    ;; else we don't have a furthest point - move the usual way
    (move-explore r c)))


(defun dfs (row col target-row target-col depth &key max-depth no-access-p
            dir-list no-access-first-steps-p)
  "Depth-first search"
  ;;(log-output "dfs(~a ~a ~a)~%" row col depth) ; :tmp:
  ;;(log-output "dfs visited-p=~a, no-access-p=~a~%"
  ;;            (visited-p row col) (funcall no-access-p row col)) ; :tmp:
  (let ((no-access (if (and no-access-first-steps-p (> depth 0) (< depth 3))
                       (funcall no-access-first-steps-p row col)
                     (and no-access-p (funcall no-access-p row col)))))
    (when (or (and max-depth (> depth max-depth))
              (visited-p row col)
              no-access)
      (return-from dfs)))
  (mark-visited row col)
  (when (null dir-list)
    (setf dir-list (cond 
                    ((and (< row target-row) (< col target-col))
                     '(:south :east :north :west))
                    ((and (< row target-row) (> col target-col))
                     '(:south :west :north :east))
                    ((and (< row target-row) (= col target-col))
                     '(:south :north :east :west))
                    ((and (= row target-row) (< col target-col))
                     '(:east :west :north :south))
                    ((and (= row target-row) (> col target-col))
                     '(:west :east :north :south))
                    ((and (> row target-row) (< col target-col))
                     '(:north :east :south :west))
                    ((and (> row target-row) (> col target-col))
                     '(:north :west :south :east))
                    ((and (> row target-row) (= col target-col))
                     '(:north :south :east :west))))
    (log-output "new dir-list generated for dfs: ~a~%" dir-list))
  (dolist (dir dir-list)
    (let* ((new-r (new-loc-row row col dir))
           (new-c (new-loc-col row col dir)))
      (when (and (= new-r target-row)
                 (= new-c target-col))
        (return-from dfs (list dir)))
      (let ((rc (dfs new-r new-c target-row target-col (1+ depth)
                     :max-depth max-depth :no-access-p no-access-p
                     :no-access-first-steps-p no-access-first-steps-p
                     :dir-list dir-list )))
        (when rc
          (return-from dfs (cons dir rc)))))))


(defun find-path (row1 col1 row2 col2 &optional max-depth)
  "Find path from (row1, col1) to (row2, col2). Return a list with directions."
  (init-visited)
  (dfs row1 col1 row2 col2 0 :max-depth max-depth
       :no-access-p
       (lambda (r c) (or (waterp r c) (own-hill-p r c)))
       :no-access-first-steps-p
       (lambda (r c) (or (waterp r c) (own-hill-p r c) (own-ant-p r c)))))


(let (visited-array
      visited-val)

  (defun init-visited ()
    "Increment visited-val to be used for the next session (to avoid resetting
    the whole array to 0)."
    (when (null visited-val)
      (setf visited-val 0)
      (setf visited-array (make-array (list (rows *state*) (cols *state*))
                                      :element-type 'fixnum
                                      :initial-element 0)))
    (incf visited-val)
    (when (> visited-val 32766) ; reset the array
      ;; :fixme: - optimize
      (loop for i from 0 to (1- (rows *state*)) do
           (loop for j from 0 to (1- (cols *state*)) do
                (setf (aref visited-array i j) 0)))
      (setf visited-val 1)))

  (defun mark-visited (row col)
    "Mark the position as visited (with the current visited-val)."
    (setf (aref visited-array row col) visited-val))

  (defun visited-p (row col)
    "Return true if the position has been visited."
    (= visited-val (aref visited-array row col)))
)


(defun bfs (row col target-p &optional max-search avoid-water)
  "Breadth-first search. target-p must accept two parameters, row and col."
  (let* ((frontier (list (list row col)))
         (search-size 0))
    (init-visited)
    (log-output "bfs(~a, ~a, .., ~a)~%" row col max-search)
    (mark-visited row col)
    (do* ((elt (first frontier) (first frontier))
          (elt-row (first elt) (first elt))
          (elt-col (second elt) (second elt)))
        ;; exit when frontier is empty or search-size exceeded max-search
        ((or (null frontier) (and max-search (> search-size max-search))))
      (when (funcall target-p elt-row elt-col)
        (return-from bfs (list elt-row elt-col)))
      (setf frontier (cdr frontier))
      ;;(log-output "bfs: remove (~a ~a) from frontier~%" elt-row elt-col);:tmp:
      (incf search-size)
      (for-each-dir-do elt-row elt-col 
                       (if avoid-water
                           (lambda (r c)
                             (when (and (not (visited-p r c)) (not (waterp r c))
                                        (not (waterp r c :north)) (not (waterp r c :south))
                                        (not (waterp r c :east)) (not (waterp r c :west)))
                               (setf frontier (append frontier (list (list r c))))
                               ;;(log-output "bfs: add (~a ~a) to frontier~%" r c);:tmp:
                               (mark-visited r c))
                             nil)
                         (lambda (r c)
                           (when (and (not (visited-p r c)) (not (waterp r c )))
                             (setf frontier (append frontier (list (list r c))))
                             ;;(log-output "bfs: add (~a ~a) to frontier~%" r c);:tmp:
                             (mark-visited r c))
                           nil))))))
  
  
(defun target-food (cells path-len)
  "Find ants close to food. Move ants towards food."
  (log-output "Targeting food~%")
  (dolist (food (food *state*))
    (let ((r (first food))
          (c (second food)))
      (log-output "Trying to find ant for food at (~a ~a)~%" r c)
      (let ((ret (bfs r c 'own-free-ant-p cells)))
        (when ret
          (let ((ret-r (first ret)) (ret-c (second ret)))
            (log-output "Found an ant for food: (~a ~a)~%" ret-r ret-c)
            (let ((path (find-path ret-r ret-c r c path-len)))
              (when path
                (log-output "Path from ant to food is ~a~%" path)
                (move-ant ret-r ret-c (first path))))))))))


(defun target-enemy-hills (steps &optional (no-ants 1))
  "Find ants close to enemy hill. Move ants towards enemy hill."
  (log-output "Targeting enemy hills~%")
  (dolist (hill (hills *state*))
    (when (/= (third hill) 0) ; enemy hill
      (let ((r (first hill))
            (c (second hill)))
        (dotimes (i no-ants)
          (log-output "Trying to find ant for hill at (~a ~a)~%" r c)
          (let ((ret (bfs r c 'own-free-ant-p (expt (* 2 steps) 2))))
            (when ret
              (let ((ret-r (first ret)) (ret-c (second ret)))
                (log-output "Found an ant for hill: (~a ~a)~%" ret-r ret-c)
                (let ((path (find-path ret-r ret-c r c (* 3 steps))))
                  (when path
                    (log-output "Path from ant to hill is ~a~%" path)
                    (move-ant ret-r ret-r (first path))))))))))))
