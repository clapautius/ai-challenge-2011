;;;; functions/classes related to ants and maps

(defclass ant ()
  ((row :accessor row :initarg :row :initform nil)
   (col :accessor col :initarg :col :initform nil)

   ;; (list task-type target-row target-col)
   ;; :follow-food :attack-hill :away-from-home
   ;; :away-from-home is a pseudo-task, it's not a proper task, it means the ant
   ;; is moving further away from own hill to avoid too many ants near home
   (task :accessor task :initform nil)
   (target-row :accessor target-row)
   (target-col :accessor target-col)
   (task-wait :accessor task-wait :initform 0)

   (route :accessor route :initform nil)))


(defmethod print-object ((ant ant) stream)
  (format stream "<ANT:(~a ~a) t=~a>" (row ant) (col ant) (task ant)))


(defgeneric ant-move (ant direction))

(defmethod ant-move ((ant ant) direction)
  "Move ant from the current position to the specified dir."
  (ant-move-from-pos (row ant) (col ant) direction))


(defun ant-move-from-pos (row col dir)
  "Move ant from (row, col) to the specified dir. Does not check if move if
  acceptable."
  (issue-order row col dir)
  (let* ((nl (new-location row col dir))
         (new-row (first nl))
         (new-col (second nl))
         (cur-ant (get-ant-at row col)))
    (log-output "cmd ~a from (~a ~a) to (~a ~a), internal-state at dest: ~a~%"
                dir row col new-row new-col *cur-turn*)
    (set-entity-at row col nil 0)
    (set-entity-at new-row new-col nil 100)
    (set-value-at *ant-map* row col nil nil)
    (set-value-at *ant-map* new-row new-col nil cur-ant)
    (set-value-at *internal-state* row col dir *cur-turn*)
    (setf (my-ants *state*)
          (delete-if (lambda (elt) (and (= (row elt) (row cur-ant))
                                        (= (col elt) (col cur-ant))))
                     (my-ants *state*)))))


(defun get-ant-at (row col &optional direction)
  "Return ant object at (row col) (with direction, if specified)."
  (if direction
      (let ((nl (new-location row col direction)))
        (aref *ant-map* (elt nl 0) (elt nl 1)))
      (aref *ant-map* row col)))


(defun set-ant-at (row col value &optional direction)
  "Set an ant object at the specified coords. (row col) (with direction, if
specified). If there's already an ant, it is overwriten."
  (if direction
      (let ((nl (new-location row col direction)))
        (setf (aref *ant-map* (elt nl 0) (elt nl 1)) value))
      (setf (aref *ant-map* row col) value)))


(defun own-ant-p (row col &optional (direction nil))
  "Returns T if the tile in the DIRECTION of ROW,COL is own ant, otherwise
  returns NIL."
  (= 100 (get-entity-at row col direction)))


(defun own-free-ant-p (row col &optional (direction nil))
  "Returns T if the tile in the DIRECTION of ROW,COL is own ant and the ant is
  free, otherwise returns NIL."
  (let* ((nl (if direction (new-location row col direction) (list row col)))
         (r (first nl)) (c (second nl))
         (elt (aref (game-map *state*) r c))
         (ant (get-ant-at row col direction)))
    (and (= 100 elt) (and ant (or (null (task ant))
                                  (eql (task ant) :away-from-home))))))


(defun remove-dead-ant (row col)
  "Remove own dead ant from lists and maps."
  (let ((dying-ant (get-ant-at row col)))
    (when (eql (task dying-ant) :follow-hill)
      (log-output "attacking ant at (~a ~a) died~%" row col)
      (decf (attack-ants *state*))))
  (set-ant-at row col nil)
  (remove-if (lambda (ant) (and (= (row ant) row)
                                (= (col ant) col)))
             (my-ants *state*)))


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



(defun get-entity-at (row col &optional direction)
  (if direction
      (let ((nl (new-location row col direction)))
        (aref (game-map *state*) (elt nl 0) (elt nl 1)))
      (aref (game-map *state*) row col)))


(defun set-entity-at (row col direction value)
  "direction may be null"
  (if direction
      (let ((nl (new-location row col direction)))
        (setf (aref (game-map *state*) (elt nl 0) (elt nl 1)) value))
      (setf (aref (game-map *state*) row col) value)))


(defun explore-potential (r c)
  "Return a value representing the explore potential of the cell (small is
better)."
  (let* ((e (get-entity-at r c))
         (p (cond
              ((= e 1) (1- *cur-turn*)) ; water
              ((own-hill-p r c) (* 2 *cur-turn*)) ; own hill
              ;((enemy-hill-p r c) (- *cur-turn*)) ; enemy hill - destroy
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


(defun ant-give-a-task (ant row col task dir-list)
  "Give the ant a set of coords, a task and a list of directions.
Calls ant-move()."
  (cond
    (task
     (setf (target-row ant) row)
     (setf (target-col ant) col)
     (setf (task ant) task)
     (setf (task-wait ant) 0)
     (setf (route ant) dir-list)
     (ant-move-towards-target ant)
     (when (eql task :follow-hill)
       (incf (attack-ants *state*))))
    (t
     (setf (task ant) nil)
     (setf (task-wait ant) 0)
     (setf (route ant) nil))))


(defun ant-move-towards-target (ant)
  "..."
  (let ((r (row ant)) (c (col ant)))
    (log-output "ant has a task: ~a~%" (task ant))
    (cond
     ;; ant reached its target
     ((and (= r (target-row ant)) (= c (target-col ant)))
      (log-output "ant has reached its target~%")
      (when (eql (task ant) :follow-hill)
        ;; remove enemy hill from list
        (setf (hills *state*) (delete-if (lambda (hill)
                                           (and (= r (first hill))
                                                (= c (second hill))))
                                         (hills *state*)))
        ;; clear task for other ants targeting the same hill
        (mapc (lambda (a)
                (when (and (eql (task a) :follow-hill)
                           (= (target-row a) r)
                           (= (target-col a) c))
                  (ant-give-a-task a 0 0 nil nil)))
              (my-ants *state*)))
      (ant-give-a-task ant 0 0 nil nil))
     ;; move ahead
     (t
      (let ((dir (first (route ant))))
        (cond
          ;; the move is acceptable
          ((move-acceptable-p r c dir)
           (log-output "move according to route, dir=~a~%" dir)
           (setf (task-wait ant) 0)
           (ant-move-from-pos r c dir)
           (setf (route ant) (cdr (route ant))))
          ;; the move is impossible - abandon
          ((move-impossible-p r c dir)
           (log-output "move according to route is impossible (dir=~a)~%" dir)
           (ant-give-a-task ant 0 0 nil nil))
          ;; the move is not acceptable atm
          (t
           (log-output "cannot move according to route atm (dir=~a)~%" dir)
           (incf (task-wait ant))
           (when (> (task-wait ant) 3)
             (log-output "waited too long, aborting task")
             (ant-give-a-task ant 0 0 nil nil)))))))))


(defun do-ant (ant)
  "Do something with ant at coord. (r, c)."
  (let ((r (row ant)) (c (col ant)))
  (log-output "doing something with ant at (~a ~a)~%" r c)
  (when (task ant)
    (ant-move-towards-target ant))

  ;; no task assigned to ant
  (when (null (task ant))
    (let* (;(near-home (nearest-own-hill r c))
           (explore t)
           dir)
      ;;(when near-home
        ;;(when (move-near-home ant near-home near-home-area)
          ;;(setf explore nil))) ; found a strategy - don't explore
      (when explore
        (setf dir (move-explore r c)))
      (if dir
          (ant-move-from-pos r c dir)
          (set-value-at *internal-state* r c dir *cur-turn*))))))


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
  (let* (last-selected-r last-selected-c)
    (bfs row col (lambda (r c)
                   (setf last-selected-r r)
                   (setf last-selected-c c)
                   nil)  ; return nil to continue search
         search-area t)
    (log-output "furthest-point-from=(~a ~a)~%" last-selected-r last-selected-c)
    (list last-selected-r last-selected-c)))


(defun move-acceptable-p (row col dir)
  "Return true if the move is acceptable"
  (let ((ent (get-entity-at row col dir)))
    (and (/= 1 ent) ; not water
         (/= 300 ent) ; not own hill
         (/= 100 ent) ; not own ant
         (/= (value-at *internal-state* row col dir) *cur-turn*))))


(defun move-impossible-p (row col dir)
  "Return true if the move is impossible"
  (waterp row col dir))
  

(defun move-explore (r c)
  "Select a cell that was not visited before or was visited a long time ago"
  (log-output "explore from (~a ~a)~%" r c)
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
  (setf (slot-value *state* 'near-home-area) search-area)
  ;(log-output "hills=~a~%" (hills *state*)) ; :tmp:
  (dolist (hill (hills *state*))
    ;(log-output-array (game-map *state*) (first hill) (second hill) 5) ; :tmp:
    (when (own-hill-p (first hill) (second hill)) ; :fixme: - optimize
      (compute-furthest-points hill (+ 5 search-area))

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
        (if furthest-point
            (progn
              (log-output "furthest point from (~a ~a) is (~a ~a) ~%"
                          hill-r hill-c r c)
              (setf (nth 3 hill) r)
              (setf (nth 4 hill) c))
          (log-output "could not compute furthest point prom (~a ~a)~%"
                      hill-r hill-c))
        ret))


(defun move-near-home (ant hill near-home-area)
  "Move an ant near home (tricky). Return nil if it can't find a strategy."
  (log-output "explore near home, from (~a ~a), hill=~a~%"
              (row ant) (col ant) hill)
  ;; Move towards the furthest point or the usual way if no such point
  ;; computed yet.
  (when (nth 3 hill) ; we have a furthest point
      (let ((path (find-path (row ant) (col ant) (nth 3 hill) (nth 4 hill)
                             (+ 5 near-home-area))))
        (cond
         (path
          (log-output "Path from ant to furthest point is ~a~%" path)
          (ant-give-a-task ant (nth 3 hill) (nth 4 hill) :away-from-home path)
          t) ; return t - we have a strategy
         (t
          (log-output "Cannot find path to furthest point.~%")
          nil))))) ; return nil - we don't have a strategy


(defun find-path (row1 col1 row2 col2 &optional search-area)
  "Find path from (row1, col1) to (row2, col2). Return a list with directions."
  (bfs-path row1 col1 (lambda (r c)
                        (and (= r row2) (= c col2)))
            search-area))


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

  (defun mark-visited (row-or-coords &optional col)
    "Mark the position as visited (with the current visited-val)."
    (if (numberp row-or-coords)
      (setf (aref visited-array row-or-coords col) visited-val)
      (setf (aref visited-array (first row-or-coords) (second row-or-coords))
            visited-val)))
        

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
      ;; :tmp:
      ;;(log-output "bfs: remove (~a ~a) from frontier~%" elt-row elt-col)
      (incf search-size)
      (when (or (<= search-size 1) ; don't check first cell
                (null avoid-water)
                (and avoid-water
                     (not (waterp elt-row elt-col :north))
                     (not (waterp elt-row elt-col :south))
                     (not (waterp elt-row elt-col :east))
                     (not (waterp elt-row elt-col :west))))
        (for-each-dir-do elt-row elt-col 
                         (lambda (r c)
                           (when (and (not (visited-p r c)) (not (waterp r c )))
                             (setf frontier (append frontier (list (list r c))))
                             ;; :tmp:
                             ;;(log-output "bfs: add (~a ~a) to frontier~%" r c)
                             (mark-visited r c))
                           nil))))))


(defun bfs-path (row col target-p &optional max-search acceptable-p)
  "Breadth-first search. target-p must accept two parameters, row and col.
acceptable-p must accept three parameters: row, col, dir.
Return path to target (list of directions) or nil if no such path exists."
  (let* ((frontier (list (list (cons row col))))
         (search-size 0))
    (init-visited)
    (log-output "bfs-path(~a, ~a, .., ~a)~%" row col max-search)
    (mark-visited row col)
    (do* ((elt (first frontier) (first frontier))
          (cur-row (car (first elt)) (car (first elt)))
          (cur-col (cdr (first elt)) (cdr (first elt))))
         ;; exit when frontier is empty or search-size exceeded max-search
         ((or (null frontier) (and max-search (> search-size max-search))))
      ;; :tmp:
      ;;(format t "bfs-path: elt=~a, cur-pos=(~a ~a)~%" elt cur-row cur-col)
      (when (funcall target-p cur-row cur-col)
        (return-from bfs-path (nreverse (cdr elt))))
      (setf frontier (cdr frontier))
      ;; :tmp:
      ;;(log-output "bfs-path: remove ~a from frontier~%" elt)
      (incf search-size)
      (when (or (<= search-size 1) ; first cell is always acceptable
                (null acceptable-p)
                (and acceptable-p
                     (funcall acceptable-p cur-row cur-col)))
        (dolist (dir '(:north :south :east :west))
          (let* ((nl (new-location cur-row cur-col dir))
                 (new-row (first nl))
                 (new-col (second nl)))
            (when (and (not (visited-p new-row new-col))
                       (not (waterp new-row new-col)))
              (setf frontier (append frontier 
                                     (list (append (list (cons new-row new-col)
                                                         dir) (cdr elt)))))
              ;; :tmp:
              ;;(log-output "bfs-path: added something, frontier=~a~%" frontier)
              (mark-visited new-row new-col))))))))

  
(defun ant-targeting-cell-p (row col task)
  "Return true if there is an ant going towards the specified cell with the
  specified task."
  (some (lambda (ant)
          (and (eql (task ant) task)
               (= (target-row ant) row)
               (= (target-col ant) col)))
        (my-ants *state*)))


(defun no-ants-targeting-cell (row col task)
  "Return number of ants going towards the specified cell with the
  specified task."
  (count-if (lambda (ant)
              (and (eql (task ant) task)
                   (= (target-row ant) row)
                   (= (target-col ant) col)))
            (my-ants *state*)))

  
(defun target-food (cells)
  "Find ants close to food. Move ants towards food."
  (log-output "Targeting food~%")
  (dolist (food (food *state*))
    (let ((r (first food))
          (c (second food)))
      ;; check if there's an ant trying to get this food
      (unless (ant-targeting-cell-p r c :follow-food)
        (log-output "Trying to find ant for food at (~a ~a)~%" r c)
        (let ((ret (bfs r c 'own-free-ant-p cells)))
          (when ret
            (let ((ret-r (first ret)) (ret-c (second ret)))
              (log-output "Found an ant for food: (~a ~a)~%" ret-r ret-c)
              (let ((path (find-path ret-r ret-c r c cells)))
                (when path
                  (log-output "Path from ant to food is ~a~%" path)
                  (ant-give-a-task (get-ant-at ret-r ret-c) r c
                                   :follow-food path))))))))))


;(defun target-enemy-hills (steps &optional (no-ants 1))
(defun target-enemy-hills (area ants-per-hill)
  "Find ants close to enemy hill. Move ants towards enemy hill."
  (log-output "Targeting enemy hills~%")
  (dolist (hill (hills *state*))
    (when (/= (third hill) 0) ; enemy hill
      (let ((r (first hill))
            (c (second hill)))
        ;; check no. of ants trying to get to this hill
        (when (< (no-ants-targeting-cell r c :follow-hill) ants-per-hill)
          (log-output "Trying to find ant for hill at (~a ~a)~%" r c)
          (let ((ret (bfs r c 'own-free-ant-p area)))
            (when ret
              (let ((ret-r (first ret)) (ret-c (second ret)))
                (log-output "Found an ant for hill: (~a ~a)~%" ret-r ret-c)
                (let ((path (find-path ret-r ret-c r c area)))
                  (when path
                    (log-output "Path from ant to hill is ~a~%" path)
                    (ant-give-a-task (get-ant-at ret-r ret-c) r c
                                     :follow-hill path)))))))))))
