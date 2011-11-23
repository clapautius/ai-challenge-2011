;;;; ants.lisp

;;; State Class

(defclass state ()
  ((rows :reader rows :initform nil)
   (cols :reader cols :initform nil)
   (game-map :reader game-map :initform nil)
   (enemy-ants :reader enemy-ants :initform nil)
   (my-ants :reader my-ants :initform nil)
   (food :reader food :initform nil)
   (hills :reader hills :initform nil)
   (turn-time :reader turn-time :initform 1000)
   (load-time :reader load-time :initform 3000)
   (turn-start-time :reader turn-start-time :initform nil)
   (view-radius2 :reader view-radius2 :initform 93)
   (attack-radius2  :reader attack-radius2 :initform 6)
   (spawn-radius2 :reader spawn-radius2 :initform 6)
   (turns :reader turns :initform nil)
   (turn :reader turn :initform nil)))

;;; Globals

(defvar *state* (make-instance 'state))

;;; Functions

(defun distance (row1 col1 row2 col2)
  "Returns the shortest distance between ROW1,COL1 and ROW2,COL2 for a grid
  that wraps around."
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (- (rows *state*) drow)))
         (mincol (min dcol (- (cols *state*) dcol))))
    (sqrt (+ (* minrow minrow) (* mincol mincol)))))

(defun issue-order (row col direction)
  "Prints a formatted order for ROW,COL and DIRECTION to standard output.
  Silently drops orders when DIRECTION isn't one of :north, :east, :south
  or :west."
  (when (member direction '(:north :east :south :west))
    (format *standard-output* "~&o ~D ~D ~A~%" row col
            (case direction
              (:north "N")
              (:east  "E")
              (:south "S")
              (:west  "W")))))

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
  (if direction
      (let ((nl (new-location row col direction)))
        (= 100 (aref (game-map *state*) (elt nl 0) (elt nl 1))))
      (= 100 (aref (game-map *state*) row col))))


(defun get-entity-at (row col &optional (direction nil))
  (if direction
      (let ((nl (new-location row col direction)))
        (aref (game-map *state*) (elt nl 0) (elt nl 1)))
      (aref (game-map *state*) row col)))


;; maintain WATER? for backwards compatibility
(setf (symbol-function 'water?) #'waterp)

(defun finish-turn ()
  "Prints the \"finish turn\" string to standard output."
  (format *standard-output* "~&go~%")
  (force-output *standard-output*))

(defun par-value (string)
  "Helper function for parsing game state input from the server."
  (parse-integer (subseq string (position #\space string) (length string))))

(defun starts-with (sequence subsequence)
  (let ((sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen (length sequence)))
      (equal (subseq sequence 0 sublen) subsequence))))

(defun parse-game-parameters ()
  "Parses turn 0 game parameters and sets them in *STATE*.  Also creates
  initial game map and assigns it to (GAME-MAP *STATE*)."
  (loop for line = (read-line *standard-input* nil)
        until (starts-with line "ready")
        do (cond ((starts-with line "attackradius2 ")
                  (setf (slot-value *state* 'attack-radius2) (par-value line)))
                 ((starts-with line "cols ")
                  (setf (slot-value *state* 'cols) (par-value line)))
                 ((starts-with line "loadtime ")
                  (setf (slot-value *state* 'load-time)
                        (/ (par-value line) 1000.0)))
                 ((starts-with line "rows ")
                  (setf (slot-value *state* 'rows) (par-value line)))
                 ((starts-with line "spawnradius2 ")
                  (setf (slot-value *state* 'spawn-radius2) (par-value line)))
                 ((starts-with line "turns ")
                  (setf (slot-value *state* 'turns) (par-value line)))
                 ((starts-with line "turntime ")
                  (setf (slot-value *state* 'turn-time)
                        (/ (par-value line) 1000.0)))
                 ((starts-with line "viewradius2 ")
                  (setf (slot-value *state* 'view-radius2) (par-value line)))))
  (setf (slot-value *state* 'game-map)
        (make-array (list (rows *state*) (cols *state*)) :element-type 'fixnum
                    :initial-element 0))
  (init-internal-data (rows *state*) (cols *state*)))


;; TODO is this the right thing to do?
(defun reset-game-map ()
  "Sets all tiles on the map to land (0) if they're not already land or
  water (1).  Modifies (GAME-MAP *STATE*)."
  (loop with game-map = (game-map *state*)
        with dim = (array-dimensions game-map)
        for row from 0 below (first dim)
        do (loop for col from 0 below (second dim)
                 when (> (aref game-map row col) 1)
                   do (setf (aref game-map row col) 0))))

;; TODO needs a docstring
(defun split-state-string (string)
  (loop with result = nil
        with value = nil
        for c across string
        when (and (char= c #\space) value)
          do (push (coerce (nreverse value) 'string) result)
             (setf value nil)
        when (char/= c #\space)
          do (push c value)
        finally (when value
                  (push (coerce (nreverse value) 'string) result))
                (return (nreverse result))))

(defun set-ant (string)
  "Parses the \"a row col owner\" STRING and sets the specific map tile to
  an ant of owner.  Modifies (ENEMY-ANTS *STATE*), (MY-ANTS *STATE*) and
  (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3))))
    (if (= owner 0)
        (push (list row col) (slot-value *state* 'my-ants))
        (push (list row col owner) (slot-value *state* 'enemy-ants)))
    (setf (aref (game-map *state*) row col) (+ owner 100))))

(defun set-dead (string)
  "Parses the \"d row col owner\" STRING and sets the specific map tile to
  a dead ant of owner.  Modifies (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3))))
    (unless (= 2 (aref (game-map *state*) row col))
      (setf (aref (game-map *state*) row col) (+ owner 200)))))

(defun set-food (string)
  "Parses the \"f row col\" STRING and sets the specific map tile to food.
  Modifies (FOOD *STATE*) and (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (push (list row col) (slot-value *state* 'food))
    (setf (aref (game-map *state*) row col) 2)))

(defun set-water (string)
  "Parses the \"w row col\" STRING and sets the specific map tile to water.
  Modifies (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (setf (aref (game-map *state*) row col) 1)))

;; TODO detect the razing of hills
(defun set-hill (string)
  "Parses the \"a row col owner\" STRING and sets the specific map tile to
  a hill of owner.  Modifies (HILLS *STATE*) and (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3))))

    (let ((hill-record (list row col owner)))
      (unless (member hill-record (hills *state*) :test 'equal)
        (push hill-record (slot-value *state* 'hills))))

    (setf (aref (game-map *state*) row col) (+ owner 300))))

(defun parse-turn ()
  "Parses a typical turn.  Modifies *STATE* indirectly through RESET-GAME-MAP
  and the SET-* functions."
  (reset-game-map)
  (loop for line = (read-line *standard-input* nil)
        until (starts-with line "go")
        do (cond ((starts-with line "f ") (set-food line))
                 ((starts-with line "w ") (set-water line))
                 ((starts-with line "a ") (set-ant line))
                 ((starts-with line "d ") (set-dead line))
                 ((starts-with line "h ") (set-hill line)))))

(defun reset-some-state ()
  "Sets (ENEMY-ANTS *STATE*), (MY-ANTS *STATE*) and (FOOD *STATE*) to NIL."
  (setf (slot-value *state* 'enemy-ants) nil
        (slot-value *state* 'my-ants)    nil
        (slot-value *state* 'food)       nil))

(let ((time-units (/ 1.0 internal-time-units-per-second)))
  ;; TODO correctly name function: doesn't return wall time
  ;; TODO use DOUBLE-FLOATs?
  (defun wall-time (&key (offset 0))
    "Returns the time in seconds (as a FLOAT) since SBCL was started."
    (+ (* (get-internal-real-time) time-units)
       offset)))

(defun parse-game-state ()
  "Calls either PARSE-TURN or PARSE-GAME-PARAMETERS depending on the line
  on standard input.  Modifies *STATE* and returns T if the game has ended,
  otherwise NIL."
  (setf (slot-value *state* 'turn-start-time) (wall-time))
  (reset-some-state)
  (loop for line = (read-line *standard-input* nil)
        until (> (length line) 0)
        finally (return (cond ((starts-with line "end")
                               (parse-turn)
                               t)
                              ((starts-with line "turn 0")
                               (setf (slot-value *state* 'turn) 0)
                               (parse-game-parameters)
                               nil)
                              ((starts-with line "turn ")
                               (setf (slot-value *state* 'turn)
                                     (par-value line))
                               (parse-turn)
                               nil)))))

(defun turn-time-remaining ()
  "Returns the turn time remaining in seconds (as a FLOAT)."
  (- (+ (turn-start-time *state*) (turn-time *state*))
     (wall-time)))

(defun user-interrupt (arg)
  (declare (ignore arg))
  (format *debug-io* "~&User interrupt. Aborting...~%")
  (quit))



;;; intelligent code

(defclass ant ()
  ((x :accessor x :initarg x :initform (error "Must specify x"))
   (y :accessor y :initarg y :initform (error "Must specify y"))))


(defvar *internal-state* nil)
(defvar *cur-turn* 1)
(defvar *log-output* nil)


(defun init-internal-data (rows cols)
  "Init internal data."
  (setf *internal-state*
        (make-array (list rows cols) :element-type 'fixnum :initial-element 0)))


(defun explore-potential (r c)
  "Return a value representing the explore potential of the cell (small is
better)."
  (let* ((e (get-entity-at r c))
         (p (cond
              ((= e 1) (1- *cur-turn*)) ; water
              ((>= e 300) (1+ *cur-turn*)) ; hill :fixme: - check if correct
              ((= e 200) (1+ *cur-turn*)) ; enemy ant - run, forrest, run
              ;((= e 100) 0) ; own ant
              ((= e 2) (- *cur-turn*)) ; food
              (t (aref *internal-state* r c)))))
    (when *log-output*
      (format *log-output* "potential of (~a, ~a)=~a~%"
              r c p))
    p))


(defun for-each-direction-sum (row col direction func
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


(defun do-ant (r c)
  "Do something with ant at coord. (r, c)."
  (let ((dir (move-explore r c)))
    (when dir
      (when *log-output*
        (format *log-output* "cmd ~a from (~a,~a), internal-state at dest: ~a~%"
                dir r c *cur-turn*))
      (issue-order r c dir)
      (set-value-at *internal-state* r c dir *cur-turn*))))


(defun value-at (array r c dir)
  "..."
  (let ((nl (new-location r c dir)))
    (aref array (first nl) (second nl))))


(defun set-value-at (array r c dir value)
  "..."
  (let ((nl (new-location r c dir)))
    (setf (aref array (first nl) (second nl)) value)))


(defun move-acceptable-p (r c dir)
  "Return true if the move is acceptable"
  (and (not (waterp r c dir))
       (/= (value-at *internal-state* r c dir) *cur-turn*)))
  

(defun move-explore (r c)
  "Select a cell that was not visited before or was visited a long time ago"
  (when *log-output*
    (format *log-output* "explore from (~a, ~a), *cur-turn*=~a~%"
            r c *cur-turn*)
    ;; :tmp:
    ;;(format *log-output* "map:~%~a~%internal-state:~%~a~%"
    ;;        (game-map *state*)
    ;;        *internal-state*)
    )

  (let ((min (* 5 *cur-turn*))
        (dir nil))
    (when (move-acceptable-p r c :north)
      (let ((val (for-each-direction-sum r c :north 'explore-potential t)))
        (when *log-output*
          (format *log-output* "potential at north: ~a~%" val))
        (when (< val min)
          (setf dir :north)
          (setf min val))))
    (when (move-acceptable-p r c :east)
      (let ((val (for-each-direction-sum r c :east 'explore-potential t)))
        (when *log-output*
          (format *log-output* "potential at east: ~a~%" val))
        (when (< val min)
          (setf dir :east)
          (setf min val))))
    (when (move-acceptable-p r c :south)
      (let ((val (for-each-direction-sum r c :south 'explore-potential t)))
        (when *log-output*
          (format *log-output* "potential at south: ~a~%" val))
        (when (< val min)
          (setf dir :south)
          (setf min val))))
    (when (move-acceptable-p r c :west)
      (let ((val (for-each-direction-sum r c :west 'explore-potential t)))
        (when *log-output*
          (format *log-output* "potential at west: ~a~%" val))
        (when (< val min)
          (setf dir :west)
          (setf min val))))
    (when *log-output*
      (format *log-output* "best direction is ~a~%" dir))
    dir))
