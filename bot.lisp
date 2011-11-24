;;; functions related to bot protocol, logging or OS

;;; State Class

;;; On map:
;;;   1 - water
;;; 100 - own ant (free)
;;; 101 - own ant (busy)


(defclass state ()
  ((rows :accessor rows :initform nil)
   (cols :accessor cols :initform nil)
   (game-map :accessor game-map :initform nil)

   (enemy-ants :reader enemy-ants :initform nil)
   (my-ants :reader my-ants :initform nil)

   ;; (list row col)
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

(defvar *internal-state* nil)
(defvar *cur-turn* 1)
(defvar *log-output* nil)

;;; protocol

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
  "Parses the \"h row col owner\" STRING and sets the specific map tile to
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
                 ((starts-with line "h ") (set-hill line))))
  ;; print turn data
  (log-output "hills for current turn: ~a~%" (hills *state*)))


(defun reset-some-state ()
  "Sets (ENEMY-ANTS *STATE*), (MY-ANTS *STATE*) and (FOOD *STATE*) to NIL."
  (setf (slot-value *state* 'enemy-ants) nil
        (slot-value *state* 'my-ants)    nil
        (slot-value *state* 'food)       nil))


(defun init-internal-data (rows cols)
  "Init internal data."
  (setf *internal-state*
        (make-array (list rows cols) :element-type 'fixnum :initial-element 0)))


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


(defun log-output-finish ()
  "Flush *log-output*"
  (when *log-output*
    (finish-output *log-output*)))


(defun log-output (&rest params)
  "Print the output to *log-output* stream, if not nil."
  (when *log-output*
    (apply 'format *log-output* params)
    (log-output-finish) ;; :tmp:
    ))


;; This is the actual 'AI' function.
(defun do-turn ()
  (let ((steps 8))
    (target-food steps)
    (target-enemy-hills (* 2 steps) 2)

    (loop
       for ant in (my-ants *state*)
       for row = (elt ant 0)
       for col = (elt ant 1)
       do (do-ant row col))
    (incf *cur-turn*)
    (log-output-finish)))


