(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))
(defparameter *ai-level* 4)

; DIRTY IMPERATIVE

(defun gen-board()
                                        ; Randomize board when game begins
  (board-array (loop for n below *board-hexnum*
                  collect (list (random *num-players*)
                                (1+ (random *max-dice*))))))

(defun draw-board (board)
  (loop for y below *board-size*
     do (progn (fresh-line)
               (loop repeat (- *board-size* y)
                  do (princ "  "))
               (loop for x below *board-size*
                  for hex = (aref board (+ x (* *board-size* y)))
                  do (format t "~a-~a " (player-letter (first hex))
                             (second hex))))))

; CLEAN FUNCTIONAL

(defun board-array (lst)
                                        ; Convert board list to array
  (make-array *board-hexnum* :initial-contents lst))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun game-tree-v1 (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(let ((previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply #'game-tree-v1 rest)))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (first (aref board pos)))
           (dice (pos)
             (second (aref board pos))))
    (lazy-mapcan (lambda (src)
              (if (eq (player src) cur-player)
                (lazy-mapcan (lambda (dst)
                          (if (and (not (eq (player dst) cur-player))
                                   (> (dice src) (dice dst)))
                              (make-lazy (list (list (list src dst)
                                                     (game-tree (board-attack board
                                                                              cur-player
                                                                              src
                                                                              dst
                                                                              (dice src))
                                                                cur-player
                                                                (+ spare-dice (dice dst))
                                                                nil))))
                              (lazy-nil)))
                             (make-lazy (neighbors src)))
                (lazy-nil)))
                 (make-lazy (loop for n below *board-hexnum* collect n)))))

(defun neighbors-v1 (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
       when (and (>= p 0) (< p *board-hexnum*))
       collect p)))

(let ((previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (neighbors-v1 pos)))))

(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                  for hex across board
                  collect (cond ((eq pos src) (list player 1))
                                ((eq pos dst) (list player (1- dice)))
                                (t hex)))))

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
             (cond ((zerop n) lst)
                   ((null lst) nil)
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            (cons (car lst) (f (rest lst) n))))))))
    (board-array (f (coerce board 'list)
                    (largest-cluster-size board player)))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (first tree)))
  (draw-board (first (rest tree))))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-first moves))
                        (action (first move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                       (format t "~a -> ~a" (first action) (first (last action)))
                       (princ "end turn.")))
                 (print-moves (lazy-rest moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (first (last (lazy-nth (1- (read)) moves)))))

(defun winners (board)
  (let* ((tally (loop for hex across board
                   collect (first hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'first
            (remove-if (lambda (x)
                         (not (eq (rest x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (first w))))))

                                        ; The MINIMAX Algorithm
(defun rate-position (tree player)
  (let ((moves (third tree)))
    (if (not (lazy-null moves))
        (apply (if (eq (first tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (score-board (second tree) player))))

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (second move) player))
                         (third tree))))

;(defun handle-computer (tree)
;  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
;                                     (first tree)
;                                     most-positive-fixnum
;                                     most-negative-fixnum)))
;    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                                        ;                    (caddr tree)))))

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (first tree))))
    (pick-chance-branch
     (second tree)
     (lazy-nth (position (apply #'max ratings) ratings) (third tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (third tree)) (announce-winner (second tree)))
        ((zerop (first tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

(defun score-board (board player)
  (loop for hex across board
     for pos from 0
     sum (if (eq (first hex) player)
             (if (threatened pos board)
                 1
                 2)
             -1)))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (first hex))
         (dice (second hex)))
    (loop for n in (neighbors pos)
       do (let* ((nhex (aref board n))
                 (nplayer (first nhex))
                 (ndice (second nhex)))
            (when (and (not (eq player nplayer)) (> ndice dice))
              (return t))))))
