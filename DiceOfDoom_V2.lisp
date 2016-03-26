(load "~/Development/CLISP/LOL/DiceOfDoom.lisp")
(load "~/Development/CLISP/LOL/lazy.lisp")

(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun limit-tree-depth (tree depth)
  (list (first tree)
        (first (rest tree))
        (if (zerop depth)
            (lazy-nil)
            (lazy-mapcar (lambda (move)
                           (cons (first move)
                                 (mapcar (lambda (x)
                                           (limit-tree-depth x (1- depth)))
                                         (rest move))))
                         (caddr tree)))))

(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
             (unless (lazy-null moves)
               (let ((x (ab-rate-position (second (lazy-first moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 (if (>= x upper-limit)
                     (list x)
                     (cons x (f (lazy-rest moves) (max x lower-limit))))))))
    (f (third tree) lower-limit)))

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
             (unless (lazy-null moves)
               (let ((x (ab-rate-position (second (lazy-first moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 (if (<= x lower-limit)
                     (list x)
                     (cons x (f (lazy-rest moves) (min x upper-limit))))))))
    (f (third tree) upper-limit)))

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (third tree)))
    (if (not (lazy-null moves))
        (if (eq (first tree) player)
            (apply #'max (ab-get-ratings-max tree
                                             player
                                             upper-limit
                                             lower-limit))
            (apply #'min (ab-get-ratings-min tree
                                             player
                                             upper-limit
                                             lower-limit)))
        (score-board (second tree) player))))
