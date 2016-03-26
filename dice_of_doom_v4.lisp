(load "~/Development/CLISP/LOL/dice_of_doom_v3.lisp")

(defparameter *num-players* 4)
(defparameter *die-colors* '((255 63 63) (63 63 255) (63 255 63) (255 63 255)))
(defparameter *dice-probability* #(#(0.84 0.97 1.0 1.0)
                                   #(0.44 0.78 0.94 0.99)
                                   #(0.15 0.45 0.74 0.91)
                                   #(0.04 0.19 0.46 0.72)
                                   #(0.01 0.06 0.22 0.46)))

(defparameter *max-dice* 5)
(defparameter *ai-level* 2)

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (first (aref board pos)))
           (dice (pos)
             (second (aref board pos))))
    (lazy-mapcan (lambda (src)
                   (if (eq (player src) cur-player)
                       (lazy-mapcan
                        (lambda (dst)
                          (if (and (not (eq (player dst) cur-player))
                                   (> (dice src) 1))
                              (make-lazy (list (list (list src dst)
                                                     (game-tree (board-attack board cur-player src dst (dice src))
                                                                cur-player
                                                                (+ spare-dice (dice dst))
                                                                nil)
                                                     (game-tree (board-attack-fail board cur-player src dst (dice src))
                                                                cur-player
                                                                (+ spare-dice (dice dst))
                                                                nil))))
                              (lazy-nil)))
                        (make-lazy (neighbors src)))
                       (lazy-nil)))
                 (make-lazy (loop for n below *board-hexnum*
                               collect n)))))

(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0
                  for hex across board
                  collect (if (eq pos src)
                              (list player 1)
                              hex))))

(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
                  sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a." dice-num total)
    total))

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice)))

(defun pick-chance-branch (board move)
  (labels ((dice (pos)
             (second (aref board pos))))
    (let ((path (first move)))
      (if (or (null path) (roll-against (dice (first path))
                                        (dice (second path))))
          (second move)
          (third move)))))

(defun get-ratings (tree player)
  (let ((board (second tree)))
    (labels ((dice (pos)
               (second (aref board pos))))
      (take-all (lazy-mapcar
                 (lambda (move)
                   (let ((path (first move)))
                     (if path
                         (let* ((src (first path))
                                (dst (second path))
                                (probability (aref (aref *dice-probability*
                                                         (1- (dice dst)))
                                                   (- (dice src) 2))))
                           (+ (* probability (rate-position (second move) player))
                              (* (- 1 probability) (rate-position (third move) player))))
                         (rate-position (second move) player))))
                 (third tree))))))

(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
             (if (and (eq (first (aref board pos)) player)
                      (not (member pos visited)))
                 (check-neighbors (neighbors pos) (cons pos visited))
                 visited))
           (check-neighbors (lst visited)
             (if lst
                 (check-neighbors (rest lst) (check-pos (first lst) visited))
                 visited)))
    (check-pos pos '())))

(defun largest-cluster-size (board player)
  (labels ((f (pos visited best)
             (if (< pos *board-hexnum*)
                 (if (and (eq (first (aref board pos)) player)
                          (not (member pos visited)))
                     (let* ((cluster (get-connected board player pos))
                            (size (length cluster)))
                       (if (> size best)
                           (f (1+ pos) (append cluster visited) size)
                           (f (1+ pos) (append cluster visited) best)))
                     (f (1+ pos) visited best))
                 best)))
    (f 0 '() 0)))
