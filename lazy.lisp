(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-first (x)
  (first (force x)))

(defun lazy-rest (x)
  (rest (force x)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (lst)
  (lazy (when lst
          (cons (first lst) (make-lazy (rest lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-first lst) (take (1- n) (lazy-rest lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-first lst) (take-all (lazy-rest lst)))))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-first lst))
                (lazy-mapcar fun (lazy-rest lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
                 (force (lazy-mapcan fun (lazy-rest lst)))
                 (cons (lazy-first lst-cur) (lazy (f (lazy-rest lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-first lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-first lst)))
      (if (funcall fun x)
          x
          (lazy-find-if fun (lazy-rest lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-first lst)
      (lazy-nth (1- n) (lazy-rest lst))))
