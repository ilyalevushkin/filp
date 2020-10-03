;многоуровневый список в одноуровневый

(defun into_one (lst rst)
    (cond ((null lst) rst)
          ((atom lst) (cons lst rst))
          (t (into_one (car lst) (into_one (cdr lst) rst) ))
    )
)

(defun to-odn(lst)
    (mapcan #'(lambda(x) (cond
        ((listp x) (to-odn x))
        (T (list x))
    )) lst)
)

(write (into_one '(a b c (1 ((v)) sd (1 2 3)) 2) nil))


;удалить i-тый элемент

(defun remove-i (i lst)
(setq i (1+ i)) ;setq НЕ убирать и не менять на set
(remove-if #'(lambda (x) (zerop (setq i (1- i)))) lst)
)


;-----------------------
(defun return_head (head changed_value)
    head
)

(defun remove-i-help (i lst pos head)
    (cond ((null lst) head)
            ((equal i pos) (return_head head (rplacd lst (cddr lst))))
            (t (remove-i-help i (cdr lst) (1+ pos) head))
    )
)

(defun remove-i (i lst)
    (cond ((null lst) lst)
            ((equal 0 i) (cdr lst))
            (t (remove-i-help i lst 1 lst))
    )
)
;------------------------


(defun my-reverse-help (lst res)
    (cond ((null lst) res)
        (t (my-reverse-help (cdr lst) (cons (car lst) res)))
    )
)

(defun my-reverse (lst)
    (my-reverse-help lst nil)
)

(defun remove-i-help (lst pos cur_pos res)
    (cond ((null lst) (reverse res))
        ((equal pos cur_pos) (nconc (reverse res) (cdr lst)))
        (t (remove-i-help (cdr lst) pos (1+ cur_pos) (cons (car lst) res)))
    )
)

(defun remove-i (i lst)
    (cond ((null lst) lst)
        (t (remove-i-help lst i 0 nil))
    )
)

(write (remove-i 1 '(A B C) ))

;вставить в список на i-ое место (другой список)

;функционалы



;рекурсия
(defun insert-help (lst value pos cur_pos res)
    (cond ((null lst) (reverse res))
        ((equal pos cur_pos) (nconc (reverse res) (cons value nil) lst))
        (t (insert-help (cdr lst) value pos (1+ cur_pos) (cons (car lst) res)))
    )
)

(defun insert (lst value i)
    (cond ((null lst) lst)
        (t (insert-help lst value i 0 nil))
    )
)

;функционалы

(defun insert (lst value i)
    (mapcar #'(lambda (x) (if (equal i ) (list value x) x)) lst)
)


;(атом)
  (defun insert_atom(ARRAY VALUE PLACE)
    ( cond ( (null ARRAY) (cons VALUE NIL) )
    ( (> n 0) (cons (car ARRAY) (insert (cdr ARRAY) VALUE (- n 1))) )
    ( T (cons VALUE ARRAY))
    ))

;определить длину
(defun len(lst)
  (if(null lst) 0
    (+ (len (cdr lst)) 1))
)

;вернуть n-ый элемент
(defun getn(ARRAY PLACE)
  ( cond ( (null ARRAY) Nil )
  ( (> PLACE 0) (getn (cdr ARRAY) (- PLACE 1)))
  (  T (car ARRAY))
      )
  )


(write (getn  '(A B C) 1  ))