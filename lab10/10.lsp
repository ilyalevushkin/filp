;7------------------------------------------

(defun add-list-deep (lst depth)
    (cond ((null lst) depth)
            (t (add-list-deep (cdr lst) (+ 1 depth)))
    )
)

(defun summ-length-help (list-of-list summ)
    (cond ((null list-of-list) summ)
        (t (summ-length-help (cdr list-of-list) (+ (add-list-deep (car list-of-list) 0) summ)))
    )
)

(defun summ-length (list-of-list)
    (summ-length-help list-of-list 0)
)

;8------------------------------------------

(defun reg-add-help (lst summ)
    (cond ((null lst) summ)
            (t  (reg-add-help (cdr lst) (if (listp (car lst)) 
                        (+ (reg-add (car lst)) summ) (+ summ (car lst)))
                )
            )
    )
)

(defun reg-add (lst)
    (reg-add-help lst 0)
)

;9------------------------------------------

(defun recnth-help (lst n count)
    (cond ((null lst) count)
            ((equal n count) (car lst))
            (t (recnth-help (cdr lst) n (+ count 1)))
    )
)

(defun recnth (n lst)
    (cond ((null lst) nil)
            ((>= n (length lst)) nil)
            (t (recnth-help lst n 0))
    )
)

;10------------------------------------------



(defun alloddr-help-low (obj)
    (cond ((null obj) nil)
            ((listp obj) (alloddr-help obj))
            (t (if (oddp obj) t nil))
    )
)

(defun alloddr-help (lst)
    (cond ((null lst) t)
            (t  (if (alloddr-help-low (car lst)) (alloddr-help (cdr lst)) nil
                )
            )
    )
)

(defun alloddr (lst)
    (cond ((null lst) nil)
        (t (alloddr-help lst))
    )
)

;11------------------------------------------

(defun last-elem (lst)
    (cond ((null (cdr lst)) (car lst))
        (t (last-elem (cdr lst)))
    )
)

;12------------------------------------------

;0######

;n - не включительно
(defun sum-elem-from-help (lst n pos summ)
    (cond ((null lst) summ)
            ((equal n pos) summ)
            (t (sum-elem-from-help (cdr lst) n (+ pos 1) (+ summ (car lst))))
    )
)

(defun sum-elem-from (lst n)
    (cond ((null lst) nil)
        (t (sum-elem-from-help lst n 0 0))
    )
)

;1######

;используется sum-elem-from-help из предыдущего
(defun sum-elem-to (lst from)
    (cond ((null lst) nil)
            ((>= from (length lst)) 0)
        (t (sum-elem-from-help (reverse lst) (- (length lst) from) 0 0))
    )
)

;2######


;to - не включительно
(defun sum-elem-from-to-h-help (lst to h pos summ)
    (cond ((>= pos to) summ)
            (t (sum-elem-from-to-h-help lst to h (+ pos h) (+ summ (
                let ((elem (nth pos lst)));ищем элемент списка на позиции pos
                    (if elem elem 0)
            ))))
    )
)

(defun sum-elem-from-to-h (lst from to h)
    (cond ((null lst) nil)
            ((> from to) 0)
        (t (sum-elem-from-to-h-help lst to h from 0))
    )
)

;13------------------------------------------

(defun last-oddp-number-help (lst last-number)
    (cond ((null lst) last-number)
            (t (last-oddp-number-help (cdr lst) (if (oddp (car lst)) (car lst) last-number)))
    )
)

(defun last-oddp-number (lst)
    (last-oddp-number-help lst nil)
)

;14------------------------------------------

(defun square-list-help (lst result)
    (cond ((null lst) result)
        (t (square-list-help (cdr lst) (cons (* (car lst) (car lst)) result)))
    )
)

(defun square-list (lst)
    (reverse (square-list-help lst nil))
)

;cons-дополняемая
(defun cons-square-list (lst)
    (cond ((null lst) nil)
        (t (cons (* (car lst) (car lst)) (cons-square-list (cdr lst))))
    )
)

;15------------------------------------------

;для списка чисел
(defun select-odd-help (lst answer)
    (cond ((null lst) answer)
            (t (select-odd-help (cdr lst) (if (oddp (car lst)) 
                        (cons (car lst) answer) answer)))
    )
)

(defun select-odd (lst)
    (reverse (select-odd-help lst nil))
)



(defun sum-all-odd-help (lst answer)
    (cond ((null lst) answer)
            (t (sum-all-odd-help (cdr lst) (if (oddp (car lst)) 
                        (+ (car lst) answer) answer)))
    )
)

(defun sum-all-odd (lst)
    (cond ((null lst) nil)
    (t (sum-all-odd-help lst 0))
    )
)


;для объекта


(defun select-odd-help (lst answer)
    (cond ((null lst) answer)
            ((listp (car lst)) (select-odd-help (cdr lst) (select-odd-help (car lst) answer)))
            (t (select-odd-help (cdr lst) (if (oddp (car lst)) (cons (car lst) answer) answer)))
    )
)

(defun select-odd (lst)
    (select-odd-help lst nil)
)


(defun sum-all-odd-help (lst answer)
    (cond ((null lst) answer)
            ((listp (car lst)) (sum-all-odd-help (cdr lst) (sum-all-odd-help (car lst) answer)))
            (t (sum-all-odd-help (cdr lst) (if (oddp (car lst)) (+ (car lst) answer) answer)))
    )
)

(defun sum-all-odd (lst)
    (cond ((null lst) nil)
    (t (sum-all-odd-help lst 0))
    )
)