
;5.2-------------------------------------------------

;##################
;функционалы
;##################

(defun reduce_and (lst); применяем and к списку
    (reduce #'(lambda (prev lst) (and prev lst)) lst)
)

(defun set-equal (col1 col2)
    (cond ((and (null col1) (null col2)) t)
            ((or (null col1) (null col2)) nil)
            ((equal (length col1) (length col2)) (
                reduce_and (mapcar (lambda (el) (if (member el col2) t nil)) col1))
                )
            (t nil)
    )
)

;##################
;хвостовая рекурсия
;##################

(defun set-equal (col1 col2)
    (cond ((and (null col1) (null col2)) t)
            ((or (null col1) (null col2)) nil)
            (t  (if (member (car col1) col2) 
            (set-equal (cdr col1) (set-difference col2 (cons (car col1) nil))) nil
                )
            )
    )
)

;5.3-------------------------------------------------

;##################
;функционалы
;##################

;создание списка из точечных пар
(defun two_dot_pair_list (capital country) (
    mapcar #'cons capital country
))

;нахождение страны по столице в списке точечных пар
(defun capital_for_two_dot_pair_list (lst capital) (
    reduce (lambda (answer lst) (
        if (equal answer Nil) (
            if (equal (car lst) capital) (cdr lst) Nil
            ) answer
    )) lst :initial-value Nil
))

;нахождение столицы по стране в списке точечных пар
(defun country_for_two_dot_pair_list (lst country) (
    reduce (lambda (answer lst) (
        if (equal answer Nil) (
            if (equal (cdr lst) country) (car lst) Nil
            ) answer
    )) lst :initial-value Nil
))

;##################
;хвостовая рекурсия
;##################

;создание списка из точечных пар
(defun create_list_recurs (capital country result)
    (cond ((or (null country) (null capital)) result)
            (t (create_list_recurs (cdr capital) (cdr country) 
            (nconc result (list (cons (car country) (car capital))))))
    )
)

(defun two-dot-pair-list (capital country)
    (create_list_recurs capital country nil)
)

;нахождение страны по столице в списке точечных пар

(defun check_capital (lst capital)
    (if (equal (cdar lst) capital) t nil)
)

(defun capital_for_two_dot_pair_list (lst capital)
    (cond ((null lst) nil)
        ((check_capital lst capital) (caar lst))
        (t (capital_for_two_dot_pair_list (cdr lst) capital))
    )
)

;нахождение столицы по стране в списке точечных пар

(defun check_country (lst country)
    (if (equal (caar lst) country) t nil)
)

(defun country_for_two_dot_pair_list (lst country)
    (cond ((null lst) nil)
        ((check_country lst country) (cdar lst))
        (t (country_for_two_dot_pair_list (cdr lst) country))
    )
)


;5.7-------------------------------------------------

;##################
;функционалы
;##################

(defun multiply_numbers (lst number)
    (mapcar (lambda (x) (* x number)) lst)
)

;##################
;хвостовая рекурсия
;##################


(defun recurs_multiply (lst number result)
    (cond ((null lst) result)
            (t (recurs_multiply (cdr lst) number (cons (* (car lst) number) result)))
    )
)

(defun multiply_numbers (lst number)
    (reverse (recurs_multiply lst number nil))
)


;##################
;функционалы+рекурсия
;##################
(defun multiply (lst number)
    (mapcar (lambda (obj) 
                (cond ((numberp obj) (* obj number))
                         ((listp obj) (multiply obj number))
                        (t obj)
                )
            )  
    lst)
)


;6.2-------------------------------------------------

;##################
;функционалы+рекурсия
;##################

(defun decrease_ten (lst)
    (mapcar (lambda (obj) 
                (cond ((numberp obj) (- obj 10))
                         ((listp obj) (decrease_ten obj 10))
                        (t obj)
                )
            )  
    lst)
)

;6.3-------------------------------------------------

;##################
;функционалы
;##################

(defun reduce_or (lst); применяем or к списку
    (reduce #'(lambda (prev lst) (or prev lst)) lst)
)

(defun get_first_list (lst)
    (reduce_or (mapcar (lambda (x) (if (listp x) x nil)) lst))
)

;##################
;хвостовая рекурсия
;##################

(defun get_first_list (lst)
    (cond ((null lst) nil)
            ((null (car lst)) (get_first_list (cdr lst)))
            ((listp (car lst)) (car lst))
            (t (get_first_list (cdr lst)))
    )
)

;6.4-------------------------------------------------

;##################
;функционалы+рекурсия
;##################

(defun get_all_numbers_between (lst less more)
    (mapcan (lambda (x) 
                (cond ((null x) nil)
                        ((listp x) (get_all_numbers_between x less more))
                        ((numberp x) (if (and (> x less) (< x more)) (cons x nil) nil))
                        (t nil)
                )
            ) lst
    )
)

(defun less_and_more_numbers (lst a b)
    (let ((less (min a b))
            (more (max a b))
    )
        (get_all_numbers_between lst less more)
    )
)


;6.5-------------------------------------------------

;##################
;функционалы
;##################

(defun decart (X Y)
    (mapcan #'
        (lambda (x)
            (mapcar #'
                (lambda (y) (list x y))
                Y
            )
        )
        X
    )
)


;##################
;хвостовая рекурсия
;##################


(defun decart-y (x Y result)
    (cond ((null Y) result)
            (t (decart-y x (cdr Y) (cons (list x (car Y)) result) ))
    )
)

(defun decart-x (X Y result)
    (cond ((null X) result)
            (t (decart-x (cdr X) Y (decart-y (car X) Y result)))
    )
)

(defun decart (X Y)
    (decart-x X Y nil)
)

;6.6-------------------------------------------------

(reduce #'+ '()) -> 0 

(reduce #'* '()) -> 1

Связано это с рекурсивной реализацией функции reduce
