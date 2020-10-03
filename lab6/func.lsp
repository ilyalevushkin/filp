(defun how_alike (x y)
( cond ((or (= x y) (equal x y)) 'the_same)
(( and (oddp x) (oddp y)) 'both_odd)
((and (evenp x) (evenp y)) 'both_even)
(t 'difference)))

(defun how_alike_if (x y) (
    if (or (= x y) (equal x y)) 'the_same (if 
        (and (oddp x) (oddp y)) 'both_odd (if 
            (and (evenp x) (evenp y)) 'both_even 'difference
            )
        )
))


;создание списка из двух элементного списка
(defun two_elem_list (capital country) (
    mapcar #'list capital country
))

;нахождение страны по столице в списке из двухэлементных списков (4 страны)
(defun capital_for_two_elem_list (lst capital) (
    if (equal (caar lst) capital) (cadar lst) (
        if (equal (caadr lst) capital) (cadadr lst) (
            if (equal (caaddr lst) capital) (cadadr (cdr lst)) (
                if (equal (caaddr (cdr lst)) capital) (cadadr (cddr lst)) Nil
            )
        )
    )
))

;нахождение столицы по стране в списке из двухэлементных списков (4 страны)
(defun country_for_two_elem_list (lst country) (
    if (equal (cadar lst) country) (caar lst) (
        if (equal (cadadr lst) country) (caadr lst) (
            if (equal (cadadr (cdr lst)) country) (caaddr lst) (
                if (equal (cadadr (cddr lst)) country) (caaddr (cdr lst)) Nil
            )
        )
    )
))

;нахождение страны по столице в списке из двухэлементных списков
(defun capital_for_two_elem_list (lst capital) (
    reduce (lambda (answer lst) (
        if (equal answer Nil) (
            if (equal (car lst) capital) (cadr lst) Nil
            ) answer
    )) lst :initial-value Nil
))


;нахождение столицы по стране в списке из двухэлементных списков 
(defun country_for_two_elem_list (lst country) (
    reduce (lambda (answer lst) (
        if (equal answer Nil) (
            if (equal (cadr lst) country) (car lst) Nil
            ) answer
    )) lst :initial-value Nil
))

;-----------------------------------------------------------

;создание списка из точечных пар
(defun two_dot_pair_list (capital country) (
    mapcar #'cons capital country
))

;нахождение страны по столице в списке точечных пар (4 страны)
(defun capital_for_two_dot_pair_list (lst capital) (
    if (equal (caar lst) capital) (cdar lst) (
        if (equal (caadr lst) capital) (cdadr lst) (
            if (equal (caaddr lst) capital) (cdadr (cdr lst)) (
                if (equal (caaddr (cdr lst)) capital) (cdadr (cddr lst)) Nil
            )
        )
    )
))

;нахождение столицы по стране в списке точечных пар (4 страны)
(defun country_for_two_dot_pair_list (lst country) (
    if (equal (cdar lst) country) (caar lst) (
        if (equal (cdadr lst) country) (caadr lst) (
            if (equal (cdadr (cdr lst)) country) (caaddr lst) (
                if (equal (cdadr (cddr lst)) country) (caaddr (cdr lst)) Nil
            )
        )
    )
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


(defun find_first_pair (x name)
   (reduce (lambda (prev x) (or prev x))
       (mapcar (lambda (l)
                   (and (equal (car l) name) (cdr l))
               ) x
       )
   )
)


;-----------------------------------------------------------

;проверка нахождения страны по столице в списке двухэлементных списков
(defun exec_capital_for_two_elem_list (capitals countrys capital) (
    capital_for_two_elem_list (two_elem_list capitals countrys) capital
))

;проверока нахождения столицы по стране в списке двухэлементных списков
(defun exec_country_for_two_elem_list (capitals countrys country) (
    country_for_two_elem_list (two_elem_list capitals countrys) country
))

;проверка нахождения страны по столице в списке точечных пар
(defun exec_capital_for_two_dot_pair_list (capitals countrys capital) (
    capital_for_two_dot_pair_list (two_dot_pair_list capitals countrys) capital
))

;проверока нахождения столицы по стране в списке точечных пар
(defun exec_country_for_two_dot_pair_list (capitals countrys country) (
    country_for_two_dot_pair_list (two_dot_pair_list capitals countrys) country
))

;функционалы
(defun find_in_list (capitals countries sth finder creator) 
    (FUNCALL finder (FUNCALL creator capitals countries) sth
    )
)


(defun sq_test()
    (list
        (exec_capital_for_two_elem_list '(s b d f) '(cs bs ds fs) 't)
        (exec_country_for_two_elem_list '(s b d f) '(cs bs ds fs) 'cs)
    )
)