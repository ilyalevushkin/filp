
(defun polindrom (lst)
    (and lst
        (reduce (lambda (prev x) (and prev x))
            (mapcar #'equal lst (reverse lst)))
    )
)

(defun get_pre_last_elem (lst)
    (if (null (cddr lst)) lst (get_pre_last_elem (cdr lst)))
)


(defun change_first_second_elem (lst)
    (rplacd (cdr lst) (rplacd lst Nil))
)

(defun swap-first-last (lst)
    (cond ((not lst) lst)
          ((and (cdr lst) (not (cddr lst)) ) (change_first_second_elem lst))
          (t
                (let* (
                            (first_elem lst)
                            (second_elem (cdr lst))
                            (pre_last_elem (get_pre_last_elem lst))
                            (last_elem (last lst))
                            (last_elem (rplacd last_elem second_elem))
                            (pre_last_elem (rplacd pre_last_elem first_elem))
                            (first_elem (rplacd first_elem nil))
                      )
                            last_elem
                )
          )
    )
)


(defun swap-first-last(lst)
    (cond ((< (length lst) 2) lst)
        (t (cons (
                mapcon #'(lambda (el);поиск последнего элемента в списке
                    (cond
                        (
                            (null (cdr el)) (car el)
                        )
                    )
                ) lst
            )
            (
                nconc (
                    mapcon #'(lambda (el);берем все элементы начиная со второго кроме последнего
                        (cond
                            (
                                (cdr el) (cons (car el) nil)
                            )
                        )
                    )
                    (cdr lst)
                )
                (cons (car lst) nil);добавляем первый элемент в конец
            )
        ))
    )
)

;5 задача--------------------------------




(defun my_nthcdr (lst num);поиск списка, начинающегося с num в списке
    (let ((after (- (length lst) num )))
                (maplist #'(lambda (el)
                    (cond
                        (
                            (equal (length el) after) el
                        )
                    )
                ) lst)
    )
)


(defun reduce_or (lst); применяем or к списку
    (reduce #'(lambda (prev lst) (or prev lst)) lst)
)

(defun from_to_list (lst from to);поиск списка, начинающегося с from до to в списке(все включительно)
    (cond ((or (< from 0) (>= from (length lst)) (< to 0) (>= to (length lst))) nil)
        (t
            (let* ((to_position_in_new_list (- to from));позиция элемента с индексом to в списке без from
                    (from_list (reduce_or (my_nthcdr lst from))); список начиная с from
            )
            (reverse (reduce_or (my_nthcdr (reverse from_list) (- (length from_list) (+ to_position_in_new_list 1)))))
            )
        )
    )
)

(defun my_nth_list (pos lst);если не nil, оборачиваем в список, иначе просто nil
    (let ((answer (nth pos lst)))
        (if answer (list answer) nil)
    )
)

(defun swap-two-element(lst num1 num2)
    (cond ((equal num1 num2) lst)
            ((or (>= num1 (length lst)) (>= num2 (length lst))) lst)
            (t (let ((before (min num1 num2))
                    (after (max num1 num2))
                )
                (nconc
                    (from_to_list lst 0 (- before 1))
                    (my_nth_list after lst)
                    (from_to_list lst (+ before 1) (- after 1))
                    (my_nth_list before lst)
                    (from_to_list lst (+ after 1) (- (length lst) 1))
                )
            ))
    )
)


;6------------------------------------------------------------


;рекурсия
(defun get_k_cdr_elem (lst k)
    (if (equal k 0) lst (get_k_cdr_elem (cdr lst) (- k 1)))
)


(defun create_circle_list (lst)
    (rplacd (get_k_cdr_elem lst (- (length lst) 1)) lst)
)

(defun swap-to-right (lst k)
    (cond ((null lst) nil)
    (t (let* ((lst_length (length lst))
            (mod_k (mod k lst_length))
            (circle_list (create_circle_list lst))
            (new_first_elem (get_k_cdr_elem circle_list (- lst_length (- mod_k 1))))
            (last_elem (rplacd (get_k_cdr_elem circle_list (- lst_length mod_k)) nil))
    )
        new_first_elem
    ))
    )
)

(defun swap-to-left (lst k)
    (cond ((null lst) nil)
    (t(let* ((lst_length (length lst))
            (mod_k (mod k lst_length))
            (circle_list (create_circle_list lst))
            (new_first_elem (get_k_cdr_elem circle_list (+ mod_k 1)))
            (last_elem (rplacd (get_k_cdr_elem circle_list mod_k) nil))
    )
        new_first_elem
    ))
    )
)

;7------------------------------------------------------------------

(defun multiply_numbers (lst number)
    (mapcar (lambda (x) (* x number)) lst)
)

(defun multiply (lst number)
    (mapcar (lambda (obj) 
                (cond ((numberp obj) (* obj number))
                         ((listp obj) (multiply obj number))
                        (t obj)
                )
            )  
    lst)
)

;8-------------------------------------------------------------------

(defun select-between (lst from to)
    (let ((min_el (min from to))
            (max_el (max from to))
    )
        (apply #'nconc (;конкатенирую все подсписки списка
            mapcar (lambda (el) (if (and (< el max_el) (> el min_el)) (cons el nil) nil)) lst
        ))
    )
)

;что-то наподобие сортировки вставками (хвостовая рекурсия)
(defun sort_insert (head before lst elem)
    (cond
        ((null head) (cons elem head))
        ((null lst) (let ((change_list (rplacd before (cons elem nil))))
                            head
                        )
        )
        (t (if (< elem (car lst)) (let* (  (list_elem (cons elem nil))
                                        (next_list (rplacd list_elem lst));вставляем элемент между before и lst
                                        (before_list (if (null before) nil (rplacd before list_elem)))
                                        )
                                        (if (null before) next_list head)
                                    )
                (sort_insert head lst (cdr lst) elem)
            )
        )
    )
)

(defun check (result lst from to)
    (if (null lst) result 
        (check (if (and (< (car lst) to) (> (car lst) from)) (
            sort_insert result nil result (car lst)
        ) result) (cdr lst) from to)
    )
)

;отсортированный вариант
(defun select-between (lst from to)
    (let ((min_el (min from to))
            (max_el (max from to))
    )
        (let ((result nil))
            (check result lst min_el max_el)
        )
    )
)