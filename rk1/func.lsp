

;рекурсия

;Функция проверяет, принадлежит ли элемент множеству
(defun elem-in-uni (el uni)
    (cond ((null uni) nil)
            ((equal el (car uni)) T)
            (t (elem-in-uni el (cdr uni)))
    )
)

;рекурсивная функция, проверяющая является ли элемент списком, если да, 
;то вызывает саму себя для этого элемента, а потом вызывается рекурсивно для следующего элемента, 
;если нет, то просто вызывается рекурсивно для следующего элемента
(defun find-belong-help (lst uni count)
    (cond ((null lst) count)
            ((listp (car lst) ) (find-belong-help (cdr lst) uni (find-belong-help (car lst) uni count)))
            (t (find-belong-help (cdr lst) uni (if (elem-in-uni (car lst) uni) (+ count 1) count) ))
    )
)

;Функция обертка для рекурсивной функции find-belong-help
(defun find-belong (lst uni)
    (cond ((null lst) 0)
            (t (find-belong-help lst uni 0))
    )
)

;Рекурсивная реализация функции reverse
(defun my-reverse-help (lst res)
    (cond ((null lst) res)
        (t (my-reverse-help (cdr lst) (cons (car lst) res)))
    )
)

;Обертка для рекурсивной функции my-reverse-help
(defun my-reverse (lst)
    (my-reverse-help lst nil)
)


;Рекурсивная функция, добавляющая в голову первого списка по элементу из второго, а затем переворачивая все
(defun my-append-two-help (res lst2)
    (cond ((null lst2) (my-reverse res))
            (t (my-append-two-help (cons (car lst2) res) (cdr lst2)))
    )
)

;Функция append, написанная для 2 аргументов (функция обертка для рекурсивной функции my-append-two-help)
(defun my-append-two (lst1 lst2)
    (cond ((null lst1) lst2)
            (t (my-append-two-help (my-reverse lst1) lst2))
    )
)

;Рекурсивная функция, проверяющая дошли ли до конца списка. 
;Если да, то возвращаем реверсивный список, 
;так как до этого все элементы, которые мы кидали в result, мы кидали в голову.
;Если нет, то проверяем, дошли ли мы до этого элемента. Если да, то добавляем в голову рещультата наше value, 
;переворачиваем список и конкатенируем его с оставшемся списком lst.
(defun change-value-help (lst k cur_pos res value)
    (cond ((null lst) (my-reverse (cons value res)))
            ((equal k cur_pos) (my-append-two (my-reverse (cons value res)) (cdr lst)))
            (t (change-value-help (cdr lst) k (+ 1 cur_pos) (cons (car lst) res) value))
    )
)

;функция обертка для рекурсивной функции change-value-help
(defun change-value (lst uni k)
    (cond ((null lst) nil)
            (t  (change-value-help lst k 0 nil (find-belong lst uni)))
    )
)


;Рекурсивная функция. Смысл тот же, что и в сортировке вставками. 
;Проходимся по элементам списка до тех пор пока el не станет меньше (car lst)
(defun sort-insert (el lst res)
    (cond ((null lst) (my-reverse (cons el res)))
        ((not (numberp el)) (sort-insert el (cdr lst) (cons (car lst) res)))
        ((< el (car lst)) (my-append-two (my-reverse (cons el res)) lst))
        (t (sort-insert el (cdr lst) (cons (car lst) res)))
    )
)

;Рекурсивная функция. 
;Проходится по всем элементам множества и вставляет эти элементы в res с помощью sort-insert
(defun sort-up-help (uni res)
    (cond ((null uni) res)
            (t (sort-up-help (cdr uni) (sort-insert (car uni) res nil)))
    )
)

;Функция обертка для рекурсивной функции sort-up-help
(defun sort-up (uni)
    (cond ((null uni) nil)
            (t (sort-up-help uni nil))
    )
)


;Функционалы


;Функция проверяет, принадлежит ли элемент множеству или нет.
(defun elem-in-uni (el uni)
    (reduce #'(lambda (res x) (or res (if (equal el x) T nil))) uni :initial-value nil)
)


;Функция проходится по всему списку, и на каждой итерации проверяет, 
;принадлежит ли элемент множеству (с помощью функции elem-in-uni) или нет. Если да, то увеличивает res на 1
(defun find-belong (lst uni)
    (reduce #'(lambda (res x) 
        (cond ((null x) res)
                ((listp x) (+ (find-belong x uni) res))
                ((elem-in-uni x uni) (+ res 1))
                (t res)
        )
    ) lst :initial-value 0)
)


;Функция считает длину списка с помощбю функционалов
(defun my-length (lst)
    (reduce #'(lambda (res x) (+ res 1)) lst :initial-value 0)
)

;Функция добавляет элемент в конец списка
(defun throw-elem-in-tail (lst el)
    (my-reverse (cons el (my-reverse lst)))
)

;Функция проходится по всем элементам списка и когда k станет равным 0, 
;вставляет в полученный результат не элемент списка, а результат работы функции find-belong.
(defun change-value (lst uni k)
    (cond ((>= k (my-length lst)) (throw-elem-in-tail lst (find-belong lst uni)))
    (t (mapcar #'(lambda (x) (if (equal -1 (setq k (- k 1)) ) (find-belong lst uni) x)) lst))
    )
)

;Функция проходится по всем элементам списка и вызывает
; для каждого элемента функцию sort-insert (смотреть выше), 
;которая ставит i-ый элемент списка на соответствующую позицию в res
(defun sort-up (uni)
    (reduce #'(lambda (res x) (sort-insert x res nil)) uni :initial-value nil)
)