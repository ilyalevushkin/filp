(defun near_even (ar) (
    cond ((oddp ar) (+ ar 1))
         (T ar)
))

(defun abs_more (ar) (
    cond ((< ar 0) (- ar 1))
         (T (+ ar 1))
))

(defun list_increase (ar1 ar2) (
    cond ((< ar1 ar2) (list ar1 ar2))
         (T (list ar2 ar1))
))

(defun list_increase (ar1 ar2) (
    if (> ar1 ar2) (cons ar2 (cons ar1 Nil)) 
    (cons ar1 (cons ar2 Nil))
))

(defun middle (ar1 ar2 ar3) (
    cond ((and (< ar1 ar3) (> ar1 ar2)) T)
         (T Nil)
))

(defun not_less (ar1 ar2) (
    cond ((< ar1 ar2) Nil)
         (T T)
))

(defun middle_cond (ar1 ar2 ar3) (
    cond ( 
            (< ar1 ar3) (cond (
                                (> ar1 ar2) T
                              ) 
                              (T Nil) 
                        )
         )
         (T Nil)
))

(defun middle_and (ar1 ar2 ar3) (
    and (< ar1 ar3) (> ar1 ar2)
))

(defun middle_if (ar1 ar2 ar3) (
    if (< ar1 ar3) (if (> ar1 ar2) T Nil) Nil
))