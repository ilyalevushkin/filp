(defun sq_solver (a b c) 
    (
        cond (
                (<= a 1e-15) (cond (
                                        (<= b 1e-15) (cond (
                                                                (<= c 1e-15) "any"
                                                           ) 
                                                           (
                                                               T Nil
                                                           )
                                                     )
                                   )
                                   (
                                       T (/ (- c) b)
                                   )
                             )
             )
             (
                T (list
                    (/ (+
                            (- b) 
                            (sqrt 
                                (
                                    - (* b b) (* 4.0 a c)
                                )
                            )
                       ) 
                       (
                            * 2.0 a
                       )
                    )
                    (/ (- 
                            (- b) 
                            (sqrt 
                                (
                                    - (* b b) (* 4.0 a c)
                                )
                            )
                       ) 
                       (
                            * 2.0 a
                       )
                    )
                  )
             )
    )
)