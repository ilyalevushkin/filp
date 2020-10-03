(mapcan #'reverse '((1 2 3) (a b c) (100 200 300)))

result: (3 2 1 C B A 300 200 100)

(mapcar #'reverse '((1 2 3) (a b c) (100 200 300)))

result: ((3 2 1) (C B A) (300 200 100))

(mapcon #'reverse '((1 2 3) (a b c) (100 200 300)))

result: ((100 200 300) (A B C) (1 2 3) (100 200 300) (A B C) (100 200 300))

(maplist #'reverse '((1 2 3) (a b c) (100 200 300)))

result: ( ((100 200 300) (A B C) (1 2 3)) ((100 200 300) (A B C)) ((100 200 300)) )

(nconc '(1) nil '(2))
result: (1 2)