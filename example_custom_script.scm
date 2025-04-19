(define randf (random-uniform))
(define randr (random-range-float 0.0 0.2))
(define randi (random-range-int0 10))

(define sr (* s randf))
(define Nr (+ N randi))

(parameters sr randr)
