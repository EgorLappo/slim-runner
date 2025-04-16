(define rng (new-rng seed))
(define randf (random-uniform rng))
(define randr (random-range-float rng 0.0 0.2))
(define randi (random-range-int rng 0 10))

(define sr (* s randf))
(define Nr (+ N randi))

(parameters sr randr)
