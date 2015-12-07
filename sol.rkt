#lang racket
(define (proc x) (* x x))

(define the-empty-stream '())

(define (stream-car stream)
 (car stream))

(define-syntax cons-stream
 (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define (stream-cdr stream)
 (force (cdr stream)))

(define stream-null? null?)

(define (stream-proc proc. argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
(define (display-line x)
  (newline)
  (display x))

(define (stream-ref s n)
  (if (= n 0) (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))

(define (show x)
  (display-line x) x)

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval a b)
  (cond ((> a b) the-empty-stream)
    (else  (cons-stream a (stream-enumerate-interval (+ a 1) b)))
  ))
  
; Exercise 1
(define x (stream-map show (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))


(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams a b)
 (cons-stream
 (* (stream-car a) (stream-car b))
 (mul-streams (stream-cdr a)
 (stream-cdr b)))) 

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))


(define A (cons-stream 1 (scale-stream A 2)))

(define double (cons-stream 1 (scale-stream double 2)))



;(stream-ref A 0)
;(stream-ref A 1)
;(stream-ref A 2)
;(stream-ref A 3)
;(stream-ref A 4)
;(stream-ref A 5)
;(stream-ref A 6)

; Exercise 2

(define B (cons-stream 1 (mul-streams B integers)))

;(stream-ref B 0)
;(stream-ref B 1)
;(stream-ref B 2)
;(stream-ref B 3)
;(stream-ref B 4)
;(stream-ref B 5)
;(stream-ref B 6)
;(stream-ref B 7)
(define (stream-append s1 s2)
  (cons s1 s2))

;Exercise 3
(define (stream-pairs s)
 (if (stream-null? s) the-empty-stream
 (stream-append
  (stream-map
   (lambda (sn) (list (stream-car s) sn))
   (stream-cdr s))
  (stream-pairs (stream-cdr s)))))

(define onetof (cons-stream 1 (cons-stream 2 (cons-stream 3 (cons-stream 4 (cons-stream 5 null))))))

;(define res3 (stream-pairs onetof))
;(define r3 (stream-ref res3 0))
;(stream-ref r3 0)
;(stream-ref r3 1)
;(stream-ref r3 2)
;(stream-ref r3 3)
;(stream-ref res3 1)
;(stream-ref res3 2)
;(stream-ref res3 3)
;(stream-ref res3 4)
;; template
(define nil '())

(define (write-line term)
  (begin (display term)
         (newline)))

;;; power series operations

(define add-series add-streams)

(define scale-series scale-stream)

(define (negate-series s)
  (scale-series s -1))

(define (subtract-series s1 s2)
  (add-series s1 (negate-series s2)))

;;; display the first n coefficients of a series

(define (show-series s nterms)
  (if (= nterms 0)
      'done
      (begin (write-line (stream-car s))
             (show-series (stream-cdr s) (- nterms 1)))))

;;; return the coefficient of x^n

(define (series-coeff s n)  (stream-ref s n))

;;; create a (finite) series from a list of coefficients

(define (coeffs->series list-of-coeffs)
  (define zeros (stream-cons 0 zeros))
  (define (iter lst)
    (if (null? lst)
        zeros
        (stream-cons (car lst)
                     (iter (cdr lst)))))
  (iter list-of-coeffs))

;
;Problem 1 - must be copied to as a comment
;define non-neg-integers here
; needed for Problem 1 solution
(define non-neg-integers "program text to generate") 

(define (proc->series proc)
  (stream-map proc non-neg-integers))

;cosine and sine series
(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))
(define sine-series
  (cons-stream 0 (scale-stream (integrate-series cosine-series) -1)))

(define (integrate-series coeffs)
  (stream-map / coeffs integers))

;Exercise 4
;s1
(define s1 ones)
;s2
(define s2 integers)
;(show-series s1 8)
;(show-series s2 8)

;Exercise 5

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
            (add-series (scale-series (stream-cdr s2) (stream-car s1))
                     (mul-series (stream-cdr s1) s2))))

(define s1-times-s1 (mul-series s1 s1))

;(show-series s1-times-s1 9)

;(series-coeff (mul-series s2 s2) 10)

;Exercise 6
(define (invert-unit-series s)
  (cons-stream 1 (negate-series (mul-series (stream-cdr s) (invert-unit-series s)))))

;(show-series (invert-unit-series s1) 10)

;Exercise 7

;(define (div-series s1 s2)
;  (cons-stream (/ (stream-car s1) (stream-car s2)) (add-series (scale-series (stream-cdr s1) (/ 1 (stream-car s2)))
;                                                               (div-series s1 (stream-cdr s2)))))
(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "no constants that is 0")
      (mul-series s1 
		  (scale-series (invert-unit-series (scale-series s2  (/ 1 (stream-car s2))))  (/ 1 (stream-car s2)))
       )
  )
)

; Tests

;(show-series (div-series s2 s1) 8)
;1 1 1 1 1 1 1 1 1 
;(show-series (div-series s1 s2) 8)
;1 -1 0 0 0 00 0 0
;(show-series (div-series (mul-series s2 s2) s1) 10)
;1 3 6 10 15 21 28 36 45 55

;Exercise 8
(define (integrate-series-tail s)
  (define (integrate s n)
    (cons-stream (* (/ 1 n) (stream-car s))
                 (integrate (stream-cdr s) (+ n 1)))
  )

  (integrate s 1))
;Tests

;(define integrate-s2 (integrate-series-tail s2))
;(show-series integrate-s2 10)
;(define integrate-s1 (integrate-series-tail s1))
;(show-series integrate-s1 10)

;Exercise 9
(define sin
  (cons-stream 0 (integrate-series-tail cos)))
(define cos
  (cons-stream 1 (negate-series (integrate-series-tail sin))))

;Tests
;(show-series sin 10)
;(show-series cos 10)

;Exercise 10
;Because it will produce a loop when we want to use exp-series3
;it will do this procedure again and again so it will fill everything with constant and it will integrate constant forever

; Exercise 11
(define (derivative-series-tail s)
  (define (derivative s n)
    (cons-stream (* n (stream-car s))
                 (derivative (stream-cdr s) (+ n 1)))
  )

  (derivative s 1))
;Tests
;(show-series (derivative-series-tail s1) 10)
;1 2 3 4 5 6 7 8 9 10
;(show-series (derivative-series-tail s2) 10)
;1 4 9 16 25 36  49 64 81 100

(define (sqr x) (* x x))

(define (root-series stream)
  (stream-map (lambda (x) (sqrt x)) stream))

(define (sqr-series stream)
  (stream-map (lambda (x) (sqr x)) stream))

(define tan (cons-stream 0 (integrate-series-tail (sqr-series sec))))
(define sec (cons-stream 1 (root-series (derivative-series-tail tan) )))


(define (x-cot-x s)
  (define (x-cot-x s n)
    (cons-stream (* n (stream-car s))
                 (derivative (stream-cdr s) (+ n 1)))
  )

  (derivative s 1))
;Tests
(show-series tan 10)
;0 1 0 0.66 0 0.5333 0 0.4571
(show-series sec 10)
;1 0 1.414213 0 1.632999 0 17888 0 1.19123