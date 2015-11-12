;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file hw03skel_2015.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NURZHAN YERGOZHIN
(define false #f)
(define nil '())

;;; GENERIC ARITHMETIC OPERATIONS

;;;   GN = ({number} X RepNum) U ({rational} X RepRat) U 
;;;        ({complex} X RepCom) U ({polynomial} X RepPoly)

;;;   (GN, GN) --> GN
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
;(define (repnum->reprat x) (apply-generic 'repnum->reprat x))

;;;   GN --> GN
(define (negate x) (apply-generic 'negate x))

;;;   GN --> Bool
(define (=zero? x) (apply-generic '=zero? x))
;Exercise 4a, 4b
(define (equ? x y) (apply-generic 'equ? x y))

;;; a sample compound generic operation
;;;   GN --> GN
(define (square x) (mul x x))

;;; the ordinary  number package


(define (install-number-package)
  (define (tag x)
    (attach-tag 'number x))
  (define (make-number x) (tag x))
  (define (negate x) (tag (- x)))
  (define (zero? x) (= x 0))
  (define (add x y) (tag (+ x y)))
  (define (sub x y) (tag (- x y)))
  (define (mul x y) (tag (* x y)))
  (define (div x y) (tag (/ x y)))
  ;Exercise 4a
  ;Generic-OrdNum -> Sch-Bool
  (define (=number? x y) (= x y))

  (put 'make 'number make-number)
  (put 'negate '(number) negate)
  (put '=zero? '(number) zero?)
  (put 'add '(number number) add)
   ;Exercise 4a
  (put 'equ? '(number number) =number?)
  (put 'sub '(number number) sub)
  (put 'mul '(number number) mul)
  (put 'div '(number number) div)
  'done)

;;; Number Package User Interface

;;; A convenient external  procedure for building a generic
;;; ordinary number from Scheme numbers.

;;; Sch-Num --> ({number} X RepNum)
(define (create-number x) 
  ((get 'make 'number) x))

;;; the rational number package
(define (install-rational-package)
  (define (make-rat n d) (cons n d))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;negate-rat : Generic-RatNum -> Generic-RatNum
  (define (negate-rat x)
    (make-rat (negate (numer x)) (denom x)))
  
  ;zero-rat? : Generic-RatNum -> Sch-Bool
  (define (=zero-rat? x)
    (= (cdr (numer x)) 0))
  
  ;rational? : (Generic-RatNum, Generic-RatNum) -> Sch-Bool
  (define (=rational? x y)
    (equ? (mul (numer x) (denom y)) (mul (numer y) (denom x))))
  
  ;; Exercise 8
  ; repnum->reprat: RepNum -> RepRat
  (define (repnum->reprat x) (make-rat (create-number x)
                                       (create-number 1)))
  ; ((RepRat,RepRat) --> T) --> ((RepNum,RepRat) --> T)
  (define (RRmethod->NRmethod method)
    (lambda (num rat)
      (method
       (repnum->reprat num)
       rat)))
  ; Exercise 9
  ; ((RepRat,RepRat) --> T) --> ((RepRat,RepNum) --> T)
  (define (RRmethod->RNmethod method)
    (lambda (rat num)
      (method rat
              (repnum->reprat num))))
  
  (define (tag x) (attach-tag 'rational x))
  (define (make-rational n d) (tag (make-rat n d)))
  (define (add-rational x y) (tag (add-rat x y)))
  (define (sub-rational x y) (tag (sub-rat x y)))
  (define (mul-rational x y) (tag (mul-rat x y)))
  (define (div-rational x y) (tag (div-rat x y)))
  (define (negate-rational x) (tag (negate-rat x)))
 
  
  (put 'make 'rational make-rational)
  (put 'add '(rational rational) add-rational)
  (put 'sub '(rational rational) sub-rational)
  (put 'mul '(rational rational) mul-rational)  
  (put 'div '(rational rational) div-rational)
  ; EXERCISE 7A
  (put 'negate '(rational) negate-rational)
  (put '=zero? '(rational) =zero-rat?)
  (put 'equ? '(rational rational) =rational?)
  ; EXERCISE 10A
  (put 'add '(number rational) (RRmethod->NRmethod add-rational))
  (put 'sub '(number rational) (RRmethod->NRmethod sub-rational))
  (put 'mul '(number rational) (RRmethod->NRmethod mul-rational))
  (put 'div '(number rational) (RRmethod->NRmethod div-rational))
  (put 'equ? '(number rational)
       (RRmethod->NRmethod (lambda (x y)
                    (and (=rational? x x)
                         (=rational? y y)
                             (equ? (mul (numer x) (denom y))
                                 (mul (denom x) (numer y)))))))
  (put 'add '(rational number) (RRmethod->RNmethod add-rational))
  (put 'sub '(rational number) (RRmethod->RNmethod sub-rational))
  (put 'mul '(rational number) (RRmethod->RNmethod mul-rational))
  (put 'div '(rational number) (RRmethod->RNmethod div-rational))
  (put 'equ? '(rational number)
       (RRmethod->RNmethod (lambda (x y)
                             (and (=rational? x x)
                                  (=rational? y y)
                                  (equ? (mul (numer x) (denom y))
                                        (mul (denom x) (numer y)))))))
  'done)


;;; Rational Package User Interface

;;; A convenient procedure for building a generic rational
;;; from generic numbers.

;;;    (GN, GN) --> ({rational} X RepRat)
(define  (create-rational n d)
  ((get 'make 'rational) n d))


;;;  ((RepRat,RepRat) --> T) --> ((RepNum,RepRat) --> T)
(define (RRmethod->NRmethod method)
  (lambda (num rat)
    (method
     (repnum->reprat num)
     rat)))


;;; Complex Number Package, rectangular form a+bi

(define (install-complex-package)
  (define (make-com r i) (cons r i))
  (define (real x) (car x))
  (define (imag x) (cdr x))
  (define (add-com x y)
    (make-com (add (real x) (real y))
		  (add (imag x) (imag y))))
  (define (sub-com x y)
    (make-com (sub (real x) (real y))
		  (sub (imag x) (imag y))))
  (define (mul-com x y) 
    (make-com (sub (mul (real x) (real y)) (mul (imag x) (imag y)))
              ((mul (real x) (imag y))(mul (real y) (imag x)))))
  (define (div-com x y)  
    (let ((com-conj (complex-conjugate y)))
       (let ((x-times-com-conj (mul-com x com-conj))
             (y-times-com-conj (mul-com y com-conj)))
	 (make-com (div (real x-times-com-conj) (real y-times-com-conj))
		   (div (imag x-times-com-conj) (real y-times-com-conj))))))
  (define (complex-conjugate x)
    (make-com (real x) 
	      (negate (imag x))))
  (define (tag x) (attach-tag 'complex x))
  (define (make-complex n d) (tag (make-com n d)))
  (define (add-complex x y) (tag (add-com x y)))
  (define (sub-complex x y) (tag (sub-com x y)))
  (define (mul-complex x y) (tag (mul-com x y)))
  (define (div-complex x y) (tag (div-com x y)))
  (put 'make 'complex make-complex)
  (put 'add '(complex complex) add-complex)
  (put 'sub '(complex complex) sub-complex)
  (put 'mul '(complex complex) mul-complex)  
  (put 'div '(complex complex) div-complex)
  'done)


(define (create-complex r i)
  ((get 'make 'complex) r i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       This is the file type.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The bottom level typing system

(define attach-tag cons)

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad typed datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum) 
      (cdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))


;;; The apply-generic mechanism.  
;;;  Note that we don't deal with coercion here.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for the given types -- APPLY-GENERIC"
		 (list op type-tags))))))


;;; Code for creating the table, you don't need to worry about this.

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;
;ADD YOUR PROGRAM FROM HERE
;
(install-number-package)
(install-rational-package)
(install-complex-package)
; PUT YOUR COMMON ROUTINES HERE


; EXERCISE 1
; A) Generic-Num -> Generic-Num
; B) Mul is already implemented by apply-generic, so you can just use mul

; EXERCISE 2
; make-number and negate are (RepNum -> Generic-OrdNum)
; zero? is (RepNum-> Sch-Bool).

; EXERCISE 3
; Because make-number has only one argument inside of it (of type Sch-Num), others not.
; There's a tag for different types of numbers and for this one we have a tag 'number, which helps us to do this operation.

; EXERCISE 4

(define n2 (create-number 2))
(define n4 (create-number 4))
(define n6 (create-number 6))
(equ? n4 (sub n6 n2))

;EXERCISE 5A
; 2nd one is correct because in order to build rational number, we should use
; 2 generic-ordinary numbers to do this, where in first case we neglected this.
; All of our procedures in rational-package are installed so that we should use
; ordinary numbers as its arguments. Therefore, it will cause an error.

;EXERCISE 5B
(define r2/7 (create-rational (create-number 2) (create-number 7)))
(define r3/1 (create-rational (create-number 3) (create-number 1)))
(define rsq (square (sub r2/7 r3/1)))
rsq

;EXERCISE 6

;Because in order to do ‘add-rational’ we need to do add-rat,
;where ‘add-rat’ function calls ‘add’ and ‘mul’ procedure which is within the table makes additional of ordinary numbers.
;Therefore, if we’ll give for ‘add-rat’ procedure name add, then there will be an error, because we have this operation inside our table.
;Having two similar names will cause troubles. We can’t use the same names for both functions.

;EXERCISE 7B
(define r1/1 (create-rational  (create-number 1) (create-number 1)))
(define r1/2 (create-rational  (create-number 1) (create-number 2)))
(define r1/3 (create-rational  (create-number 1) (create-number 3)))
(equ? (sub r1/1 (mul r1/2 r1/3)) (add r1/2 r1/3))

; EXERCISE 10B
(define n3 (create-number 3))
(define r3 (create-rational n3 (create-number 1)))
(define r2/7 (create-rational (create-number 2) (create-number 7)))
(equ? n3 r3)
(equ? (sub (add n3 r2/7) r2/7) n3)

; EXERCISE 11
;Using complex number package
(define c1 (create-complex (create-number 1) (create-number 3)))
(define c2 (create-complex (create-number 5) (create-number 0)))
(div c1 c2)
;Using rational number package
(define c3 (create-rational (create-complex (create-number 1) (create-number 3)) (create-number 5)))
c3


;EXERCISE 12
;Rational package
(define c4 (create-rational (create-complex (create-number 1) (create-number 3)) (create-complex
                                                                                   (create-number 1) (create-number 2))))
 c4
;Complex package
(define c5 (create-complex (create-number 1) (create-number 2)))
(div c1 c5)