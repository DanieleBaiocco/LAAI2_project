#lang gamble
(require gamble/viz)

;It does not terminate If I ask for (test 0 (p))
(define p (lambda() (p)))
(define (test x y)
(if (= x 0) 0 y))

(define (a-plus-abs-b a b)
((if (> b 0) + -) a b))

(define tollerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tollerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ration-fixed-point)
  (fixed-point (lambda(x)(/
                          (+
                           (+ 1 (/ 1 x)) x)
                          2)) 1.0))

(define (general-logarithm trans res)
  (fixed-point (trans res) 1.1))

;without average damping
(define (log-no-avg res)
  (general-logarithm (lambda(r)(lambda(x)(/ (log r) (log x)))) res))
;with average damping
(define (log-with-avg res)
  (general-logarithm (lambda(r)(lambda(x)(/( + (/ (log r)(log x)) x) 2))) res))

;infinite continued fraction
;recursive process (11 is the value to approximate 4 digits)
(define (cont-frac-rec n d k)
  (define (aux-cont-frac-rec i)
    (if (= i (+ k 1))
        0
        ( /(n i) (+ (d i) (aux-cont-frac-rec (+ i 1))))))
  (aux-cont-frac-rec 1))

(define (one-over-gr-appr-rec k)
  (cont-frac-rec (lambda(i) 1.0) (lambda(i) 1.0) k))

;iterative process
(define (cont-frac-iter n d k)
  (define (aux-cont-frac-iter res i)
    (if (= i 0)
        res
        (aux-cont-frac-iter (/ (n i) (+ (d i) res)) (- i 1))))
  (aux-cont-frac-iter 0  k))

(define (one-over-gr-appr-iter k)
  (cont-frac-iter (lambda(i) 1.0) (lambda(i) 1.0) k))

;De Fractionibus Continuis
;It's equal to e-2
(define (de-fractionibus-continuis k)
  (define (n i) 1.0)
  (define (d i) (if (= 2 (modulo i 3))
                    (* 2 (+ (quotient i 3) 1))
                    1))
  (cont-frac-rec n d k))
