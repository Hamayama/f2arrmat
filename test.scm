;;;
;;; Test f2arrmat
;;;

(add-load-path "." :relative)
(use gauche.test)
(use gauche.array)
(use gauche.uvector)
(use gauche.reload)

(define (nearly=? x y :optional (precision 1e-12))
  (<= (abs (- x y)) precision))

(test-start "f2arrmat")
(use f2arrmat)
(test-module 'f2arrmat)

(define A (f2-array 0 2 0 2 1 2 3 4))
(define B (f2-array 0 2 0 2 5 6 7 8))
(define D (f2-array 0 2 0 2 1 1 1 1))
(define E (f2-array 0 2 0 1 1 2))
(define F (f2-array 0 2 0 2 0 0 0 0))
(define G (f2-array 0 0 0 0))
(define H (f2-array 0 4 0 4 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
(define J (f2-array 0 4 0 4 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8
                            0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6))
(define K (f2-array 0 2 0 3 1 2 3 4 5 6))
(define L (f2-array 0 2 0 3 -2 -1 0 1 2 3))

(define (run-test)
  (test* "f2-array-ref 1" 1
         (f2-array-ref A  0  0) nearly=?)
  (test* "f2-array-ref 2" 4
         (f2-array-ref A  1  1) nearly=?)
  (test* "f2-array-ref 3" (test-error <error>)
         (f2-array-ref A -1  0))
  (test* "f2-array-ref 4" (test-error <error>)
         (f2-array-ref A  0  3))
  (test* "f2-array-ref 5" (test-error <error>)
         (f2-array-ref G  0  0))

  (let ((A1 (f2-array-copy A))
        (G1 (f2-array-copy G)))
    (test* "f2-array-set! 1" 100
           (begin (f2-array-set! A1  0  0  100) (f2-array-ref  A1  0  0)) nearly=?)
    (test* "f2-array-set! 2" 400
           (begin (f2-array-set! A1  1  1  400) (f2-array-ref  A1  1  1)) nearly=?)
    (test* "f2-array-set! 3" (test-error <error>)
           (f2-array-set! A1 -1  0  100))
    (test* "f2-array-set! 4" (test-error <error>)
           (f2-array-set! A1  0  3  400))
    (test* "f2-array-set! 5" (test-error <error>)
           (f2-array-set! G1  0  0  500))
    )

  (let ((A1 (f2-array-copy A))
        (G1 (f2-array-copy G)))
    (test* "f2-array-fill! 1" D
           (begin (f2-array-fill! A1 1) A1) f2-array-nearly=?)
    (test* "f2-array-fill! 2" G
           (begin (f2-array-fill! G1 1) G1) f2-array-nearly=?)
    )

  (test* "f2-array-copy 1" A
         (f2-array-copy A)  f2-array-nearly=?)
  (test* "f2-array-copy 2" L
         (f2-array-copy (array (shape 0 2 0 3) -2 -1 0 1 2 3)) f2-array-nearly=?)

  (let1 A1 (make-f2-array 0 2 0 2)
    (test* "f2-array-copy! 1" A
           (begin (f2-array-copy! A1 A) A1) f2-array-nearly=?)
    )

  (test* "f2-array-map 1" B
         (f2-array-map (lambda (x) (+ x 4)) A) f2-array-nearly=?)
  (test* "f2-array-map 2" L
         (f2-array-map (lambda (x) (- x 1))
                       (array (shape 0 2 0 3) -1 0 1 2 3 4)) f2-array-nearly=?)

  (let1 A1 (make-f2-array 0 2 0 2)
    (test* "f2-array-map! 1" B
           (begin (f2-array-map! A1 (lambda (x) (+ x 4)) A) A1) f2-array-nearly=?)
    )

  (test* "make-f2-array 1" F
         (make-f2-array 0 2 0 2) f2-array-nearly=?)
  (test* "make-f2-array 2" D
         (make-f2-array 0 2 0 2 1) f2-array-nearly=?)
  (test* "make-f2-array 3" (test-error <error>)
         (make-f2-array 0 2 0 2 1 2))

  (test* "make-f2-array-same-shape 1" F
         (make-f2-array-same-shape A) f2-array-nearly=?)
  (test* "make-f2-array-same-shape 2" D
         (make-f2-array-same-shape A 1) f2-array-nearly=?)
  (test* "make-f2-array-same-shape 3" (test-error <error>)
         (make-f2-array-same-shape A 1 2))

  (test* "f2-array 1" A
         (f2-array 0 2 0 2 1 2 3 4) f2-array-nearly=?)

  (test* "f2-array-nearly=? 1" #t
         (f2-array-nearly=? A (f2-array 0 2 0 2 1 2 3 4)))
  (test* "f2-array-nearly=? 2" #t
         (f2-array-nearly=? A (f2-array 0 2 0 2 1 2 3 (+ 4 1e-13))))
  (test* "f2-array-nearly=? 3" #f
         (f2-array-nearly=? A (f2-array 0 2 0 2 1 2 3 (+ 4 1e-11))))
  (test* "f2-array-nearly=? 4" #f
         (f2-array-nearly=? F (f2-array 0 2 0 2 0 0 0 1e-13)))

  (test* "f2-array-nearly-zero? 1" #t
         (f2-array-nearly-zero? F))
  (test* "f2-array-nearly-zero? 2" #t
         (f2-array-nearly-zero? (f2-array 0 2 0 2 0 0 0 1e-13)))
  (test* "f2-array-nearly-zero? 3" #f
         (f2-array-nearly-zero? (f2-array 0 2 0 2 0 0 0 1e-11)))

  (let ((A1 (f2-array-copy A))
        (L1 (f2-array-copy L)))
    (define (sigmoid x) (/. 1 (+ 1 (exp (- x)))))

    (test* "f2-array-add-elements  1" #,(<f64array> (0 2 0 2) 3 6 9 12)
           (f2-array-add-elements  A A A) f2-array-nearly=?)
    (test* "f2-array-add-elements  2" #,(<f64array> (0 2 0 2) 4 5 6 7)
           (f2-array-add-elements  A 1 2) f2-array-nearly=?)
    (test* "f2-array-add-elements  3" A
           (f2-array-add-elements  A) f2-array-nearly=?)

    (test* "f2-array-add-elements! 1" #,(<f64array> (0 2 0 2) 3 6 9 12)
           (begin (f2-array-add-elements! A1 A A A) A1) f2-array-nearly=?)
    (test* "f2-array-add-elements! 2" #,(<f64array> (0 2 0 2) 4 5 6 7)
           (begin (f2-array-add-elements! A1 A 1 2) A1) f2-array-nearly=?)
    (test* "f2-array-add-elements! 3" A
           (begin (f2-array-add-elements! A1 A) A1) f2-array-nearly=?)

    (test* "f2-array-sub-elements  1" #,(<f64array> (0 2 0 2) -1 -2 -3 -4)
           (f2-array-sub-elements  A A A) f2-array-nearly=?)
    (test* "f2-array-sub-elements  2" #,(<f64array> (0 2 0 2) -2 -1  0  1)
           (f2-array-sub-elements  A 1 2) f2-array-nearly=?)
    (test* "f2-array-sub-elements  3" A
           (f2-array-sub-elements  A) f2-array-nearly=?)

    (test* "f2-array-sub-elements! 1" #,(<f64array> (0 2 0 2) -1 -2 -3 -4)
           (begin (f2-array-sub-elements! A1 A A A) A1) f2-array-nearly=?)
    (test* "f2-array-sub-elements! 2" #,(<f64array> (0 2 0 2) -2 -1  0  1)
           (begin (f2-array-sub-elements! A1 A 1 2) A1) f2-array-nearly=?)
    (test* "f2-array-sub-elements! 3" A
           (begin (f2-array-sub-elements! A1 A) A1) f2-array-nearly=?)

    (test* "f2-array-mul  1" #,(<f64array> (0 2 0 2) 7 10 15 22)
           (f2-array-mul  A A) f2-array-nearly=?)

    (test* "f2-array-mul! 1" #,(<f64array> (0 2 0 2) 7 10 15 22)
           (begin (f2-array-mul! A1 A A) A1) f2-array-nearly=?)

    (test* "f2-array-mul-elements  1" #,(<f64array> (0 2 0 2) 1 8 27 64)
           (f2-array-mul-elements  A A A) f2-array-nearly=?)
    (test* "f2-array-mul-elements  2" #,(<f64array> (0 2 0 2) 2 4 6 8)
           (f2-array-mul-elements  A 1 2) f2-array-nearly=?)
    (test* "f2-array-mul-elements  3" A
           (f2-array-mul-elements  A) f2-array-nearly=?)

    (test* "f2-array-mul-elements! 1" #,(<f64array> (0 2 0 2) 1 8 27 64)
           (begin (f2-array-mul-elements! A1 A A A) A1) f2-array-nearly=?)
    (test* "f2-array-mul-elements! 2" #,(<f64array> (0 2 0 2) 2 4 6 8)
           (begin (f2-array-mul-elements! A1 A 1 2) A1) f2-array-nearly=?)
    (test* "f2-array-mul-elements! 3" A
           (begin (f2-array-mul-elements! A1 A) A1) f2-array-nearly=?)

    (test* "f2-array-div-elements  1" #,(<f64array> (0 2 0 2) 1/1 1/2 1/3 1/4)
           (f2-array-div-elements  A A A) f2-array-nearly=?)
    (test* "f2-array-div-elements  2" #,(<f64array> (0 2 0 2) 1/2 2/2 3/2 4/2)
           (f2-array-div-elements  A 1 2) f2-array-nearly=?)
    (test* "f2-array-div-elements  3" A
           (f2-array-div-elements  A) f2-array-nearly=?)

    (test* "f2-array-div-elements! 1" #,(<f64array> (0 2 0 2) 1/1 1/2 1/3 1/4)
           (begin (f2-array-div-elements! A1 A A A) A1) f2-array-nearly=?)
    (test* "f2-array-div-elements! 2" #,(<f64array> (0 2 0 2) 1/2 2/2 3/2 4/2)
           (begin (f2-array-div-elements! A1 A 1 2) A1) f2-array-nearly=?)
    (test* "f2-array-div-elements! 3" A
           (begin (f2-array-div-elements! A1 A) A1) f2-array-nearly=?)

    (test* "f2-array-pow  1" #,(<f64array> (0 2 0 2) 25 36 49 64)
           (f2-array-pow  B 2) f2-array-nearly=?)

    (test* "f2-array-pow! 1" #,(<f64array> (0 2 0 2) 25 36 49 64)
           (f2-array-pow! A1 B 2) f2-array-nearly=?)

    (test* "f2-array-exp  1" (f2-array 0 2 0 2 (exp 5) (exp 6) (exp 7) (exp 8))
           (f2-array-exp  B) f2-array-nearly=?)

    (test* "f2-array-exp! 1" (f2-array 0 2 0 2 (exp 5) (exp 6) (exp 7) (exp 8))
           (f2-array-exp! A1 B) f2-array-nearly=?)

    (test* "f2-array-log  1" (f2-array 0 2 0 2 (log 5) (log 6) (log 7) (log 8))
           (f2-array-log  B) f2-array-nearly=?)

    (test* "f2-array-log! 1" (f2-array 0 2 0 2 (log 5) (log 6) (log 7) (log 8))
           (f2-array-log! A1 B) f2-array-nearly=?)

    (test* "f2-array-sinh  1" (f2-array 0 2 0 2 (sinh 5) (sinh 6) (sinh 7) (sinh 8))
           (f2-array-sinh  B) f2-array-nearly=?)

    (test* "f2-array-sinh! 1" (f2-array 0 2 0 2 (sinh 5) (sinh 6) (sinh 7) (sinh 8))
           (f2-array-sinh! A1 B) f2-array-nearly=?)

    (test* "f2-array-cosh  1" (f2-array 0 2 0 2 (cosh 5) (cosh 6) (cosh 7) (cosh 8))
           (f2-array-cosh  B) f2-array-nearly=?)

    (test* "f2-array-cosh! 1" (f2-array 0 2 0 2 (cosh 5) (cosh 6) (cosh 7) (cosh 8))
           (f2-array-cosh! A1 B) f2-array-nearly=?)

    (test* "f2-array-tanh  1" (f2-array 0 2 0 2 (tanh 5) (tanh 6) (tanh 7) (tanh 8))
           (f2-array-tanh  B) f2-array-nearly=?)

    (test* "f2-array-tanh! 1" (f2-array 0 2 0 2 (tanh 5) (tanh 6) (tanh 7) (tanh 8))
           (f2-array-tanh! A1 B) f2-array-nearly=?)

    (test* "f2-array-sigmoid  1" (f2-array
                                  0 2 0 2
                                  (sigmoid 5) (sigmoid 6) (sigmoid 7) (sigmoid 8))
           (f2-array-sigmoid  B) f2-array-nearly=?)
    (test* "f2-array-sigmoid  2" (f2-array-map sigmoid A) (f2-array-sigmoid A))

    (test* "f2-array-sigmoid! 1" (f2-array
                                  0 2 0 2
                                  (sigmoid 5) (sigmoid 6) (sigmoid 7) (sigmoid 8))
           (f2-array-sigmoid! A1 B) f2-array-nearly=?)

    (test* "f2-array-relu  1" #,(<f64array> (0 2 0 3) 0 0 0 1 2 3)
           (f2-array-relu  L) f2-array-nearly=?)

    (test* "f2-array-relu! 1" #,(<f64array> (0 2 0 3) 0 0 0 1 2 3)
           (f2-array-relu! L1 L) f2-array-nearly=?)

    (test* "f2-array-step  1" #,(<f64array> (0 2 0 3) 0 0 0 1 1 1)
           (f2-array-step  L) f2-array-nearly=?)

    (test* "f2-array-step! 1" #,(<f64array> (0 2 0 3) 0 0 0 1 1 1)
           (f2-array-step! L1 L) f2-array-nearly=?)
    )

  (test* "f2-array-sum  1" 26   (f2-array-sum  B) nearly=?)
  (test* "f2-array-sum  2" (rlet1 ret 0
                             (f2-array-map (lambda (x) (inc! ret x)) A))
         (f2-array-sum  A) nearly=?)

  (test* "f2-array-min  1" 5    (f2-array-min  B) nearly=?)

  (test* "f2-array-max  1" 8    (f2-array-max  B) nearly=?)

  (test* "f2-array-mean 1" 26/4 (f2-array-mean B) nearly=?)

  (test* "f2-array-trace 1" 5 (f2-array-trace A) nearly=?)
  (test* "f2-array-trace 2" 6 (f2-array-trace K) nearly=?)

  (test* "f2-array-determinant 1" -2
         (f2-array-determinant A) nearly=?)
  (test* "f2-array-determinant 2" (test-error <error>)
         (f2-array-determinant K))

  (test* "f2-array-identity  1" #,(<f64array> (0 2 0 2) 1 0 0 1)
         (f2-array-identity  2 2) f2-array-nearly=?)
  (test* "f2-array-identity  2" #,(<f64array> (0 2 0 3) 1 0 0 0 1 0)
         (f2-array-identity  2 3) f2-array-nearly=?)

  (test* "f2-array-identity! 1" #,(<f64array> (0 2 0 2) 1 0 0 1)
         (f2-array-identity! (make-f2-array 0 2 0 2)) f2-array-nearly=?)
  (test* "f2-array-identity! 2" #,(<f64array> (0 2 0 3) 1 0 0 0 1 0)
         (f2-array-identity! (make-f2-array 0 2 0 3)) f2-array-nearly=?)

  (test* "f2-array-transpose  1" #,(<f64array> (0 2 0 2) 1 3 2 4)
         (f2-array-transpose  A) f2-array-nearly=?)
  (test* "f2-array-transpose  2" #,(<f64array> (0 3 0 2) -2 1 -1 2 0 3)
         (f2-array-transpose  L) f2-array-nearly=?)

  (test* "f2-array-transpose! 1" #,(<f64array> (0 2 0 2) 1 3 2 4)
         (f2-array-transpose! (make-f2-array 0 2 0 2) A) f2-array-nearly=?)
  (test* "f2-array-transpose! 2" #,(<f64array> (0 3 0 2) -2 1 -1 2 0 3)
         (f2-array-transpose! (make-f2-array 0 3 0 2) L) f2-array-nearly=?)

  (test* "f2-array-inverse  1" #,(<f64array> (0 2 0 2) -2 1 1.5 -0.5)
         (f2-array-inverse  A) f2-array-nearly=?)
  (test* "f2-array-inverse  2" #,(<f64array> (0 2 0 2) +inf.0 -inf.0 -inf.0 +inf.0)
         (f2-array-inverse  D)
         (lambda (exp ans) (if (or (equal? exp ans) (not ans)) #t #f)))

  (test* "f2-array-inverse! 1" #,(<f64array> (0 2 0 2) -2 1 1.5 -0.5)
         (f2-array-inverse! (make-f2-array 0 2 0 2) A) f2-array-nearly=?)
  (test* "f2-array-inverse! 2" #,(<f64array> (0 2 0 2) +inf.0 -inf.0 -inf.0 +inf.0)
         (f2-array-inverse! (make-f2-array 0 2 0 2) D)
         (lambda (exp ans) (if (or (equal? exp ans) (not ans)) #t #f)))

  (test* "f2-array-solve  1" #,(<f64array> (0 2 0 1) 0 0.5)
         (f2-array-solve  A E) f2-array-nearly=?)
  (test* "f2-array-solve  2" #,(<f64array> (0 2 0 2) 1 0 0 1)
         (f2-array-solve  A A) f2-array-nearly=?)
  (test* "f2-array-solve  3" #,(<f64array> (0 2 0 1) -inf.0 +inf.0)
         (f2-array-solve  D E)
         (lambda (exp ans) (if (or (equal? exp ans) (test-error? ans)) #t #f)))

  (test* "f2-array-solve! 1" #,(<f64array> (0 2 0 1) 0 0.5)
         (f2-array-solve! (make-f2-array 0 2 0 1) A E) f2-array-nearly=?)
  (test* "f2-array-solve! 2" #,(<f64array> (0 2 0 2) 1 0 0 1)
         (f2-array-solve! (make-f2-array 0 2 0 2) A A) f2-array-nearly=?)
  (test* "f2-array-solve! 3" #,(<f64array> (0 2 0 1) -inf.0 +inf.0)
         (f2-array-solve! (make-f2-array 0 2 0 1) D E)
         (lambda (exp ans) (if (or (equal? exp ans) (test-error? ans)) #t #f)))

  (test* "f2-array-row  1" #,(<f64array> (0 1 0 3) 1 2 3)
         (f2-array-row  L 1) f2-array-nearly=?)

  (test* "f2-array-row! 1" #,(<f64array> (0 1 0 3) 1 2 3)
         (f2-array-row! (make-f2-array 0 1 0 3) L 1) f2-array-nearly=?)

  (test* "f2-array-col  1" #,(<f64array> (0 2 0 1) 0 3)
         (f2-array-col  L 2) f2-array-nearly=?)

  (test* "f2-array-col! 1" #,(<f64array> (0 2 0 1) 0 3)
         (f2-array-col! (make-f2-array 0 2 0 1) L 2) f2-array-nearly=?)

  (test* "f2-array-block  1" #,(<f64array> (0 2 0 3) 1 2 3 5 6 7)
         (f2-array-block H 0 0 2 3) f2-array-nearly=?)

  (test* "f2-array-block! 1" #,(<f64array> (0 2 0 3) 10 11 12 14 15 16)
         (f2-array-block! (make-f2-array 0 2 0 3) H 2 1 2 3) f2-array-nearly=?)

  (test* "f2-array-block-copy  1" #,(<f64array> (0 4 0 4) 1   2   3   0.4 5   6   7   0.8
                                                          0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6)
         (f2-array-block-copy H 0 0 2 3 J 0 0) f2-array-nearly=?)

  (test* "f2-array-block-copy! 1" #,(<f64array> (0 4 0 4) 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8
                                                          0.9 10  11  12  1.3 14  15  16)
         (f2-array-block-copy! (make-f2-array 0 4 0 4) H 2 1 2 3 J 2 1) f2-array-nearly=?)

  (let ((B1 (f2-array-copy B))
        (K1 (f2-array-copy K)))
    (test* "f2-array-ra+b! 1" #,(<f64array> (0 2 0 2) 7 10 13 16)
           (f2-array-ra+b! 2.0 A B1) f2-array-nearly=?)
    (test* "f2-array-ra+b! 2" #,(<f64array> (0 2 0 3) 0.0 1.5 3.0 4.5 6.0 7.5)
           (f2-array-ra+b! 0.5 L K1) f2-array-nearly=?)
    )

  (let ((D1 (f2-array-copy D))
        (D2 (f2-array-copy D)))
    (test* "f2-array-ab+c! 1" #,(<f64array> (0 2 0 2) 20 23 44 51)
           (f2-array-ab+c! A B D1 1.0 1.0 #f #f) f2-array-nearly=?)
    (test* "f2-array-ab+c! 2" #,(<f64array> (0 2 0 2) 9.0 18.0 18.0 40.5)
           (f2-array-ab+c! K K D2 0.5 2.0 #f #t) f2-array-nearly=?)
    )
  )

(test-section "use eigenmat and blasmat")
(test-log "*eigenmat-loaded*: ~a" (with-module f2arrmat *eigenmat-loaded*))
(test-log "*blasmat-loaded* : ~a" (with-module f2arrmat *blasmat-loaded*))
(run-test)
;(read-line)

(test-section "don't use eigenmat")
(select-module f2arrmat)
(define *disable-eigenmat* #t)
(define *disable-blasmat*  #f)
(select-module user)
(reload 'f2arrmat)
(test-log "*eigenmat-loaded*: ~a" (with-module f2arrmat *eigenmat-loaded*))
(test-log "*blasmat-loaded* : ~a" (with-module f2arrmat *blasmat-loaded*))
(run-test)

(test-section "don't use blasmat")
(select-module f2arrmat)
(define *disable-eigenmat* #f)
(define *disable-blasmat*  #t)
(select-module user)
(reload 'f2arrmat)
(test-log "*eigenmat-loaded*: ~a" (with-module f2arrmat *eigenmat-loaded*))
(test-log "*blasmat-loaded* : ~a" (with-module f2arrmat *blasmat-loaded*))
(run-test)

(test-section "don't use eigenmat and blasmat")
(select-module f2arrmat)
(define *disable-eigenmat* #t)
(define *disable-blasmat*  #t)
(select-module user)
(reload 'f2arrmat)
(test-log "*eigenmat-loaded*: ~a" (with-module f2arrmat *eigenmat-loaded*))
(test-log "*blasmat-loaded* : ~a" (with-module f2arrmat *blasmat-loaded*))
(run-test)

;; summary
(format (current-error-port) "~%~a" ((with-module gauche.test format-summary)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)

