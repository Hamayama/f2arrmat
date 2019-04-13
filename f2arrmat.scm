;; -*- coding: utf-8 -*-
;;
;; f2arrmat.scm
;; 2019-4-13 v1.10
;;
;; ＜内容＞
;;   Gauche で、行列 (2次元の f64array) を扱うためのモジュールです。
;;   gauche.array, eigenmat, blasmat モジュールよりも後に
;;   読み込んで使用することを想定しています。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/f2arrmat
;;
(define-module f2arrmat
  (use gauche.sequence)
  (use gauche.uvector)
  (use gauche.array)
  (use gauche.version)
  (use math.const)
  ;(use eigenmat)
  (define-module eigenmat)
  (import eigenmat)
  ;(use blasmat)
  (define-module blasmat)
  (import blasmat)
  (export
    f2-array-cache-on     f2-array-cache-off
    f2-array-ref          f2-array-set!
    f2-array-fill!
    f2-array-copy         f2-array-copy!
    f2-array-map          f2-array-map!
    make-f2-array         make-f2-array-same-shape
    f2-array
    f2-array-nearly=?     f2-array-nearly-zero?
    f2-array-add-elements f2-array-add-elements!
    f2-array-sub-elements f2-array-sub-elements!
    f2-array-mul          f2-array-mul!
    f2-array-mul-elements f2-array-mul-elements!
    f2-array-div-elements f2-array-div-elements!
    f2-array-pow          f2-array-pow!
    f2-array-exp          f2-array-exp!
    f2-array-log          f2-array-log!
    f2-array-sinh         f2-array-sinh!
    f2-array-cosh         f2-array-cosh!
    f2-array-tanh         f2-array-tanh!
    f2-array-sigmoid      f2-array-sigmoid!
    f2-array-relu         f2-array-relu!
    f2-array-step         f2-array-step!
    f2-array-sum
    f2-array-min
    f2-array-max
    f2-array-mean
    f2-array-trace
    f2-array-determinant
    f2-array-transpose    f2-array-transpose!
    f2-array-inverse      f2-array-inverse!
    f2-array-solve        f2-array-solve!
    f2-array-row          f2-array-row!
    f2-array-col          f2-array-col!
    f2-array-block        f2-array-block!
    f2-array-block-copy   f2-array-block-copy!
    f2-array-ra+b!        f2-array-ab+c!
    ))
(select-module f2arrmat)

;; eigenmat モジュールのロード
;; (存在しなければ使用しない)
;(define *disable-eigenmat* #t) ; 無効化フラグ
(define *eigenmat-loaded*
  (and (not (global-variable-ref (current-module) '*disable-eigenmat* #f))
       (load "eigenmat" :error-if-not-found #f)))

;; blasmat モジュールのロード
;; (存在しなければ使用しない)
;(define *disable-blasmat* #t)  ; 無効化フラグ
(define *blasmat-loaded*
  (and (not (global-variable-ref (current-module) '*disable-blasmat* #f))
       (load "blasmat" :error-if-not-found #f)))

;; s32vector をハッシュテーブルのキーに使えるようにする
;; (Gauche の開発最新版では、デフォルトで使用可能)
(when (guard (ex (else #t)) (default-hash #s32(1)) #f)
  ;; for Gauche v0.9.4
  (if (version<=? (gauche-version) "0.9.4")
    (define-method object-hash ((obj <s32vector>))
      (hash (s32vector->vector obj)))
    (define-method object-hash ((obj <s32vector>) rec-hash)
      (rec-hash (s32vector->vector obj)))))

;; 行列のキャッシュ(ハッシュテーブル)
(define use-f2-array-cache #t) ; 使用有無
(define f2-array-cache-table (make-hash-table 'equal?))

;; 行列のキャッシュ使用/未使用
(define (f2-array-cache-on)
  (set! use-f2-array-cache #t))
(define (f2-array-cache-off)
  (set! use-f2-array-cache #f))

;; gauche.array の shape の内部処理を上書き(高速化)
(select-module gauche.array)
(define (shape->start/end-vector shape)
  (let* ([rank (array-end shape 0)]
         [cnt  (iota rank)]
         [vec  (slot-ref shape 'backing-storage)])
    ;(values (map-to <s32vector> (^i (array-ref shape i 0)) cnt)
    ;        (map-to <s32vector> (^i (array-ref shape i 1)) cnt))))
    (values (map-to <s32vector> (^i (vector-ref vec (* i 2))) cnt)
            (map-to <s32vector> (^i (vector-ref vec (+ (* i 2) 1))) cnt))))
(select-module f2arrmat)

;; 行列の情報取得(エラーチェックなし)
(define-syntax array-rank
  (syntax-rules ()
    ((_ A)
     (s32vector-length (slot-ref A 'start-vector)))))
(define-syntax array-start
  (syntax-rules ()
    ((_ A dim)
     (s32vector-ref    (slot-ref A 'start-vector) dim))))
(define-syntax array-end
  (syntax-rules ()
    ((_ A dim)
     (s32vector-ref    (slot-ref A 'end-vector)   dim))))
(define-syntax array-length
  (syntax-rules ()
    ((_ A dim)
     (- (s32vector-ref (slot-ref A 'end-vector)   dim)
        (s32vector-ref (slot-ref A 'start-vector) dim)))))

;; 行列のタイプのチェック
(define-syntax check-array-type
  (syntax-rules ()
    ((_ A)
     (unless (eq? (class-of A) <f64array>)
       (error "f64array required")))
    ((_ A B ...)
     (unless (and (eq? (class-of A) <f64array>)
                  (eq? (class-of B) <f64array>) ...)
       (error "f64array required")))))

;; 行列の次元数のチェック
(define-syntax check-array-rank
  (syntax-rules ()
    ((_ A B ...)
     (unless (= (array-rank A) (array-rank B) ... 2)
       (error "array rank must be 2")))))

;; 行列の要素の参照(2次元のみ)(タイプと次元数のエラーチェックなし)
(define (f2-array-ref A i j)
  (let ((n1 (array-length A 0))
        (m1 (array-length A 1))
        (i1 (- i (array-start A 0)))
        (j1 (- j (array-start A 1))))
    (unless (and (>= i1 0) (>= j1 0) (< i1 n1) (< j1 m1))
      (error "invalid index value"))
    (f64vector-ref  (slot-ref A 'backing-storage) (+ (* i1 m1) j1))))

;; 行列の要素の設定(2次元のみ)(タイプと次元数のエラーチェックなし)
;; (戻り値は未定義)
(define (f2-array-set! A i j d)
  (let ((n1 (array-length A 0))
        (m1 (array-length A 1))
        (i1 (- i (array-start A 0)))
        (j1 (- j (array-start A 1))))
    (unless (and (>= i1 0) (>= j1 0) (< i1 n1) (< j1 m1))
      (error "invalid index value"))
    (f64vector-set! (slot-ref A 'backing-storage) (+ (* i1 m1) j1) d)))

;; 行列の要素の埋めつくし(エラーチェックなし)
;; (戻り値は未定義)
(define (f2-array-fill! A d)
  (f64vector-fill! (slot-ref A 'backing-storage) d))

;; 行列のコピー(エラーチェックなし)
(define (array-copy A)
  (make (class-of A)
    :start-vector    (slot-ref A 'start-vector)
    :end-vector      (slot-ref A 'end-vector)
    :mapper          (slot-ref A 'mapper)
    :backing-storage (let1 v (slot-ref A 'backing-storage)
                       (if (vector? v)
                         (vector-copy v)
                         (uvector-copy v)))))

;; 行列のコピー(破壊的変更版)(タイプかサイズが違うときはエラー)
;; (戻り値は未定義)
(define (array-copy! A B)
  (slot-set! A 'start-vector (slot-ref B 'start-vector))
  (slot-set! A 'end-vector   (slot-ref B 'end-vector))
  (slot-set! A 'mapper       (slot-ref B 'mapper))
  (let ((v1 (slot-ref A 'backing-storage))
        (v2 (slot-ref B 'backing-storage)))
    (cond
     ((and (vector? v1) (vector? v2)
           (= (vector-length v1) (vector-length v2)))
      (vector-copy! v1 0 v2))
     ((and (eq? (class-of v1) (class-of v2))
           (= (uvector-length v1) (uvector-length v2)))
      (uvector-copy! v1 0 v2))
     (else
      (error "can't copy array (type or size mismatch)")))))

;; f64array 用の array-copy (エラーチェックなし)
(define (f2-array-copy A)
  (if (eq? (class-of A) <f64array>)
    (array-copy A)
    (make <f64array>
      :start-vector    (slot-ref A 'start-vector)
      :end-vector      (slot-ref A 'end-vector)
      :mapper          (slot-ref A 'mapper)
      :backing-storage (coerce-to <f64vector> (slot-ref A 'backing-storage)))))

;; f64array 用の array-copy! (エラーチェックなし)
;; (戻り値は未定義)
(define f2-array-copy! array-copy!)

;; f64array 用の array-map (ただし shape の明示指定は不可)(エラーチェックなし)
(define (f2-array-map proc ar1 . rest)
  (rlet1 ar (if (eq? (class-of ar1) <f64array>)
              (array-copy ar1)
              (make-f64array (array-shape ar1)))
    (apply array-map! ar proc ar1 rest)))

;; f64array 用の array-map! (エラーチェックなし)
;; (戻り値は未定義)
(define f2-array-map! array-map!)

;; 行列の生成(2次元のみ)(キャッシュ使用)
(define (make-f2-array ns ne ms me :optional (maybe-init 0))
  (if use-f2-array-cache
    (let1 key (s32vector ns ne ms me)
      (if-let1 A (hash-table-get f2-array-cache-table key #f)
        (if (= maybe-init 0)
          (array-copy A)
          (rlet1 B (array-copy A)
            (f64vector-fill! (slot-ref B 'backing-storage) maybe-init)))
        (let1 B ((with-module gauche.array %make-array-internal-orig)
                 <f64array> (shape ns ne ms me) 0)
          (hash-table-put! f2-array-cache-table key B)
          (if (= maybe-init 0)
            (array-copy B)
            (rlet1 C (array-copy B)
              (f64vector-fill! (slot-ref C 'backing-storage) maybe-init))))))
    ((with-module gauche.array %make-array-internal-orig)
     <f64array> (shape ns ne ms me) maybe-init)))

;; 同じ shape の行列の生成(2次元のみ)
(define (make-f2-array-same-shape A :optional (maybe-init 0))
  (check-array-rank A)
  (make-f2-array (array-start A 0) (array-end A 0)
                 (array-start A 1) (array-end A 1) maybe-init))

;; 行列の初期化データ付き生成(2次元のみ)
(define (f2-array ns ne ms me . inits)
  (rlet1 ar (make-f2-array ns ne ms me 0)
    (f64vector-copy! (slot-ref ar 'backing-storage)
                     0 (list->f64vector inits))))

;; eigenmat モジュールの行列の生成を上書き(キャッシュを統一するため)
(select-module eigenmat)
(define make-eigen-array (with-module f2arrmat make-f2-array))
(define make-eigen-array-same-shape (with-module f2arrmat make-f2-array-same-shape))
(define eigen-make-array (with-module f2arrmat make-f2-array))
(define eigen-make-array-same-shape (with-module f2arrmat make-f2-array-same-shape))
(define eigen-array      (with-module f2arrmat f2-array))
(select-module f2arrmat)

;; gauche.array の行列の生成の内部処理を上書き(キャッシュ使用のため)
(select-module gauche.array)
(define (%make-array-internal-orig class shape . maybe-init)
  (receive (Vb Ve) (shape->start/end-vector shape)
    (make class
      :start-vector Vb
      :end-vector Ve
      :mapper (generate-amap Vb Ve)
      :backing-storage (apply (backing-storage-creator-of class)
                              (fold * 1 (s32vector-sub Ve Vb))
                              maybe-init))))
(define (make-array-internal class shape . maybe-init)
  (if (and (with-module f2arrmat use-f2-array-cache)
           (eq? class <f64array>)
           (equal? (slot-ref shape 'end-vector) #s32(2 2))) ; rank 2 only
    (receive (Vb Ve) (shape->start/end-vector shape)
      (let ((ns (s32vector-ref Vb 0))
            (ne (s32vector-ref Ve 0))
            (ms (s32vector-ref Vb 1))
            (me (s32vector-ref Ve 1)))
        (apply (with-module f2arrmat make-f2-array) ns ne ms me maybe-init)))
    (apply %make-array-internal-orig class shape maybe-init)))
(select-module f2arrmat)

;; 転置行列の生成(Gauche v0.9.7 の不具合対応(resの生成のところ) + 高速化)
(define (%array-transpose a :optional (dim1 0) (dim2 1))
  (let* ([sh (array-copy (array-shape a))]
         [rank (array-rank a)]
         ;[tmp0 (array-ref sh dim1 0)]
         ;[tmp1 (array-ref sh dim1 1)])
         [vec  (slot-ref sh 'backing-storage)]
         [vs1  (* dim1 2)]
         [ve1  (+ vs1  1)]
         [vs2  (* dim2 2)]
         [ve2  (+ vs2  1)]
         [tmp0 (vector-ref vec vs1)]
         [tmp1 (vector-ref vec ve1)])
    ;(array-set! sh dim1 0 (array-ref sh dim2 0))
    ;(array-set! sh dim1 1 (array-ref sh dim2 1))
    ;(array-set! sh dim2 0 tmp0)
    ;(array-set! sh dim2 1 tmp1)
    (vector-set! vec vs1 (vector-ref vec vs2))
    (vector-set! vec ve1 (vector-ref vec ve2))
    (vector-set! vec vs2 tmp0)
    (vector-set! vec ve2 tmp1)
    ;(rlet1 res (array-copy a)
    (rlet1 res ((with-module gauche.array make-array-internal) (class-of a) sh)
      (array-for-each-index a
        (^[vec1] (let* ([vec2 (vector-copy vec1)]
                        [tmp (vector-ref vec2 dim1)])
                   (vector-set! vec2 dim1 (vector-ref vec2 dim2))
                   (vector-set! vec2 dim2 tmp)
                   (array-set! res vec2 (array-ref a vec1))))
        (make-vector rank)))))


;; == 以下では、eigenmat モジュールがあれば使用する ==
;; (ただし f2-array-mul と f2-array-mul! は、blasmat モジュールがあれば優先的に使用する)


;; 行列の一致チェック
(define f2-array-nearly=?
  (if *eigenmat-loaded*
    eigen-array-nearly=?
    (lambda (ar1 ar2 :optional (precision 1e-12))
      (unless (= (array-rank ar1) (array-rank ar2))
        (error "array rank mismatch"))
      (dotimes (i (array-rank ar1))
        (unless (= (array-length ar1 i) (array-length ar2 i))
          (error "array shape mismatch")))
      (let ((norm1 0) (norm2 0) (norm3 0))
        (for-each (lambda (d1 d2)
                    (inc! norm1 (* d1 d1))
                    (inc! norm2 (* d2 d2))
                    (inc! norm3 (* (- d1 d2) (- d1 d2))))
                  (slot-ref ar1 'backing-storage)
                  (slot-ref ar2 'backing-storage))
        (<= (%sqrt norm3) (* precision (min (%sqrt norm1) (%sqrt norm2))))))))

;; 行列のゼロチェック
(define f2-array-nearly-zero?
  (if *eigenmat-loaded*
    eigen-array-nearly-zero?
    (lambda (ar1 :optional (precision 1e-12))
      (let1 norm1 0
        (for-each (lambda (d1) (inc! norm1 (* d1 d1)))
                  (slot-ref ar1 'backing-storage))
        (<= (%sqrt norm1) precision)))))

;; 行列の和を計算
(define f2-array-add-elements
  (if *eigenmat-loaded*
    (lambda (ar1 . rest)
      (rlet1 ar (f2-array-copy ar1)
        (for-each (lambda (arX) (eigen-array-add! ar ar arX)) rest)))
    array-add-elements))

;; 行列の和を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-add-elements!
  (if *eigenmat-loaded*
    (lambda (ar ar1 . rest)
      (cond
       ((null? rest)
        (f2-array-copy! ar ar1))
       (else
        (eigen-array-add! ar ar1 (car rest))
        (for-each (lambda (arX) (eigen-array-add! ar ar arX)) (cdr rest))))
      ar)
    (lambda (ar ar1 . rest)
      (f2-array-copy! ar (apply array-add-elements ar1 rest))
      ar)))

;; 行列の差を計算
(define f2-array-sub-elements
  (if *eigenmat-loaded*
    (lambda (ar1 . rest)
      (rlet1 ar (f2-array-copy ar1)
        (for-each (lambda (arX) (eigen-array-sub! ar ar arX)) rest)))
    array-sub-elements))

;; 行列の差を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-sub-elements!
  (if *eigenmat-loaded*
    (lambda (ar ar1 . rest)
      (cond
       ((null? rest)
        (f2-array-copy! ar ar1))
       (else
        (eigen-array-sub! ar ar1 (car rest))
        (for-each (lambda (arX) (eigen-array-sub! ar ar arX)) (cdr rest))))
      ar)
    (lambda (ar ar1 . rest)
      (f2-array-copy! ar (apply array-sub-elements ar1 rest))
      ar)))

;; 行列の積を計算(2次元のみ)
(define f2-array-mul
  (cond
   (*blasmat-loaded*
    (lambda (ar1 ar2)
      (let1 ar (make-f2-array 0 (array-length ar1 0)
                              0 (array-length ar2 1) 0)
        (blas-array-dgemm! ar1 ar2 ar 1.0 0.0 #f #f))))
   (*eigenmat-loaded*
    eigen-array-mul)
   (else
    array-mul)))

;; 行列の積を計算(破壊的変更版)(2次元のみ)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-mul!
  (cond
   (*blasmat-loaded*
    (lambda (ar ar1 ar2)
      (blas-array-dgemm! ar1 ar2 ar 1.0 0.0 #f #f)))
   (*eigenmat-loaded*
    eigen-array-mul!)
   (else
    (lambda (ar ar1 ar2)
      (f2-array-copy! ar (array-mul ar1 ar2))
      ar))))

;; 行列の要素の積を計算
(define f2-array-mul-elements
  (if *eigenmat-loaded*
    (lambda (ar1 . rest)
      (rlet1 ar (f2-array-copy ar1)
        (for-each (lambda (arX) (eigen-array-mul-elements! ar ar arX)) rest)))
    array-mul-elements))

;; 行列の要素の積を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-mul-elements!
  (if *eigenmat-loaded*
    (lambda (ar ar1 . rest)
      (cond
       ((null? rest)
        (f2-array-copy! ar ar1))
       (else
        (eigen-array-mul-elements! ar ar1 (car rest))
        (for-each (lambda (arX) (eigen-array-mul-elements! ar ar arX)) (cdr rest))))
      ar)
    (lambda (ar ar1 . rest)
      (f2-array-copy! ar (apply array-mul-elements ar1 rest))
      ar)))

;; 行列の要素の割り算を計算
(define f2-array-div-elements
  (if *eigenmat-loaded*
    (lambda (ar1 . rest)
      (rlet1 ar (f2-array-copy ar1)
        (for-each (lambda (arX) (eigen-array-div! ar ar arX)) rest)))
    array-div-elements))

;; 行列の要素の割り算を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-div-elements!
  (if *eigenmat-loaded*
    (lambda (ar ar1 . rest)
      (cond
       ((null? rest)
        (f2-array-copy! ar ar1))
       (else
        (eigen-array-div! ar ar1 (car rest))
        (for-each (lambda (arX) (eigen-array-div! ar ar arX)) (cdr rest))))
      ar)
    (lambda (ar ar1 . rest)
      (f2-array-copy! ar (apply array-div-elements ar1 rest))
      ar)))

;; 行列の要素のべき乗を計算
(define f2-array-pow
  (if *eigenmat-loaded*
    eigen-array-pow
    (lambda (ar1 r)
      (f2-array-map (lambda (x1) (%expt x1 r)) ar1))))

;; 行列の要素のべき乗を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-pow!
  (if *eigenmat-loaded*
    eigen-array-pow!
    (lambda (ar2 ar1 r)
      (f2-array-map! ar2 (lambda (x1) (%expt x1 r)) ar1)
      ar2)))

;; 行列の要素の演算生成用マクロ
;; (行列1個の演算)
(define-macro (define-f2-array-op-unary op1 func1)
  `(define ,(symbol-append 'f2-array- op1)
     (if *eigenmat-loaded*
       ,(symbol-append 'eigen-array- op1)
       (lambda (ar1)
         (f2-array-map ,func1 ar1)))))
;; (行列1個の演算(破壊的変更版))
(define-macro (define-f2-array-op-unary! op1 func1)
  `(define ,(symbol-append 'f2-array- op1 '!)
     (if *eigenmat-loaded*
       ,(symbol-append 'eigen-array- op1 '!)
       (lambda (ar2 ar1)
         (f2-array-map! ar2 ,func1 ar1)
         ar2))))

;; 行列の要素を指数として、自然対数の底eのべき乗を計算
(define-f2-array-op-unary  exp %exp)
(define-f2-array-op-unary! exp %exp)

;; 行列の要素に対して、自然対数を計算
(define-f2-array-op-unary  log %log)
(define-f2-array-op-unary! log %log)

;; 行列の要素に対して、sinh を計算
(define-f2-array-op-unary  sinh %sinh)
(define-f2-array-op-unary! sinh %sinh)

;; 行列の要素に対して、cosh を計算
(define-f2-array-op-unary  cosh %cosh)
(define-f2-array-op-unary! cosh %cosh)

;; 行列の要素に対して、tanh を計算
(define-f2-array-op-unary  tanh %tanh)
(define-f2-array-op-unary! tanh %tanh)

;; 行列の要素に対して、シグモイド関数を計算
(define-f2-array-op-unary  sigmoid (lambda (x1) (/. 1 (+ 1 (%exp (- x1))))))
(define-f2-array-op-unary! sigmoid (lambda (x1) (/. 1 (+ 1 (%exp (- x1))))))

;; 行列の要素に対して、ReLU関数を計算
(define-f2-array-op-unary  relu (lambda (x1) (if (> x1 0) x1 0)))
(define-f2-array-op-unary! relu (lambda (x1) (if (> x1 0) x1 0)))

;; 行列の要素に対して、ステップ関数を計算
(define-f2-array-op-unary  step (lambda (x1) (if (> x1 0) 1 0)))
(define-f2-array-op-unary! step (lambda (x1) (if (> x1 0) 1 0)))

;; 行列の要素の和を計算
(define f2-array-sum
  (if *eigenmat-loaded*
    eigen-array-sum
    (lambda (ar1)
      (let ((v1  (slot-ref ar1 'backing-storage))
            (ret 0))
        (for-each (lambda (x1) (inc! ret x1)) v1)
        ret))))

;; 行列の要素の最小値を計算
(define f2-array-min
  (if *eigenmat-loaded*
    eigen-array-min
    (lambda (ar1)
      (let* ((v1  (slot-ref ar1 'backing-storage))
             (ret (f64vector-ref v1 0)))
        (for-each (lambda (x1) (if (< x1 ret) (set! ret x1))) v1)
        ret))))

;; 行列の要素の最大値を計算
(define f2-array-max
  (if *eigenmat-loaded*
    eigen-array-max
    (lambda (ar1)
      (let* ((v1  (slot-ref ar1 'backing-storage))
             (ret (f64vector-ref v1 0)))
        (for-each (lambda (x1) (if (> x1 ret) (set! ret x1))) v1)
        ret))))

;; 行列の要素の平均値を計算
(define f2-array-mean
  (if *eigenmat-loaded*
    eigen-array-mean
    (lambda (ar1)
      (let ((v1  (slot-ref ar1 'backing-storage))
            (ret 0))
        (for-each (lambda (x1) (inc! ret x1)) v1)
        (/. ret (f64vector-length v1))))))

;; 行列のトレースを計算
(define f2-array-trace
  (if *eigenmat-loaded*
    eigen-array-trace
    (lambda (ar1)
      (check-array-type ar1)
      (check-array-rank ar1)
      (let ((n1  (array-length ar1 0))
            (m1  (array-length ar1 1))
            (v1  (slot-ref ar1 'backing-storage))
            (ret 0))
        (let loop ((i1 0))
          (inc! ret (f64vector-ref v1 (+ (* i1 m1) i1)))
          (if (and (< i1 (- n1 1)) (< i1 (- m1 1)))
            (loop (+ i1 1))
            ret))))))

;; 行列式を計算
(define f2-array-determinant
  (if *eigenmat-loaded*
    eigen-array-determinant
    determinant))

;; 転置行列を計算
(define f2-array-transpose
  (if *eigenmat-loaded*
    eigen-array-transpose
    %array-transpose))

;; 転置行列を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-transpose!
  (if *eigenmat-loaded*
    eigen-array-transpose!
    (lambda (ar2 ar1)
      (f2-array-copy! ar2 (%array-transpose ar1))
      ar2)))

;; 逆行列を計算
(define f2-array-inverse
  (if *eigenmat-loaded*
    eigen-array-inverse
    (lambda (ar1)
      (if-let1 ar2 (array-inverse ar1)
        ;; Gauche v0.9.6 以前で <array> が返る件の対策
        (if (eq? (class-of ar2) <f64array>)
          ar2
          (f2-array-copy ar2))
        #f))))

;; 逆行列を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-inverse!
  (if *eigenmat-loaded*
    eigen-array-inverse!
    (lambda (ar2 ar1)
      (if-let1 ar3 (array-inverse ar1)
        (begin
          ;; Gauche v0.9.6 以前で <array> が返る件の対策
          (f2-array-copy! ar2 (if (eq? (class-of ar3) <f64array>)
                                ar3
                                (f2-array-copy ar3)))
          ar2)
        #f))))

;; AX=B となる X を求める
(define f2-array-solve
  (if *eigenmat-loaded*
    eigen-array-solve
    (lambda (ar1 ar2)
      (let1 ar3 (array-div-left ar2 ar1)
        ;; Gauche v0.9.6 以前で <array> が返る件の対策
        (if (eq? (class-of ar3) <f64array>)
          ar3
          (f2-array-copy ar3))))))

;; AX=B となる X を求める(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-solve!
  (if *eigenmat-loaded*
    eigen-array-solve!
    (lambda (ar3 ar1 ar2)
      (let1 ar4 (array-div-left ar2 ar1)
        ;; Gauche v0.9.6 以前で <array> が返る件の対策
        (f2-array-copy! ar3 (if (eq? (class-of ar4) <f64array>)
                              ar4
                              (f2-array-copy ar4)))
        ar3))))

;; 行列から行を抜き出す(2次元のみ)
(define f2-array-row
  (if *eigenmat-loaded*
    eigen-array-row
    (lambda (ar1 i1r)
      (check-array-type ar1)
      (check-array-rank ar1)
      (let ((n1 (array-length ar1 0))
            (m1 (array-length ar1 1))
            (i1 (- i1r (array-start ar1 0)))
            (v1 (slot-ref ar1 'backing-storage)))
        (unless (and (>= i1 0) (< i1 n1))
          (error "invalid index value"))
        (let* ((start (* i1 m1))
               (ar2   (make-f2-array 0 1 0 m1)) ; 結果は 1 x m1 になる
               (v2    (slot-ref ar2 'backing-storage)))
          (f64vector-copy! v2 0 v1 start (+ start m1))
          ar2)))))

;; 行列から行を抜き出す(破壊的変更版)(2次元のみ)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-row!
  (if *eigenmat-loaded*
    eigen-array-row!
    (lambda (ar2 ar1 i1r)
      (check-array-type ar1 ar2)
      (check-array-rank ar1 ar2)
      (let ((n1 (array-length ar1 0))
            (m1 (array-length ar1 1))
            (i1 (- i1r (array-start ar1 0)))
            (v1 (slot-ref ar1 'backing-storage))
            (n2 (array-length ar2 0))
            (m2 (array-length ar2 1))
            (v2 (slot-ref ar2 'backing-storage)))
        (unless (and (>= i1 0) (< i1 n1))
          (error "invalid index value"))
        (unless (and (= n2 1) (= m2 m1))        ; 結果は 1 x m1 になる
          (error "array shape mismatch"))
        (let1 start (* i1 m1)
          (f64vector-copy! v2 0 v1 start (+ start m1))
          ar2)))))

;; 行列から列を抜き出す(2次元のみ)
(define f2-array-col
  (if *eigenmat-loaded*
    eigen-array-col
    (lambda (ar1 j1r)
      (check-array-type ar1)
      (check-array-rank ar1)
      (let ((n1 (array-length ar1 0))
            (m1 (array-length ar1 1))
            (j1 (- j1r (array-start ar1 1)))
            (v1 (slot-ref ar1 'backing-storage)))
        (unless (and (>= j1 0) (< j1 m1))
          (error "invalid index value"))
        (let* ((ar2 (make-f2-array 0 n1 0 1))   ; 結果は n1 x 1 になる
               (v2  (slot-ref ar2 'backing-storage)))
          (dotimes (i2 n1)
            (f64vector-set! v2 i2 (f64vector-ref v1 (+ j1 (* i2 m1)))))
          ar2)))))

;; 行列から列を抜き出す(破壊的変更版)(2次元のみ)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-col!
  (if *eigenmat-loaded*
    eigen-array-col!
    (lambda (ar2 ar1 j1r)
      (check-array-type ar1 ar2)
      (check-array-rank ar1 ar2)
      (let ((n1 (array-length ar1 0))
            (m1 (array-length ar1 1))
            (j1 (- j1r (array-start ar1 1)))
            (v1 (slot-ref ar1 'backing-storage))
            (n2 (array-length ar2 0))
            (m2 (array-length ar2 1))
            (v2 (slot-ref ar2 'backing-storage)))
        (unless (and (>= j1 0) (< j1 m1))
          (error "invalid index value"))
        (unless (and (= n2 n1) (= m2 1))        ; 結果は n1 x 1 になる
          (error "array shape mismatch"))
        (dotimes (i2 n1)
          (f64vector-set! v2 i2 (f64vector-ref v1 (+ j1 (* i2 m1)))))
        ar2))))

;; 行列から一部を抜き出す(2次元のみ)
(define f2-array-block
  (if *eigenmat-loaded*
    eigen-array-block
    (lambda (ar1 i1r j1r n2 m2)
      (check-array-type ar1)
      (check-array-rank ar1)
      (let ((n1 (array-length ar1 0))
            (m1 (array-length ar1 1))
            (i1 (- i1r (array-start ar1 0)))
            (j1 (- j1r (array-start ar1 1)))
            (v1 (slot-ref ar1 'backing-storage)))
        (unless (and (>= n2 0) (>= m2 0))
          (error "invalid block size"))
        (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n2) n1) (<= (+ j1 m2) m1))
          (error "invalid block range"))
        (let* ((ar2 (make-f2-array 0 n2 0 m2))  ; 結果は n2 x m2 になる
               (v2  (slot-ref ar2 'backing-storage)))
          (dotimes (i2 n2)
            (let1 start (+ (* (+ i1 i2) m1) j1)
              (f64vector-copy! v2 (* i2 m2) v1 start (+ start m2))))
          ar2)))))

;; 行列から一部を抜き出す(破壊的変更版)(2次元のみ)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-block!
  (if *eigenmat-loaded*
    eigen-array-block!
    (lambda (ar2 ar1 i1r j1r n2 m2)
      (check-array-type ar1 ar2)
      (check-array-rank ar1 ar2)
      (let ((n1  (array-length ar1 0))
            (m1  (array-length ar1 1))
            (i1  (- i1r (array-start ar1 0)))
            (j1  (- j1r (array-start ar1 1)))
            (v1  (slot-ref ar1 'backing-storage))
            (n2a (array-length ar2 0))
            (m2a (array-length ar2 1))
            (v2  (slot-ref ar2 'backing-storage)))
        (unless (and (>= n2 0) (>= m2 0))
          (error "invalid block size"))
        (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n2) n1) (<= (+ j1 m2) m1))
          (error "invalid block range"))
        (unless (and (= n2a n2) (= m2a m2))     ; 結果は n2 x m2 になる
          (error "array shape mismatch"))
        (dotimes (i2 n2)
          (let1 start (+ (* (+ i1 i2) m1) j1)
            (f64vector-copy! v2 (* i2 m2) v1 start (+ start m2))))
        ar2))))

;; 行列から一部を抜き出してコピー(2次元のみ)
(define f2-array-block-copy
  (if *eigenmat-loaded*
    eigen-array-block-copy
    (lambda (ar1 i1r j1r n3 m3 ar2 i2r j2r)
      (check-array-type ar1 ar2)
      (check-array-rank ar1 ar2)
      (let ((n1 (array-length ar1 0))
            (m1 (array-length ar1 1))
            (i1 (- i1r (array-start ar1 0)))
            (j1 (- j1r (array-start ar1 1)))
            (v1 (slot-ref ar1 'backing-storage))
            (n2 (array-length ar2 0))
            (m2 (array-length ar2 1))
            (i2 (- i2r (array-start ar2 0)))
            (j2 (- j2r (array-start ar2 1)))
            (v2 (slot-ref ar2 'backing-storage)))
        (unless (and (>= n3 0) (>= m3 0))
          (error "invalid block size"))
        (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n3) n1) (<= (+ j1 m3) m1))
          (error "invalid block range for copy-from"))
        (unless (and (>= i2 0) (>= j2 0) (<= (+ i2 n3) n2) (<= (+ j2 m3) m2))
          (error "invalid block range for copy-to"))
        (let* ((ar3 (f2-array-copy ar2))        ; 結果は n2 x m2 になる
               (v3  (slot-ref ar3 'backing-storage)))
          (dotimes (i3 n3)
            (let ((start1 (+ (* (+ i1 i3) m1) j1))
                  (start3 (+ (* (+ i2 i3) m2) j2)))
              (f64vector-copy! v3 start3 v1 start1 (+ start1 m3))))
          ar3)))))

;; 行列から一部を抜き出してコピー(破壊的変更版)(2次元のみ)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-block-copy!
  (if *eigenmat-loaded*
    eigen-array-block-copy!
    (lambda (ar3 ar1 i1r j1r n3 m3 ar2 i2r j2r)
      (check-array-type ar1 ar2 ar3)
      (check-array-rank ar1 ar2 ar3)
      (let ((n1  (array-length ar1 0))
            (m1  (array-length ar1 1))
            (i1  (- i1r (array-start ar1 0)))
            (j1  (- j1r (array-start ar1 1)))
            (v1  (slot-ref ar1 'backing-storage))
            (n2  (array-length ar2 0))
            (m2  (array-length ar2 1))
            (i2  (- i2r (array-start ar2 0)))
            (j2  (- j2r (array-start ar2 1)))
            (v2  (slot-ref ar2 'backing-storage))
            (n3a (array-length ar3 0))
            (m3a (array-length ar3 1))
            (v3  (slot-ref ar3 'backing-storage)))
        (unless (and (>= n3 0) (>= m3 0))
          (error "invalid block size"))
        (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n3) n1) (<= (+ j1 m3) m1))
          (error "invalid block range for copy-from"))
        (unless (and (>= i2 0) (>= j2 0) (<= (+ i2 n3) n2) (<= (+ j2 m3) m2))
          (error "invalid block range for copy-to"))
        (unless (and (= n3a n2) (= m3a m2))     ; 結果は n2 x m2 になる
          (error "array shape mismatch"))
        (f2-array-copy! ar3 ar2)
        (dotimes (i3 n3)
          (let ((start1 (+ (* (+ i1 i3) m1) j1))
                (start3 (+ (* (+ i2 i3) m2) j2)))
            (f64vector-copy! v3 start3 v1 start1 (+ start1 m3))))
        ar3))))


;; == 以下では、blasmat モジュールがあれば使用する ==


;; B = alpha A + B を計算
(define f2-array-ra+b!
  (if *blasmat-loaded*
    blas-array-daxpy!
    (lambda (alpha A B)
      (f2-array-add-elements! B (f2-array-mul-elements A alpha) B))))

;; C = alpha A B + beta C を計算
(define f2-array-ab+c!
  (if *blasmat-loaded*
    blas-array-dgemm!
    (lambda (A B C alpha beta trans-A trans-B)
      (let ((TA (if trans-A (f2-array-transpose A) A))
            (TB (if trans-B (f2-array-transpose B) B))
            (D  (make-f2-array-same-shape C)))
        (f2-array-add-elements!
         C
         (f2-array-mul-elements! D (f2-array-mul! D TA TB) alpha)
         (f2-array-mul-elements! C C beta))))))

