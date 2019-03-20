;; -*- coding: utf-8 -*-
;;
;; f2arrmat.scm
;; 2019-3-20 v1.00
;;
;; ＜内容＞
;;   Gauche で、2次元の f64array を扱うためのモジュールです。
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
    f2-array-sigmoid      f2-array-sigmoid!
    f2-array-relu         f2-array-relu!
    f2-array-step         f2-array-step!
    f2-array-transpose    f2-array-transpose!
    f2-array-row          f2-array-row!
    f2-array-col          f2-array-col!
    f2-array-ra+b         f2-array-ra+b!
    f2-array-ab+c         f2-array-ab+c!
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
;(define *disable-blasmat* #t) ; 無効化フラグ
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

;; shape の内部処理の高速化
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
(define-inline (array-rank   A)
  (s32vector-length (slot-ref A 'start-vector)))
(define-inline (array-start  A dim)
  (s32vector-ref    (slot-ref A 'start-vector) dim))
(define-inline (array-end    A dim)
  (s32vector-ref    (slot-ref A 'end-vector)   dim))
(define-inline (array-length A dim)
  (- (s32vector-ref (slot-ref A 'end-vector)   dim)
     (s32vector-ref (slot-ref A 'start-vector) dim)))

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

;; 行列の要素の参照(2次元のみ)
(define (f2-array-ref A i j)
  (let ((is (s32vector-ref (slot-ref A 'start-vector) 0))
        (ie (s32vector-ref (slot-ref A 'end-vector)   0))
        (js (s32vector-ref (slot-ref A 'start-vector) 1))
        (je (s32vector-ref (slot-ref A 'end-vector)   1)))
    (unless (and (<= is i) (< i ie) (<= js j) (< j je))
      (error "invalid index value"))
    (f64vector-ref (slot-ref A 'backing-storage)
                   (+ (* (- i is) (- je js)) (- j js)))))

;; 行列の要素の設定(2次元のみ)
;; (戻り値は未定義)
(define (f2-array-set! A i j d)
  (let ((is (s32vector-ref (slot-ref A 'start-vector) 0))
        (ie (s32vector-ref (slot-ref A 'end-vector)   0))
        (js (s32vector-ref (slot-ref A 'start-vector) 1))
        (je (s32vector-ref (slot-ref A 'end-vector)   1)))
    (unless (and (<= is i) (< i ie) (<= js j) (< j je))
      (error "invalid index value"))
    (f64vector-set! (slot-ref A 'backing-storage)
                    (+ (* (- i is) (- je js)) (- j js))
                    d)))

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

;; f64array 用の array-map (ただし shape の明示指定は不可)
(define (f2-array-map proc ar0 . rest)
  (rlet1 ar (if (eq? (class-of ar0) <f64array>)
              (array-copy ar0)
              (make-f64array (array-shape ar0)))
    (apply array-map! ar proc ar0 rest)))

;; f64array 用の array-map! (エラーチェックなし)
;; (戻り値は未定義)
(define f2-array-map! array-map!)

;; 行列の生成(2次元のみ)(キャッシュ使用)
(define (make-f2-array ns ne ms me . maybe-init)
  (if use-f2-array-cache
    (let1 key (s32vector ns ne ms me)
      (if-let1 A (hash-table-get f2-array-cache-table key #f)
        (if (or (null? maybe-init) (= (car maybe-init) 0))
          (array-copy A)
          (rlet1 B (array-copy A)
            (f64vector-fill! (slot-ref B 'backing-storage) (car maybe-init))))
        (let1 B ((with-module gauche.array %make-array-internal-sub)
                 <f64array> (shape ns ne ms me) 0)
          (hash-table-put! f2-array-cache-table key B)
          (if (or (null? maybe-init) (= (car maybe-init) 0))
            (array-copy B)
            (rlet1 C (array-copy B)
              (f64vector-fill! (slot-ref C 'backing-storage) (car maybe-init)))))))
    (apply (with-module gauche.array %make-array-internal-sub)
           <f64array> (shape ns ne ms me) maybe-init)))

;; 同じ shape の行列の生成(2次元のみ)
(define (make-f2-array-same-shape A . maybe-init)
  (check-array-rank A)
  (let ((ns (array-start A 0))
        (ne (array-end   A 0))
        (ms (array-start A 1))
        (me (array-end   A 1)))
    (apply make-f2-array ns ne ms me maybe-init)))

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

;; 行列の生成(内部処理用)(キャッシュ使用)
(select-module gauche.array)
(define (%make-array-internal-sub class shape . maybe-init)
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
           (equal? (slot-ref shape 'end-vector) #s32(2 2))) ; rank 2
    (receive (Vb Ve) (shape->start/end-vector shape)
      (let ((ns (s32vector-ref Vb 0))
            (ne (s32vector-ref Ve 0))
            (ms (s32vector-ref Vb 1))
            (me (s32vector-ref Ve 1)))
        (apply (with-module f2arrmat make-f2-array)
               ns ne ms me maybe-init)))
    (apply %make-array-internal-sub class shape maybe-init)))
(select-module f2arrmat)

;; 転置行列の生成(Gauche v0.9.7 の不具合対応(resの生成) + 高速化)
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
;; (ただし f2-array-mul! は、blasmat モジュールがあれば優先的に使用する)


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
      (let ((v1    (slot-ref ar1 'backing-storage))
            (v2    (slot-ref ar2 'backing-storage))
            (norm1 0) (norm2 0) (norm3 0))
        (for-each
         (lambda (d1 d2)
           (inc! norm1 (* d1 d1))
           (inc! norm2 (* d2 d2))
           (inc! norm3 (* (- d1 d2) (- d1 d2))))
         v1 v2)
        (<= (%sqrt norm3) (* precision (min (%sqrt norm1) (%sqrt norm2))))))))

;; 行列のゼロチェック
(define f2-array-nearly-zero?
  (if *eigenmat-loaded*
    eigen-array-nearly-zero?
    (lambda (ar1 :optional (precision 1e-12))
      (let ((v1    (slot-ref ar1 'backing-storage))
            (norm1 0))
        (for-each (lambda (d1) (inc! norm1 (* d1 d1))) v1)
        (<= (%sqrt norm1) precision)))))

;; 行列の和を計算
(define f2-array-add-elements
  (if *eigenmat-loaded*
    (lambda (ar . rest) (fold-left eigen-array-add ar rest))
    array-add-elements))

;; 行列の和を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-add-elements!
  (if *eigenmat-loaded*
    (lambda (ar ar0 ar1 . rest)
      (eigen-array-add! ar ar0 ar1)
      (for-each (lambda (arX) (eigen-array-add! ar ar arX)) rest)
      ar)
    (lambda (ar . rest)
      (f2-array-copy! ar (apply array-add-elements rest))
      ar)))

;; 行列の差を計算
(define f2-array-sub-elements
  (if *eigenmat-loaded*
    (lambda (ar . rest) (fold-left eigen-array-sub ar rest))
    array-sub-elements))

;; 行列の差を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-sub-elements!
  (if *eigenmat-loaded*
    (lambda (ar ar0 ar1 . rest)
      (eigen-array-sub! ar ar0 ar1)
      (for-each (lambda (arX) (eigen-array-sub! ar ar arX)) rest)
      ar)
    (lambda (ar . rest)
      (f2-array-copy! ar (apply array-sub-elements rest))
      ar)))

;; 行列の積を計算(2次元のみ)
(define f2-array-mul
  (if *eigenmat-loaded*
    eigen-array-mul
    array-mul))

;; 行列の積を計算(破壊的変更版)(2次元のみ)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-mul!
  (cond
   (*blasmat-loaded*
    (lambda (ar ar0 ar1)
      (f64vector-fill! (slot-ref ar 'backing-storage) 0)
      (blas-array-dgemm ar0 ar1 ar 1.0 1.0 #f #f)))
   (*eigenmat-loaded*
    eigen-array-mul!)
   (else
    (lambda (ar ar0 ar1)
      (f2-array-copy! ar (array-mul ar0 ar1))
      ar))))

;; 行列の要素の積を計算
(define f2-array-mul-elements
  (if *eigenmat-loaded*
    (lambda (ar . rest) (fold-left eigen-array-mul-elements ar rest))
    array-mul-elements))

;; 行列の要素の積を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-mul-elements!
  (if *eigenmat-loaded*
    (lambda (ar ar0 ar1 . rest)
      (eigen-array-mul-elements! ar ar0 ar1)
      (for-each (lambda (arX) (eigen-array-mul-elements! ar ar arX)) rest)
      ar)
    (lambda (ar . rest)
      (f2-array-copy! ar (apply array-mul-elements rest))
      ar)))

;; 行列の要素に対して、シグモイド関数を計算
(define f2-array-sigmoid
  (if *eigenmat-loaded*
    eigen-array-sigmoid
    (lambda (ar)
      (f2-array-map
       (lambda (x1) (/. 1 (+ 1 (%exp (- x1))))) ; シグモイド関数
       ar))))

;; 行列の要素に対して、シグモイド関数を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-sigmoid!
  (if *eigenmat-loaded*
    eigen-array-sigmoid!
    (lambda (ar2 ar1)
      (f2-array-map!
       ar2
       (lambda (x1) (/. 1 (+ 1 (%exp (- x1))))) ; シグモイド関数
       ar1)
      ar2)))

;; 行列の要素に対して、ReLU関数を計算
(define f2-array-relu
  (if *eigenmat-loaded*
    eigen-array-relu
    (lambda (ar)
      (f2-array-map
       (lambda (x1) (max 0 x1)) ; ReLU関数
       ar))))

;; 行列の要素に対して、ReLU関数を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-relu!
  (if *eigenmat-loaded*
    eigen-array-relu!
    (lambda (ar2 ar1)
      (f2-array-map!
       ar2
       (lambda (x1) (max 0 x1)) ; ReLU関数
       ar1)
      ar2)))

;; 行列の要素に対して、ステップ関数を計算
(define f2-array-step
  (if *eigenmat-loaded*
    eigen-array-step
    (lambda (ar)
      (f2-array-map
       (lambda (x1) (if (> x1 0) 1 0)) ; ステップ関数
       ar))))

;; 行列の要素に対して、ステップ関数を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-step!
  (if *eigenmat-loaded*
    eigen-array-step!
    (lambda (ar2 ar1)
      (f2-array-map!
       ar2
       (lambda (x1) (if (> x1 0) 1 0)) ; ステップ関数
       ar1)
      ar2)))

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

;; 行列から行を抜き出す(2次元のみ)
(define f2-array-row
  (if *eigenmat-loaded*
    eigen-array-row
    (lambda (ar1 i1)
      (check-array-type ar1)
      (check-array-rank ar1)
      (let ((n1 (array-length ar1 0))
            (m1 (array-length ar1 1))
            (is (array-start  ar1 0))
            (ie (array-end    ar1 0))
            (js (array-start  ar1 1)))
        (unless (and (>= i1 is) (< i1 ie))
          (error "invalid index value"))
        (let* ((ar2  (make-f2-array 0 1 0 m1)) ; 結果は 1 x m1 になる
               (vec2 (slot-ref ar2 'backing-storage)))
          (dotimes (j2 m1)
            (f64vector-set! vec2 j2 (f2-array-ref ar1 i1 (+ j2 js))))
          ar2)))))

;; 行列から行を抜き出す(破壊的変更版)(2次元のみ)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-row!
  (if *eigenmat-loaded*
    eigen-array-row!
    (lambda (ar2 ar1 i1)
      (check-array-type ar1 ar2)
      (check-array-rank ar1 ar2)
      (let ((n1   (array-length ar1 0))
            (m1   (array-length ar1 1))
            (is   (array-start  ar1 0))
            (ie   (array-end    ar1 0))
            (js   (array-start  ar1 1))
            (n2   (array-length ar2 0))
            (m2   (array-length ar2 1))
            (vec2 (slot-ref ar2 'backing-storage)))
        (unless (and (>= i1 is) (< i1 ie))
          (error "invalid index value"))
        (unless (and (= n2 1) (= m2 m1))       ; 結果は 1 x m1 になる
          (error "array shape mismatch"))
        (dotimes (j2 m1)
          (f64vector-set! vec2 j2 (f2-array-ref ar1 i1 (+ j2 js))))
        ar2))))

;; 行列から列を抜き出す(2次元のみ)
(define f2-array-col
  (if *eigenmat-loaded*
    eigen-array-col
    (lambda (ar1 j1)
      (check-array-type ar1)
      (check-array-rank ar1)
      (let ((n1 (array-length ar1 0))
            (m1 (array-length ar1 1))
            (is (array-start  ar1 0))
            (js (array-start  ar1 1))
            (je (array-end    ar1 1)))
        (unless (and (>= j1 js) (< j1 je))
          (error "invalid index value"))
        (let* ((ar2  (make-f2-array 0 n1 0 1)) ; 結果は n1 x 1 になる
               (vec2 (slot-ref ar2 'backing-storage)))
          (dotimes (i2 n1)
            (f64vector-set! vec2 i2 (f2-array-ref ar1 (+ i2 is) j1)))
          ar2)))))

;; 行列から列を抜き出す(破壊的変更版)(2次元のみ)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-col!
  (if *eigenmat-loaded*
    eigen-array-col!
    (lambda (ar2 ar1 j1)
      (check-array-type ar1 ar2)
      (check-array-rank ar1 ar2)
      (let ((n1   (array-length ar1 0))
            (m1   (array-length ar1 1))
            (is   (array-start  ar1 0))
            (js   (array-start  ar1 1))
            (je   (array-end    ar1 1))
            (n2   (array-length ar2 0))
            (m2   (array-length ar2 1))
            (vec2 (slot-ref ar2 'backing-storage)))
        (unless (and (>= j1 js) (< j1 je))
          (error "invalid index value"))
        (unless (and (= n2 n1) (= m2 1))       ; 結果は n1 x 1 になる
          (error "array shape mismatch"))
        (dotimes (i2 n1)
          (f64vector-set! vec2 i2 (f2-array-ref ar1 (+ i2 is) j1)))
        ar2))))


;; == 以下では、blasmat モジュールがあれば使用する ==


;; rA+B を計算
(define f2-array-ra+b
  (if *blasmat-loaded*
    (lambda (r A B)
      (let1 C (f2-array-copy B)
        (blas-array-daxpy A C r)))
    (lambda (r A B)
      (let1 C (make-f2-array-same-shape B)
        (f2-array-add-elements! C (f2-array-mul-elements! C A r) B)))))

;; rA+B を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-ra+b!
  (if *blasmat-loaded*
    (lambda (C r A B)
      (f2-array-copy! C B)
      (blas-array-daxpy A C r))
    (lambda (C r A B)
      ;; C と B が同じ行列のときは、D を生成しないと壊れる
      (let1 D (if (eq? C B) (make-f2-array-same-shape C) C)
        (f2-array-add-elements! C (f2-array-mul-elements! D A r) B)))))

;; AB+C を計算
(define f2-array-ab+c
  (if *blasmat-loaded*
    (lambda (A B C)
      (let1 D (f2-array-copy C)
        (blas-array-dgemm A B D 1.0 1.0 #f #f)))
    (lambda (A B C)
      (let1 D (make-f2-array-same-shape C)
        (f2-array-add-elements! D (f2-array-mul! D A B) C)))))

;; AB+C を計算(破壊的変更版)
;; (第1引数は結果を格納するためだけに使用)
(define f2-array-ab+c!
  (if *blasmat-loaded*
    (lambda (D A B C)
      (f2-array-copy! D C)
      (blas-array-dgemm A B D 1.0 1.0 #f #f))
    (lambda (D A B C)
      ;; D と C が同じ行列のときは、E を生成しないと壊れる
      (let1 E (if (eq? D C) (make-f2-array-same-shape D) D)
        (f2-array-add-elements! D (f2-array-mul! E A B) C)))))

