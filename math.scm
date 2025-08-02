(define (factorial x)
  (cond ((not (number? x)) #f)
        ((> x 100) #f)
        ((= x 0) 1)
        (else (* x (factorial (- x 1))))))

(define (new-factorial n)
    (define (factorial-iter product counter max-count)
      (if (> counter max-count)
          product
          (factorial-iter (* counter product)
                     (+ counter 1) max-count)))
  (if (not (number? n))
      #f
      (factorial-iter 1 1 n)))

(define (sum x)
  (cond ((not number?) #f)
        ((= x 0) x)
        (else (+ x (sum (- x 1))))))

(define (square x)
  (if (not (number? x))
      #f
      (* x x)))

(define (average LstStream)
  (if (list? LstStream)
      (/ (apply + LstStream) (length LstStream))
      #f))

(define (absolute x)
  (cond ((not (number? x) #f))
        ((>= x 0) x)
        ((< x 0) (- x))))

(define (new-sqrt x)
  (define (good-enough? guess x)
    (< (absolute (- (square guess) x)) 0.0000001))
  (define (improve guess x)
    (/ (+ guess (/ x guess)) 2))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

#|
(let
    ((a (- (random 5))) (b (- (random 5))) (c (- (random 5)))
     (absolute (lambda (x)
                 (cond
                   ((not (number? x)))
                   ((> x 0) x)
                   ((< x 0) (- x))
                   ((= x 0) 0)))))
  (begin
    (format "~a ~a ~a~%"a b c)
    (cond ((> (+ a c) (- b c)) (display "a"))
      ((> (absolute a) (absolute b)) (display "b"))
      ((> (+ a b) (* 2 b)) (display "c"))
      ((> (/ 1 a) (/ 1 b))) (display "d"))))
|#
