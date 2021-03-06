; minischeme test.scm

(define false #f)
(define true #t)


(define (lettest x)
  (let ((a 'a)
        (b '(b c d))
        (c x)
        (d '(f g h)))
    (cons a (cons b (cons c d)))))

(define (q x y z)
  (cond ((= x y) (+ x y))
        ((= y z) (* x y))
        (else (* x y z))))

(define (mem-helper pred op)
  (lambda (acc next)
    (if (and (not acc) (pred (op next))) next acc)))
(define (curry func arg1)
  (lambda (arg) (func arg1 arg)))


(define (fold func accum lst)
  (if (null? lst)
      accum
      (fold func (func accum (car lst)) (cdr lst))))


(define (assq obj lst)  (fold (mem-helper (curry eq? obj) car) #f lst))
(define (assv obj lst)  (fold (mem-helper (curry eqv? obj) car) #f lst))
(define (assoc obj lst) (fold (mem-helper (curry equal? obj) car) #f lst))



(define (cadr x) (car (cdr x)))

(define (s x)
  (cond ((assv x '((a 1) (b 2))) => cadr)
        (else x)))

(define and3 (lambda (w x y)
  (let ((disp (lambda (a) (display a) a)))
    (and (disp w) (disp x) (disp y)))))

(define or3 (lambda (w x y)
  (let ((disp (lambda (a) (display a) a)))
    (or (disp w) (disp x) (disp y)))))

(define iter (lambda (x y) (+ x y)))

(define letfib
  (lambda (n)
    (let iter ((a 1) (b 0) (count n))
      (if (= count 0)
          b
          (iter (+ a b) a (- count 1))))))

(define (abc)
  (let f ((x '(x y z)) (y '(a b c d e)))
    'f 'g 'h
    (if (null? x) y (f (cdr x) (cdr y)))))




(define (mapp proc l)
  (if (null? l)
      '()
      (cons (proc (car l)) (mapp proc (cdr l)))))

(define (square x)
  (* x x))

(define sss (map letfib '(3 4 5 6 7 8 9 10 11 12 20)))

(define tw
  (lambda (x)
    (let ((twice (lambda (f x) (f (f x))))
          (sq    (lambda (x) (* x x))))
      (twice sq x))))



(define fac 
  (lambda (n)
    ((lambda (fact)
       (fact fact n))
     (lambda (ft k)
       (if (= k 1)
           1
           (* k (ft ft (- k 1))))))))

(define (even? x)
  (letrec
      ((zero?  (lambda (n) (= n 0)))
       (even?? (lambda (n)
                 (if (zero? n)
                     #t
                     (odd?? (- n 1)))))
       (odd??  (lambda (n)
                 (if (zero? n)
                     #f
                     (even?? (- n 1))))))
    'a 'b 'c 'd
    (even?? x)))

(define (con x)
  (cond ((= x 1) 'a 'b 'c)
        (else 'x 'y)))

(define (a2 x)
  (if (> x 2)
      (begin 'a 'b 'c 'd x)
      (+ x 1)))
