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


(define (assq obj alist)  (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)  (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))


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
    (let iter ((a 1) (b 0) (c n))
      (if (= c 0)
          b
          (iter (+ a b) a (- c 1))))))

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

(define (evv x)
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
    (even?? x)))
