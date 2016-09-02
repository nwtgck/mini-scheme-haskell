10
(display "hello, world\n")
(define x 10)
(define y 20)
(display x)
(display "\n")
(display (+ x y))
(define x 1)
(display "\n")
(display x)
(display "\n")

(define id (lambda (x) (* x 2)))

(display (id 55))
(display "\n")

; print: 改行つきdisplay
(define print (lambda (x) (display x) (display "\n")))

(print 10)
(print x)
(print (+ 1 2 3 4 5))
(print (car '(1 2 3)))

(define add (lambda (x y) (+ x y)))
(print (add 3 4))
(define fib (lambda (n)
  (if (equal? n 0)
    1
    (if (equal? n 1)
      1
      (+ (fib (- n 2)) (fib (- n 1)))
    )
  )
))
(print (fib 20) )
(print (fib 21) )
(print (fib 22) )
(print (fib 23) )
(print (fib 24) )
(print (cons 1 (cons 2 3)))
(print #t)
