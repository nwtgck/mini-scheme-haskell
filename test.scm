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

; ln: 改行つきdisplay
(define ln (lambda (x) (display x) (display "\n")))

(ln 10)
(ln x)
(ln (+ 1 2 3 4 5))
(ln (car '(1 2 3)))

(define add (lambda (x y) (+ x y)))
(ln (add 3 4))
(define fib (lambda (n)
  (if (equal? n 0)
    1
    (if (equal? n 1)
      1
      (+ (fib (- n 2)) (fib (- n 1)))
    )
  )
))
(ln (fib 20) )
