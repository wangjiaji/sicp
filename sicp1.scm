(define (square x)
  (* x x))

(define (two-square x y)
  (+ (square x) (square y)))

(define (two-sum x y z)
  (cond ((and (> x z) (> y z))
	 (two-square x y))
	((and (> x y) (> z y))
	 (two-square x z))
	(else (two-square y z))))

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else
	 (A (- x 1) (A x (- y 1))))))

(define (pascal n i)
  (cond ((= n 0) 1)
	((or (= i 0) (= i n)) 1)
	(else
	 (+ (pascal (- n 1) (- i 1))
	    (pascal (- n 1) i)))))

(define (fast-expt n product)
  (cond ((= n 0) 1)
	((even? a)
	 (fast-expt (/ n 2) (square product)))
	(else
	 (* product (fast-expt (/ (- n 1) 2) (square product))))))

(define (half x)
  (/ x 2))

(define (multiply x y)
  (define (mul n)
    (cond ((= n 0) 0)
	  ((even? n)
	   (double (mul (half n))))
	  (else
	   (+ x (double (mul (half (- n 1))))))))
  (define (mul-iter n accu)
    (cond ((= n 0) accu)
	  ((even? n)
	   (mul-iter (half n) (double accu)))
	  (else
	   (mul-iter (half (- n 1)) (+ x (double accu))))))
  (mul y))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (square q) (* 2 p q))
		   (/ count 2)))
	(else
	 (fib-iter (+ (* b q) (* a q) ( * a p))
		   (+ (* b p) (* a q))
		   p
		   q
		   (- count 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? n test-divisor) test-divisor)
	(else
	 (find-divisor n (+ 1 test-divisor)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (half exp) m)) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))

(define (mr-expmod base exp m)
  (define (trivial a)
    (cond ((= a 1) 1)
	  ((= a (- m 1)) 1)
	  ((= (remainder (square a) m) 1) 0)
	  (else (remainder a m)))
    (cond ((= exp 0) 1)
	  ((even? exp)
	   (trivial (square (expmod base (half exp) m))) m)
	  (else
	   (remainder (* base (expmod base (- exp 1) m)) m)))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fermat-test (- n 1)))
	(else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes min max start-time)
  (cond ((> max min)
	 (timed-prime-test min)
	 (search-for-primes (+ min 1) max start-time))
	(else
	 (newline)
	 (display "Test finished: ")
	 (display (- (runtime) start-time)))))

(define (timed-search-for-primes min max)
  (search-for-primes min max (runtime)))

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (simpson-integral func a b dx)
  (define (add-dx x)
    (+ x dx))
  (define (term x)
    (cond ((= x a) (func x))
	  ((= x b) (func x))
	  ((even? (floor (/ (- x a) dx))) (* 4 (func x)))
	  (else (* 2 (func x)))))
  (/ (* (sum term a add-dx b) dx) 3))

(define (sum-iter term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x) (+ result (term x)))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x) (* result (term x)))))
  (iter a 1))

(define (identity x)
  x)

(define (inc x)
  (+ 1 x))

(define (factorial n)
  (product-iter identity 1 inc n))

(define (pi precision)
  (define (term x)
    (if (even? x)
	(/ x (+ 1 x))
	(/ (+ 1 x) x)))
  (* 4.0 (product-iter term 2 inc precision)))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.00001))

(define (average x y)
  (/ (+ x y) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point (lambda (x) (average (+ x 1) (/ 1 x))) 1.0))

(define (cont-frac n d k i)
  (if (= k i)
      0
      (/ (n i) (+ (d i) (cont-frac n d k (inc i))))))

(define (cont-frac-iter n d k accu)
  (if (= k 0)
      accu
      (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) accu)))))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1)
				  x
				  (- 0 (square x))))
		  (lambda (i) (- (* 2 i) 1))
		  k
		  (- (* 2 (inc k)) 1.0)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (repeated fn n)
  (if (= n 1)
      fn
      (lambda (x)
	(fn ((repeated fn (- n 1)) x)))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
	guess
	(iter (improve guess))))
  iter)

(define (sqrt-iter x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.00001))
    (lambda (guess)
      (average guess (/ x guess)))) x))