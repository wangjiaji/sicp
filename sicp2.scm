(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
	(cons (/ (- 0 n) g) (/ (- 0 d) g))
	(cons (/ n g) (/ d g)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (let ((start-point (start-segment seg))
	(end-point (end-segment seg)))
    (make-point (average (x-point start-point) (x-point end-point))
		(average (y-point start-point) (y-point end-point)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

;; Interval arithmatic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (lower-bound y) (upper-bound y))
      (error "Cannot divide zero!")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y))))))

(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (average (lower-bound i) (upper-bound i)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent i)
  (/ (width i) (center i)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1)
				    (div-interval one r2)))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items (- n 1)))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (last-pair items)
  (define (last-pair-iter items last)
    (if (null? items)
	last
	(last-pair-iter (cdr items) (car items))))
  (last-pair-iter (cdr items) (car items)))

(define (my-append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))

(define (my-reverse alist)
  (if (null? alist)
      alist
      (my-append (my-reverse (cdr alist)) (list (car alist)))))

(define (same-parity first . rest)
  (define (iter items accu)
    (if (null? items)
	accu
	(let ((item (car items)))
	  (if (or (and (odd? first) (odd? item))
		  (and (even? first) (even? item)))
	      (iter (cdr items) (cons item accu))
	      (iter (cdr items) accu)))))
  (cons first (reverse (iter rest ()))))

(define (deep-reverse items)
  (if (null? items)
      items
      (let ((head (car items)))
	(if (pair? head)
	    (append (deep-reverse (cdr items)) (list (deep-reverse head)))
	    (append (deep-reverse (cdr items)) (list head))))))

(define (fringe alist)
  (if (null? alist)
      alist
      (let ((head (car alist)))
	(if (pair? head)
	    (append (fringe head) (fringe (cdr alist)))
	    (cons head (fringe (cdr alist)))))))

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (left branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-branch-weight branch)
  (if (null? branch)
      0
      (let ((struc (branch-structure branch)))
	(if (pair? struc)
	    (total-weight struc)
	    struc))))

(define (total-weight mobile)
  (+ (total-branch-weight (left-branch mobile))
     (total-branch-weight (right-branch mobile))))

(define (balanced-mobile? mobile)
  (define (torque branch)
    (* (branch-length branch) (total-branch-weight branch)))
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (= (torque left)
	    (torque right))
	 (if (pair? (branch-structure left))
	     (balanced-mobile? left)
	     #t)
	 (if (pair? (branch-structure right))
	     (balanced-mobile? right)
	     #t))))

(define (square-tree tree)
  (if (null? tree)
      tree
      (if (pair? tree)
	  (cons (square-tree (car tree))
		(square-tree (cdr tree)))
	  (square tree))))

(define (map-square-tree tree)
  (map (lambda (tr)
	 (if (pair? tr)
	     (map-square-tree tr)
	     (square tr)))
       tree))

(define (tree-map proc tree)
  (map (lambda (tr)
	 (if (pair? tr)
	     (tree-map proc tr)
	     (proc tr)))
       tree))

(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x)
			    (cons (car s) x))
			  rest)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x)
			 (if (pair? x)
			     (count-leaves x)
			     1))
		       tree)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
	 (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (map (lambda (col)
		  (dot-product row col))
		cols))
	 m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))


(define fold-right accumulate)

(define (reverse-right seq)
  (fold-right (lambda (x y)
		(append y (list x)))
	      () seq))

(define (reverse-left seq)
  (fold-left (lambda (x y)
	       (append (list y) x))
	     () seq))

(define (enumerate-interval s n)
  (if (> s n)
      ()
      (cons s (enumerate-interval (inc s) n))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (filter pred seq)
  (cond ((null? seq) seq)
	((pred (car seq))
	 (cons (car seq)
	       (filter pred (cdr seq))))
	(else (filter pred (cdr seq)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(define (permutations s)
  (if (null? s)
      (list s)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j)
		    (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k)
			       (list i j k))
			     (enumerate-interval 1 (- j 1))))
		      (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

;; n-queens problem

(define (queens board-size)
  (define (make-pos row col)
    (list row col))
  (define (pos-row pos)
    (car pos))
  (define (pos-col pos)
    (car (cdr pos)))

  (define (diagonal? p1 p2)
    (= (abs (- (pos-row p1) (pos-row p2)))
       (abs (- (pos-col p1) (pos-col p2)))))

  (define (safe? positions)
    (let ((new-pos (car positions))
	  (old-pos (cdr positions)))
      (if (null? old-pos)
	  #t
	  (null? (filter (lambda (p)
			   (or (= (pos-row p) (pos-row new-pos))
			       (= (pos-col p) (pos-col new-pos))
			       (diagonal? new-pos p)))
			 old-pos)))))

  (define (adjoin-pos row col prev)
    (cons (make-pos row col) prev))
  
  (define (queen-cols k)
    (if (= k 0)
	(list ())
	(filter safe?
		(flatmap (lambda (rest-of-queens)
			   (map (lambda (new-row)
				  (adjoin-pos new-row k rest-of-queens))
				(enumerate-interval 1 board-size)))
			 (queen-cols (- k 1))))))

  (queen-cols board-size))

;; Picture Language

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split op1 op2) painter (- n 1))))
	  (op1 painter (op2 smaller smaller))))))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
	     (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
	     (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (car (cdr v)))

(define (make-vect x y)
  (list x y))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
	      (add-vect (scale-vect (xcor-vect v)
				    (edge1-frame frame))
			(scale-vect (ycor-vect v)
				    (edge2-frame frame))))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))


(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
		(draw-line ((frame-coord-map frame)
			    (start-segment segment))
			   ((frame-coord-map frame)
			    (end-segment segment))))
	      segment-list)))

(define (draw-line start end) 'nil)

(define (make-segment start end)
  (list start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (car (cdr seg)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter (make-frame new-origin
			     (sub-vect (m corner1) new-origin)
			     (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let* ((split-point (make-vect 0.5 0.0))
	 (paint-left (transform-painter painter1
					(make-vect 0.0 0.0)
					split-point
					(make-vect 0.0 1.0)))
	 (paint-right (transform-painter painter2
					 split-point
					 (make-vect 1.0 0.0)
					 (make-vect 0.5 1.0))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

(define (flip-horiz painter)
  (lambda (frame)
    ((transform-painter painter
			(make-vect 0.0 1.0)
			(make-vect 1.0 1.0)
			(make-vect 0.0 0.0))
     frame)))

;; End picture language

;; Symbolic Differentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (multiplicand exp)
				 (deriv (multiplier exp) var))))
	((exponentiation? exp)
	 (let ((b (base exp))
	       (expon (exponent exp)))
	   (make-product expon
			 (make-product (make-exp b (make-sum expon -1))
				       (deriv b var)))))
	(else (error "unknown expression type -- DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (math-expr? e)
  (if (pair? e)
      (let ((op (car e)))
	(or ('eq op '+)
	    ('eq op '*)
	    ('eq op '**)))))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? x n)
  (and (number? x) (= x n)))

;; Maker

(define (make-sum a b)
  (cond ((and (number? a) (number? b)) (+ a b))
	((=number? a 0) b)
	((=number? b 0) a)
	(else (list '+ a b))))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
	((and (number? a) (number? b)) (* a b))
	((=number? a 1) b)
	((=number? b 1) a)
	(else (list '* a b))))

(define (make-exp base expon)
  (cond ((and (number? base) (number? expon)) (expt base expon))
	((or (=number? expon 0) (=number? base 1)) 1)
	((=number? base 0) 0)
	((=number? expon 1) base)
	(else (list '** base expon))))

;; Validator

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (exponentiation? x)
  (eq? (car x) '**))

;; Selector

(define (addend s)
  (second s))

(define (augend s)
  (if (> (length s) 3)
      (cons '+ (cddr s))
      (third s)))

(define (multiplier p)
  (second p))

(define (multiplicand p)
  (if (> (length p) 3)
      (cons '* (cddr p))
      (third p)))

(define (base e)
  (second e))

(define (exponent e)
  (third e))

;; Set as unordered list

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else
	 (cons (car set1) set2))))

;; Set as ordered list

(define (element-of-set-o? x set)
  (cond ((null? set) #f)
	((< x (car set)) #f)
	((= x (car set)) #t)
	(else (element-of-set-o? x (cdr set)))))

(define (intersection-set-o set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((a (car set1))
	    (b (car set2)))
	(cond ((= a b)
	       (cons a (intersection-set-o (cdr set1) (cdr set2))))
	      ((> a b)
	       (intersection-set-o set1 (cdr set2)))
	      ((< a b)
	       (intersection-set-o (cdr set1) set2))))))

(define (adjoin-set-o x set)
  (if (< x (car set))
      (cons x set)
      (cons (car set) (adjoin-set-o x (cdr set)))))

(define (union-set-o set1 set2)
  (if (or (null? set1) (null? set2))
      (if (null? set1)
	  set2
	  set1)
      (let ((a (car set1))
	    (b (car set2)))
	(cond ((= a b)
	       (cons a (union-set-o (cdr set1) (cdr set2))))
	      ((< a b)
	       (cons a (union-set-o (cdr set1) set2)))
	      (else
	       (cons b (union-set-o set1 (cdr set2))))))))

;; Set as binary tree

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (second tree))

(define (right-branch tree)
  (third tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-t? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((> x (entry set))
	 (element-of-set-t? x (right-branch set)))
	(else
	 (element-of-set-t? x (left-branch set)))))

(define (adjoin-set-t x set)
  (cond ((null? set) (make-tree x () ()))
	((< x (car set))
	 (make-tree (entry set)
		    (adjoin-set-t x (left-branch set))
		    (right-branch set)))
	((> x (car set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set-t x (right-branch set))))
	(else set)))

(define (union-set-t set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (union-set-o list1 list2))))

(define (intersection-set-t set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (intersection-set-o list1 list2))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define atree (make-tree 25
			 (make-tree 12
				    (make-tree 7
					       (make-tree 4
							  '()
							  (make-tree 6 '() '()))
					       (make-tree 9
							  (make-tree 8 '() '())
							  (make-tree 11 '() '())))
				    (make-tree 19
					       (make-tree 15 '() '())
					       (make-tree 23
							  (make-tree 20 '() '())
							  '())))
			 (make-tree 29 '() '())))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
	     (left-result (partial-tree elts left-size))
	     (left-tree (car left-result))
	     (non-left-elts (cdr left-result))
	     (right-size (- n left-size 1))
	     (this-entry (car non-left-elts))
	     (right-result (partial-tree (cdr non-left-elts) right-size))
	     (right-tree (car right-result))
	     (remaining-elts (cdr right-result)))
	(cons (make-tree this-entry left-tree right-tree) remaining-elts))))

;; Huffman Encoding Tree
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (and (pair? obj) (eq? (car obj) 'leaf)))

(define (symbol-leaf leaf)
  (second leaf))

(define (weight-leaf leaf)
  (third leaf))

(define (make-code-tree left right)
  (list (append (symbols left) (symbols right))
	left
	right
	(+ (weight left) (weight right))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (entry tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (fourth tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	bits
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0)
	 (left-branch branch))
	((= bit 1)
	 (right-branch branch))
	(else
	 (error "bad bit -- CHOOSE-BRANCH" bit))))

;; adjoin-set for Huffman tree
(define (adjoin-set-h x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set)))
	 (cons x set))
	(else
	 (cons (car set) (adjoin-set-h x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      pairs
      (let ((pair (car pairs)))
	(adjoin-set-h (make-leaf (first pair)
				 (second pair))
		      (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      message
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (if (memq s (symbols tree))
      (if (leaf? tree)
	  '()
	  (let ((left (left-branch tree))
		(right (right-branch tree)))
	    (if (memq s (symbols left))
		(cons 0 (encode-symbol s left))
		(cons 1 (encode-symbol s right)))))
      (error "Cannot encode symbol -- ENCODE-SYMBOL" s)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) leaf-set)
	((= (length leaf-set) 1) (car leaf-set))
	(else
	 (successive-merge (adjoin-set-h (make-code-tree (first leaf-set)
							 (second leaf-set))
					 (cddr leaf-set))))))
