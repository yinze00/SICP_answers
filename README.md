#charpter 1
* exercise 1.6
<pre>
(define (new-if p t l)
  (cond (p t)
        (else l)))
(new-if #t (display "good") (display "bad"))
(define (square x)
  (* x x))
(define (sqrt guess x)
  (if( <(abs ( - (square guess)
                 x))
        0.000000001)
  guess
  (sqrt (/ (+ guess
              (/ x guess))
           2) x)))
(sqrt 1 4) 
(define (s guess x)
  (new-if( <(abs ( - (square guess)
                 x))
        0.001)
  guess
  (sqrt (/ (+ guess
              (/ x guess))
           2) x)))
(s 1 3)
</pre>
* exercise 1.7
<pre>
(define (new-sqrt guess x f)
  (if ( < (/ (abs ( - guess f))
             guess)
          0.001)
      guess
      (new-sqrt (/ (+ guess (/ x guess))
                   2)
                x
                guess)))
 (new-sqrt 1 78 0)
</pre>
* exerciese 1.8
<pre>
(define (sqrt guess x)
  (if( <(abs ( - (* (square guess)
                    guess)
                 x))
        0.1)
  guess
  (sqrt (/ (+ (/ x
                 (square guess))
              (* 2 guess))
           3)
        x)))
(sqrt 1 8)
</pre>
* section 1.1.9
<pre>
(define (fact x)
  (define (factorial count cumul max)
    (if (> count max)
        cumul
        (factorial (+ count 1)
                   (* cumul count)
                   max)))
  (factorial 1 1 x))
(define (new-fact x)
  (define (factorial n)
    (if (= n x)
        1
        (* (+ n 1)
           (factorial (+ n 1)))))
 (factorial 1))
(new-fact 5)
(fact 3)
</pre>
* exercise 1.10
<pre>
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))	
</pre>
* section 1.2.2 树形递归
<pre>
(define (const-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or(< amount 0) (= kinds-of-coins 0))0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
    (cc amount 5))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
</pre>
* exercise 1.11
<pre>
(define (f n)
  (if (< n 3) n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (g n)
  (define (next a b c)
    (+ (* 3 a)
       (* 2 b)
       c))
  (define (new-g a b c count)
    (if (> count n) (next a b c )
        (new-g b c (next a b c) (+ count 1))))
  (if (< n 3) n
      (new-g 0 1 2 4)))
(g 3)
(f 3)
</pre>
* exercise 
* 
<pre>
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sin x)
  (if (< x 0.01)
      x
      (p ( sin (/  x 3)))))
</pre>
* section 1.2.4
<pre>
(define (exp b n)
  (if (= n 0)
      1
      (* b (exp b (- n 1)))))

(define (expt b n)
  (define (f acumul count)
    (if (= count n)
        acumul
        (f (* acumul b) (+ count 1))))
  (f 1 0))

(define (e b n)
  (define (square x)(* x x))
  (if (= 0 n) 1
      (if (= 0 (remainder n 2))
          (square (e b (/ n 2)))
          (* b (square (e b (/ (- n 1) 2)))))))
</pre>
* exercise 1.16
<pre>
(define (expt b n)
  (define (f acumul count)
    (if (= count n)
        acumul
        (f (* acumul b) (+ count 1))))
  (f 1 0))
</pre>
* exercise 1.17
<pre>
(define (multi a b)
  (if (= 0 b )
      0
      (+ a (multi a (- b 1)))))

(define (multiply a b)
  (define (double x) (+ x x))
  (define (half x)(/ x 2))
  (if (= b 0) 0
      (if (= 0 (remainder b 2))
          (double (multiply a (half b)))
          (+ a (double
            (multiply a (half (- b 1))))))))
</pre>
* exercise 1.18
<pre>
(define (multi a b)
  (if (= 0 b )
      0
      (+ a (multi a (- b 1)))))

(define (multiply a b)
  (define (double x) (+ x x))
  (define (half x) (/ x 2))
  (define (f a b c)
    (if (= b 0) c
          (if (= 0 (remainder b 2))
              (f (double a) (half b) c)
              (f a (- b 1) (+ a c)))))
  (f a b 0))
(multiply 12 3)
</pre>
* exercise 1.19
<pre>
用O(n)=log n的对数步骤求出斐波那契数列

</pre>

* exercise 1.19
<pre>
(define (fib n)
  (define (square x) (* x x))
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((= 0 (remainder count 2))
           (fib-iter a
                     b
                     (+ (square p ) (square q))
                     (+ (square q) (* 2 p q ))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q ) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(fib 34)
</pre>
* prablem 1.2.6
<pre>
寻找最小非1因子的算法
(define (smallest-divisor n)
  (define (square x)(* x x))
  (define (divides? a b)(= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
(define (prime n)
  (= (smallest-divisor n) n))

费马检查算法
(define (expond base exp m )
  (define (square x)(* x x))
  (cond ((= exp 0) 1)
        ((= 0 (remainder exp 2))
         (remainder ( square (expond base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expond base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expond a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)(fast-prime? n (- times 1)))
        (else false)))
</pre>
* exercise 1.22
<pre>
(define (runtime)(current-milliseconds))
(define (smallest-divisor n)
  (define (square x)(* x x))
  (define (divides? a b)(= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
(define (prime n)
  (= (smallest-divisor n) n))

(define (time-prime-test n)
    
  (define (report-prime elapsed-time)
    (display "* * *")
    (display elapsed-time))
  (define (start-prime-test n start-time)
    (if (prime n)
        (report-prime (- (runtime) start-time))
        (display "Not a Prime")))
  (newline)
  (display n)
  (start-prime-test n (runtime)))
</pre>

* exercise 1.22-own
<pre>
(define (runtime)(current-milliseconds))
(define (smallest-divisor n)
  (define (square x)(* x x))
  (define (divides? a b)(= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
(define (prime n)
  (= (smallest-divisor n) n))

(define (search-3-prime n)
  (define (p n time)
    (if (prime n) (display n) (display ""))
    (if (prime n) (display " " ) (display ""))
    (if (prime n) (display (- (runtime) time))(display ""))
    (if (prime n) (display "\n") (display ""))
    (prime n))
  (define (f n count time_int)
    (if (= count 3) (display "over\n")
        (if (p n time_int)
            (f (+ n 2) (+ count 1) (runtime))
            (f (+ n 2) count (runtime)))))
  (f (+ n 1) 0 (runtime)))

</pre>
* exercise 1.29
<pre>
(define (sum term next a b)
  {if (> a b)
      0
      (+ (term a)
         (sum term next (next a) b))})
(define (inc a) (+ a 1))
(define (cube a) (* a a a))
(define (int a) a)
(define (sum-cub a b)
  (sum cube inc a b))

(define (intergal f a b dx)
  (define (add-dx x)(+ x dx))
  (* dx (sum f add-dx (+ a (/ dx 2.0)) b)))

(define (simpson term a b n)
  (define h (/ (- b a) n))
  (define (y k)(*(term (+ a (* h k)))
                 (cond ((or(= k 0)(= k 1)) 1)
                       ((= 0 (remainder k 2)) 2)
                       ((= 1 (remainder k 2)) 4))))
  (* (/ h 3)
     (sum y inc 0 n)))
</pre>
* exercise 1.30
<pre>
(define (new-sum term next a b)
  (define (iter a result)
    (if(> a b)
       result
       (iter  (next a) (+ (term a) result))))
  (iter a 0))
</pre>

* exercise 1.31
<pre>
(define (accumul f next a b )
  (if (> a b)
      1
      (* (f a)
         (accumul f next (next a) b))))
(define (p a)(- 1 (/ 1(* a a))))
(define (add-a a) (+ a 2))
(define (pi b)
  (* 4 (accumul p add-a 3 b)))
</pre>
* exercise 1.32
<pre>
(define (accumul f next a b )
  (if (> a b)
      1
      (* (f a)
         (accumul f next (next a) b))))
(define (p a)(- 1 (/ 1(* a a))))
(define (add-a a) (+ a 2))
(define (pi b)
  (* 4 (accumul p add-a 3 b)))
</pre>
* exercise 1.33
<pre>
	a) 递归
<pre>
(define (accumulate null-value conbine f next a b)
  (if (> a b)
      null-value
      (conbine (f a)
               (accumulate null-value conbine f next (next a) b))))
</pre>
	b) 迭代
<pre>
(define (accumulate-iter null-value conbine f next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (conbine result
                                (f a)))))
  (iter a null-value))
</pre>
</pre>

* exercise 1.35
<pre>
(define (fixed-point f guess)
  (define (abs a)(if (> a 0) a (- 0 a)))
  (define (close-enough? a b)(< (abs (- (f guess) guess)) 0.00001))
  (if (close-enough? guess (f guess))
      guess
      (fixed-point f (f guess))))
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 2)
</pre>
* exercise 1.36
<pre>
(define (fixed-point f guess)
  (define (abs a)(if (> a 0) a (- 0 a)))
  (define (close-enough? a b)(< (abs (- (f guess) guess)) 0.00001))
  (cond((close-enough? guess (f guess)) guess)
       (else (display guess)
             (newline)
             (fixed-point f (f guess)))))
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(define (average a b) (/ (+ a b) 2))
(fixed-point (lambda(x) (average x ((lambda (x) (/ (log 1000) (log x))) x))) 2.0)
</pre>
* exercise 1.37
<pre>
	a) 递归
<pre>
(define (cont-frac n d k)
  (define (f i)
    (if (= i k)
        (/(n i) (d i))
        (/(n i)
          (+ (d i)
             (f (+ i 1))))))
  (f 1))
</pre>
	b) 迭代
<pre>
(define (cont n d k)
  (define (iter i res)
    (cond ((= i k)
           (iter (- i 1) (+ res (/ (n i) (d i)))))
          ((= i 0) res)
          ((< i k)
           (iter (- i 1) (/ (n i) (+ res (d i)))))))
  (iter k 0))
</pre>
</pre>
* exercise 1.38
<pre>
(define (cont n d k)
  (define (iter i res)
    (cond ((= i k)
           (iter (- i 1) (+ res (/ (n i) (d i)))))
          ((= i 0) res)
          ((< i k)
           (iter (- i 1) (/ (n i) (+ res (d i)))))))
  (iter k 0))
(+ (cont (lambda(x) 1.0) (lambda(x) (if (= 0 (remainder (+ x 1) 3)) (* 2 (/ (+ x 1) 3)) 1.0)) 10) 2)
</pre>
* exercise 1.40
<pre>
(define (cubic a b c)
  (lambda(x)(+ (* x x x)
               (* a x x)
               (* b x)
               c)))
(newton-method (cubic 1 2 3) 1)
</pre>
* exercise 1.42
<pre>
(define (compose f g)
  (lambda(x) (f (g x))))
</pre>
* exercise 1.43
<pre>
(define (duplicate f n)
  (define (int i) i)
  (define (iter i res)
    (if (> i n)
        res
        (iter (+ i 1)(lambda(x) (f (res x))))))
  (iter 1 int))
</pre>
* exercise 1.44
<pre>
(define (compose f g)
  (lambda(x) (f (g x))))
(define (duplicate f n)
  (define (int i) i)
  (define (iter i res)
    (if (> i n)
        res
        (iter (+ i 1)(lambda(x) (f (res x))))))
  (iter 1 int))

(define (smooth f dx)
  (lambda(x)
    (/ (+ (f x)
          (f (+ x dx))
          (f (- x dx)))
       3)))

(define (iter-smooth f dx n)
  (duplicate (smooth f dx) n))
</pre>
* exercise 1.45
<pre>
(define (average-damp f)
  (lambda(x) (average x (f x))))

(define (pow x n)
  (define (iter i res)
    (if (> i n)
        res
        (iter (+ i 1) (* x res))))
  (iter 1 1))

(define(damped-n-times f n)
  ((duplicate average-damp n) f))
(define (square x) (* x x))
((damped-n-times square 10) 10.0)
(define (n-sqrt-root n x)
  (fixed-point (damped-n-times
                (lambda(y) (/ x
                             (pow y (- n 1))))
                n) 1.0))
</pre>
* exercise 1.46
<pre>
(define (iterative-improve close-enough? improve)
    (lambda (first-guess)
        (define (try guess)
            (let ((next (improve guess)))
                (if (close-enough? guess next)
                    next
                    (try next))))
        (try first-guess)))
(define (new-fixed-point f guess)
  ((iterative-improve (lambda(x y)(< (abs (- x y)) 0.0001))(lambda(x)(f x)))guess))
</pre>
