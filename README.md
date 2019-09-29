# SICP_answers

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
* exercise 1.15
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
* exercise 1.21
