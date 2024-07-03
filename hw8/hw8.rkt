#lang racket

(provide 
 lorint time-calls
 total-order?
 sorted? insert merge
 isort msort
 count-compares
 make-queue)

; Please do not change lines above this one.

;************************************************************
; CS 201 HW #8  DUE Monday May 1st at 11:59 pm, 
; via the submit system on the Zoo. 
;************************************************************
; Name: Mike Masamvu
; Email address: mike.masamvu@yale.edu
;************************************************************

; Computer science topics: running times of programs, insertion sort,
; merge sort.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (10 points)

; Write two procedures

; (lorint count bound)
; (time-calls reps proc args)

; (lorint count bound) takes a nonnegative
; integer count and a positive integer bound
; and returns a list of count randomly chosen integers 
; in the range from 0 through bound - 1.

; (time-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps.

; Recall that we can apply a proc to args with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Examples of lorint
;> (lorint 10 100)
;'(49 14 28 15 12 80 33 69 18 57)
;> (lorint 10 3)
;'(0 0 2 1 0 0 1 2 0 1)

; The following examples of time-calls were run on my workstation and
; show that calling the built-in plus procedure 10,000 times on
; the arguments 13 and 14 took somewhat more than 0.001 seconds,
; while doing the same thing 100,000 times took somewhat more
; than 0.01 seconds, and a million times took somewhat more than 0.1
; seconds.  The first two runs show random variation in the measured times.

; When the number of repetitions is multiplied by 10, the time is
; also (approximately) multiplied by 10.

;> (time-calls 10000 + (list 13 14))
;0.00168701171875
;> (time-calls 10000 + (list 13 14))
;0.00122412109375
;> (time-calls 100000 + (list 13 14))
;0.012380859375
;> (time-calls 1000000 + (list 13 14))
;0.12706494140625

; The following examples show timings (on my workstation)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers in the range 0 to 9 inclusive.
; About a third of a second suffices in the last case.

;> (time-calls 1 lorint (list 100000 10))
;0.074503173828125
;> (time-calls 1 lorint (list 200000 10))
;0.19560009765625
;> (time-calls 1 lorint (list 300000 10))
;0.33381982421875
;******************************(******************************

(define (lorint count bound)
  (if (= 0 count)
      '()
      (cons (random bound) (lorint (- count 1) bound)) 
  ))

(define (time-calls-helper reps proc args) ;;function to output time after execution 
  (if (= 0 reps)
      (list (current-inexact-milliseconds))
      (cons (apply proc args) (time-calls-helper (- reps 1) proc args)))) 
 
(define (time-calls reps proc args)
  (/ (- (current-inexact-milliseconds) (car (reverse (time-calls-helper reps proc args)))) -1000)
  )


;************************************************************
; ** problem 2 ** (15 points)
; For this problem, use your procedure time-calls
; to time the built-in Racket procedures:

; length, take, drop

; and report the following measurements, and answer the following questions.
; Comment out your responses with semicolons.

; For length, report measurements of 100 repetitions of calling length
; on a list of length k * 100,000 for k = 1,2,3,4.

; For take and drop, report measurements of 100 repetitions of calling take (or drop)
; on a list of length k * 100,000 for k = 1,2,3,4, with the number
; of elements to take (or drop) being half the length of the list.

; You may want to do several measurements because of random variation.

; For the procedures length, take, and drop, replace length-runtime,
; take-runtime, and drop-runtime with either O(1) or O(n) to most accurately
; reflect each procedure's respective running time as a function of the length n
; of the list argument.

; Compare the times taken by the three procedures on comparable inputs -- which is 
; fastest? slowest? Replace length-take-drop-ordering with an ordering of these
; procedures. Then, in the space below, explain *why* on the basis of how lists and
; their operations are implemented. (Complex statistical analysis is not
; necessary.)
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

(define lst1 (lorint 100000 10)) ; list of length 1*100,000
(define lst2 (lorint 200000 10)) ; list of length 2*100,000
(define lst3 (lorint 300000 10)) ; list of length 3*100,000
(define lst4 (lorint 400000 10)) ; list of length 4*100,000

; Please report measurements here.

; These are my values from my workstation:

 ;(time-calls 100 take (list lst1  50000)) => 0.144656494140625 
 ;(time-calls 100 take (list lst2  100000)) =>  0.613728759765625
 ;(time-calls 100 take (list lst3 150000)) => 1.639862060546875
 ;(time-calls 100 take (list lst4 200000)) => 1.159878173828125 

 ;(time-calls 100 drop (list lst1 50000)) => 0.013235595703125
 ;(time-calls 100 drop (list lst2 100000)) => 0.024746826171875
 ;(time-calls 100 drop (list lst3 150000)) => 0.0405888671875
 ;(time-calls 100 drop (list lst4 200000)) => 0.05399365234375

 ;(time-calls 100 length (list lst1)) => 0.02286376953125
 ;(time-calls 100 length (list lst2)) => 0.044080810546875
 ;(time-calls 100 length (list lst3)) => 0.060229248046875
 ;(time-calls 100 length (list lst4)) => 0.09355859375

(define length-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
;The runtime of length significantly increases as n increases therefore length is of complexity O(n).

(define take-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
; The runtime of take significantly increases as n increases therefore take is of complexity O(n).

(define drop-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
; The runtime of drop grows proportionally to n, therefore it is of complexity O(n).

; Please do not use commas, and please order from fastest to slowest
; e.g. (define length-reverse-powerset-ordering '(length reverse powerset))
(define length-take-drop-ordering '(drop length take))

; Please explain your ordering here.
; According to the my workstation tests drop has the average shortest runtime followed by length and then take has the longest.

;************************************************************
; We represent a total ordering on a set X of values via a predicate
; (compare? x y), that returns #t or #f.  The results must
; satisfy the following properties for all values x, y, z from the set X:
; (1) if (equal? x y) => #t then (compare? x y) => #t,
;* (1) (if (equal? x y) 
;           (compare? x y) 
;           #t)
; (2) if (and (compare? x y) (compare? y x)) => #t, then (equal? x y) => #t,
;* (2) (if (and (compare? x y) (compare? y x)) 
;           (equal? x y) 
;           #t)
; (3) if (and (compare? x y)(compare? y z)) => #t, then (compare? x z) => #t,
;* (3) (if (and (compare? x y)(compare? y z)) 
;           (compare? x z) 
;           #t)
; (4) (or (compare? x y) (compare? y x)) => #t.
;* (4) (or (compare? x y) (compare? y x))

; If the set X is finite, then we can write a procedure to test
; whether all these properties hold of a proposed total ordering compare? 
; on the set X.  This is what the next problem asks you to do.
; Note that you do NOT need to complete this problem before doing
; the subsequent ones.

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (total-order? compare? domain)

; that takes a predicate (compare? x y) and a list of values domain
; such that whenever x and y are values from domain, (compare? x y)
; returns either #t or #f.
; The procedure returns #t if compare? is a total order on domain
; (that is, satisfies the four properties above for all x, y, z from domain),
; and #f otherwise.

; Hint: it might be helpful to write a procedure to check these conditions
; one pair x, y at a time.

; QUESTION: What is the running time of your procedure in terms of n,
; the number of elements in the domain.  Assume compare? takes time O(1).
; Give your answer in terms of O, Theta, or Omega, as appropriate and
; explain why it is correct. Replace "replace" in total-order-runtime
; with your answer.

; Examples
;> (total-order? <= '(1 3 5 4))
;#t
;> (total-order? < '(1 3 5 4))
;#f
;> (total-order? >= '(3 2 4 5 1))
;#t
;> (total-order? string<=? (list "hi" "hey" "hello"))
;#t
;> (total-order? equal? (list "hi" "hey" "hello"))
;#f
;************************************************************

(define total-order-runtime "O(2^n)")
; Explain your answer here.
; This is because every element is compared with every other element.

(define (cond1 compare? x y) ; Condition 1
  (if (equal? x y) 
      (compare? x y) 
      #t))

(define (cond2 compare? x y)
  (if (and (compare? x y) (compare? y x)) ; Condition 2
           (equal? x y) 
           #t))

(define (cond3 compare? x y z) ; Condition 3
  (if (and (compare? x y)(compare? y z)) 
           (compare? x z) 
            #t))

(define (cond4 compare? x y) ; Condition 4
  (or (compare? x y) (compare? y x)))

(define (conditions? compare? domain x) ;function to test all conditions comparing x and y
  (cond
    [(=(length domain) 1) (and  (cond1 compare? x (car domain))
                                (cond2 compare? x (car domain))
                                (cond4 compare? x (car domain))
                                (cond3 compare? x (car domain) (car domain)))]
    [else (let ([y (car domain)]
                [z (cadr domain)])
                (and  (cond1 compare? x y)
                      (cond2 compare? x y)
                      (cond4 compare? x y)
                      (cond3 compare? x y z)))]))

(define (total-order-helper compare? domain x) ;helper function that returns either #t or #f
  (cond
    [(null? domain) #t]
    [(conditions? compare? domain x) (total-order-helper compare? (cdr domain) x)]
    [else #f]))
                                         
(define (total-order? compare? domain [result #t] [copy domain])
  (cond
    [(null? domain) result]
    [else (total-order? compare? (cdr domain) (and result
                                          (total-order-helper compare? copy (car domain))) copy)]))

;************************************************************

; Now we turn to sorting a list of elements with respect to a given
; comparison operator.  You don't need to have done problem 3 to
; do the following problems.

;************************************************************
; ** problem 4 ** (15 points)
; Write three procedures

; (sorted? compare? lst)
; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For each of these procedures, you may assume that
; compare? is a total order on the elements of lst,
; item and the elements of lst, and the elements of lst1 and lst2,
; respectively.

; (sorted? compare? lst)
; takes a list of items and returns #t or #f
; depending on whether the items of lst are
; sorted with respect to the comparison predicate
; compare?
; In other words, the result should be #f if and only if
; there are two consecutive elements of lst for
; which compare? returns #f.

; (insert compare? item lst)
; inserts an item into a list lst of items
; which is sorted with respect to the compare?
; predicate, so that the resulting list of
; items is also sorted with respect to the
; compare? predicate.

; (merge compare? lst1 lst2)
; takes two lists of elements lst1 and lst2, each of which is sorted with
; respect to the compare? predicate, and produces as its result a list
; of all the items in lst1 and lst2 (preserving duplicates) that is
; sorted with respect to compare?

; Examples
;> (sorted? <= '(1 4 5 8 10))
;#t
;> (sorted? >= '(10 9 4 7 6))
;#f
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangement"))
;'("the" "hello" "best" "arrangement")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************

(define (sorted? compare? lst)
   (cond
    [(equal? 1 (length lst)) #t]
    [(compare? (first lst) (second lst)) (sorted? compare? (rest lst))]
    [else #f]
  ))

(define (insert compare? item lst)
  (cond
    [(empty? lst) (cons item '())]
    [(compare? item (car lst)) (cons item lst)]
    [else
     (cons (car lst) (insert compare? item (rest lst)))]
  ))

(define (merge compare? lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [(compare? (car lst1) (car lst2)) (cons (car lst1) (merge compare? (rest lst1) lst2))]
    [else
     (cons (car lst2) (merge compare? lst1 (rest lst2)))]
  ))

;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (isort compare? lst)
; (msort compare? lst)

; Each takes a total order comparison predicate compare? and a list
; lst of items, and returns a list of all the elements in lst (duplicates
; preserved) arranged so that they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (merge lst1 lst2) and should
; implement merge sort.

; Examples
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (isort compare? lst [value empty])
  (cond
    [(empty? lst) value]
    [else
     (isort compare? (rest lst) (insert compare? (car lst) value))]
  ))

(define (msort compare? lst)
  (cond
    [(null? lst) lst]
    [(null? (cdr lst)) lst]
    [else (merge compare? (msort compare? (take lst (quotient (length lst) 2)))
                          (msort compare? (drop lst (quotient (length lst) 2))))]))

;************************************************************
; ** problem 6 ** (20 points)
; (1)(a) Give empirical evidence that your implementation of insertion sort
;        (isort, above) has best case time Omega(n) and worst case time of
;        O(n^2).
;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.
; (2)(a) Give empirical evidence that your implementation of merge sort
;        (msort, above) has best case and worst case times of Theta(n log n).
;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.

; Be sure to use sufficiently long lists of integers and possibly repeat/average
; measurements.

; (3) Please identify inputs that give best and worst cases for your
; implementations of (a) isort and (b) msort. Be sure that you use sufficiently
; long lists of randomly chosen integers in a range larger than the length of
; the list, so that there are unlikely to be many duplicate values.

; (4) Roughly what is the longest list of random integers that your (a) isort
; procedure can sort in 10 seconds?  Same question for your (b) msort procedure?

; Because of memory caching and other effects, the timing behaviors will not
; necessarily be uniform over the whole range of feasible input lengths.
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

(define sorted-lst1 (msort <= lst1))
(define sorted-lst2 (msort <= lst2))
(define sorted-lst3 (msort <= lst3))
(define sorted-lst4 (msort <= lst4))

(define arr1(lorint 2500 100))
(define arr2(lorint 5000 100))
(define arr3(lorint 10000 100))
(define arr4(lorint 20000 100))

(define sorted-arr1 (msort <= arr1))
(define sorted-arr2 (msort <= arr2))
(define sorted-arr3 (msort <= arr3))
(define sorted-arr4 (msort <= arr4))

; 3a) Please briefly describe best case inputs for your implementation of isort here.

; Best case is when list is already sorted meaning we make very minimal comparisons.
; For each element, we compare our current element to the element at the left only once.
; Since the order is correct, we don’t swap and move on to the next element.

; 3a) Please briefly describe worst case inputs for your implementation of isort here.

; Worst case is when the list is sorted backwards.
; This means that for each element, we have to keep traversing and swapping elements to the left.

; 1b) Please indicate average case running time of your implementation of isort here.

; Average case is when part of the list is sorted and the other is not (randomly sorted list).

; 3b) Please briefly describe best case inputs for your implementation of msort here.

; Best case is when the list is already sorted hence no need to swap elements. 

; 3b) Please briefly describe worst case inputs for your implementation of msort here.

; Worst case is when the left and right sub-lists store alternate elements of a sorted list.
; However that would not change the performance time of msort.

; 2b) Please indicate average case running time of your implementation of msort here.

; Average is when the list has some elements sorted, some elements unsorted (random). 

; 1a, 2a) Please provide evidence for the above claims here.

;1a)Evidence:

;-------Average case Theta(n^2)------------:
;From the below evidence, the running time seems to be Theta(n^2) for randomly sorted lists.

;Evidence (average of ten calls for each):
;(time-calls 1 isort (list <= (lorint 2500 100))) => 0.1133203125
;(time-calls 1 isort (list <= (lorint 5000 100))) => 0.52360107421875
;(time-calls 1 isort (list <= (lorint 10000 100))) => 2.44684423828125
;(time-calls 1 isort (list <= (lorint 20000 100))) => 10.369074462890625

;-------Best case isort Omega(n)---------:

;(average of ten calls each on already sorted arrays per compare?)
;Runtime grows roughly 1:1 with n.
;(time-calls 1 isort (list <= sorted-arr1)); => 0.003669921875  
;(time-calls 1 isort (list <= sorted-arr2)); => 0.006172119140625
;(time-calls 1 isort (list <= sorted-arr3)); => 0.02837255859375
;(time-calls 1 isort (list <= sorted-arr4)); => 0.169279541015625


;-------Worst case isort O(n^2)----------:

;(average of ten calls each on already reversely sorted arrays per compare?)
;Runtime grows exponentially with n as shown 
;(time-calls 1 isort (list >= sorted-arr1)); => 0.253755126953125  
;(time-calls 1 isort (list >= sorted-arr2)); => 1.149384521484375
;(time-calls 1 isort (list >= sorted-arr3)); => 4.81952294921875
;(time-calls 1 isort (list >= sorted-arr4)); => 22.240577880859377

;2a)Evidence:

;-------Average case Theta(nlogn)------------:

;The running time seems to be Theta(nlog(n)) because as we double n, time seems to roughly double+.
;So n grows proportionally to time.
;Evidence (average of ten calls each):
;(time-calls 1 msort (list <= lst1)); => 0.16318701171875  
;(time-calls 1 msort (list <= lst2)); => 0.365148193359375
;(time-calls 1 msort (list <= lst3)); => 0.58362939453125
;(time-calls 1 msort (list <= lst4)); => 0.925940185546875


;-------Best case msort Omega(nlog(n))-------:

;The below evidence confirms Omega(nlog(n)) as runtime  roughly doubles+ as n doubles.  
;(time-calls 1 msort (list <= sorted-lst1)) => 0.102966796875
;(time-calls 1 msort (list <= sorted-lst2)) => 0.23223583984375
;(time-calls 1 msort (list <= sorted-lst3)) => 0.354855224609375
;(time-calls 1 msort (list <= sorted-lst4)) => 0.518725830078125

;-------Worst case msort O(nlog(n))----------:

;(average of ten calls each)
;As we double n, time seems to roughly double.
;So n grows proportionally to time and that proportionality factor is log(n).
;(time-calls 1 msort (list <= (lorint 200000 100))) => 0.405202880859375
;(time-calls 1 msort (list <= (lorint 400000 100))) => 0.960727783203125
;(time-calls 1 msort (list <= (lorint 800000 100))) => 2.174801025390625
;(time-calls 1 msort (list <= (lorint 1600000 100))) => 6.043709228515625


; 4a, 4b) Please indicate the longest list of random integers that a) your isort
; procedure can sort in 10 seconds and b) your msort procedure can sort
; in 10 seconds.

; 4a) isort can sort list of length 20,000 in 10 seconds on my workstation.
; 4b) msort can sort list of length 2,600,000 in 10 seconds on my workstation.

;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (count-compares sort compare? lst)

; that returns the number of calls to the compare? procedure
; when we apply the procedure sort to the arguments compare? and lst.
; Think of sort as a procedure like msort or isort, taking a comparison
; predicate and a list as its arguments, though sort could
; be some other sorting procedure devised for testing purposes.

; The trick here is to take the compare? procedure and "wrap" it
; in another procedure that will count the number of times it
; is called.  Then call sort with the "wrapped" compare? and lst
; as inputs.  Finally, return the final count from the "wrapped"
; compare? as the value from count-compares.

; Please read about the mutator set! to help you keep the count.

; Examples (yours may randomly vary.)
;> (count-compares msort <= (lorint 10 100))
;23
;> (count-compares msort <= (lorint 10 100))
;22
;> (count-compares isort <= (lorint 10 100))
;34
;************************************************************

(define (count-compares sort compare? lst [n 0])
  (sort (lambda (x y) (set! n (+ 1 n))(compare? x y)) lst)
  (+ 0 n))
  
;************************************************************
; ** problem 8 ** (10 points)

; In the Runtime lecture notes, we present a stack data structure.

(define (make-stack name (data empty))
  (let ((stack data)
        (size (length data)))
    (lambda (cmd . args)
      (case cmd
        ((name) name)
        ((empty?)
         (null? stack))
        ((copy)
         (if (null? args)
             'Error:usage:copy_stack
	     (make-stack (first args) stack)))
        ((show)
	 stack)
        ((equal?)
         (if (null? args)
             'Error:usage:equal_stack
	     (equal? stack ((first args) 'show))))
	
        ((push)
         (if (null? args)
             'Error:usage:push_element
             (begin
               (set! stack (cons (first args) stack))
               (set! size (+ size 1))
               (first args))))
        ((size) size)
	
        ((peek)
         (if (null? stack)
             'Error:stack-empty
	     (car stack)))
        ((pop)
         (if (null? stack)
             'Error:stack-empty
             (let ((result (car stack)))
               (set! stack (cdr stack))
               (set! size (- size 1))
               result)))
	(else 'invalid-method)
        ))))


; Write a queue data structure, similar to the stack above.
; Whereas a stack is LIFO (last in first out), a queue is 
; FIFO = first in, first out

; Your queue data structure should implement all the same methods
; as the stack data structure.  However, push is called enqueue,
; and pop is called dequeue.  Here are examples.

; (define q1 (make-queue 'queue1))
; (q1 'name) => 'queue1
; (q1 'empty) => 'invalid-method
; (q1 'show) => '()
; (q1 'enqueue) => 'Error:usage:enqueue_element
; (q1 'enqueue 4) => 4
; (q1 'enqueue 5) => 5
; (q1 'enqueue 6) => 6
; (q1 'peek) => 4
; (q1 'enqueue '(1 2 3)) => '(1 2 3)
; (q1 'size) => 4
; (define q2 (q1 'copy 'queue2))
; (q2 'name) => 'queue2
; (q2 'empty?) => #f
; (q2 'show) => '((1 2 3) 6 5 4)
; (q1 'equal? q2) => #t
; (q2 'equal q1) => 'invalid-method
; (q2 'equal? q1) => #t
; (q1 'equal? q1) => #t
; (q1 'dequeue) => 4
; (q1 'dequeue) => 5
; (q1 'dequeue) => 6
; (q1 'dequeue) => '(1 2 3)
; (q1 'dequeue) => 'Error:queue-empty
; (q1 'size) => 0

(define (make-queue name (data empty))
  (let ((queue data)
        (size (length data)))
    (lambda (cmd . args)
      (case cmd
        ((name) name)
        ((empty?)
         (null? queue))
        ((copy)
         (if (null? args)
             'Error:usage:copy_queue
	     (make-queue (first args) queue)))
        ((show)
	 queue)
        ((equal?)
         (if (null? args)
             'Error:usage:equal_queue
	     (equal? queue ((first args) 'show))))
	
        ((enqueue)
         (if (null? args)
             'Error:usage:enqueue_element
             (begin
               (set! queue (cons (first args) queue))
               (set! size (+ size 1))
               (first args))))
        ((size) size)
	
        ((peek)
         (if (null? queue)
             'Error:stack-empty
	     (car (reverse queue))))
        ((dequeue)
         (if (null? queue)
             'Error:queue-empty
             (let ((result (car (reverse queue))))
               (set! queue (reverse (cdr (reverse queue))))
               (set! size (- size 1))
               result)))
	(else 'invalid-method)
        ))))

;; ANSWER THIS QUESTION:
;; What is the Big-O complexity of enqueue, dequeue, size, and peek?
;; enqueue and dequeue have complexity 0(n)
;; size and peek have constant complexity 0(1)

;************************************************************
; This is where the test code normally appears.
; For this assignment, write your own tests.  

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '---X---)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))



;; Here are some tests for the procedures with predictable results


(test 'total-order?  (total-order? <= '(1 3 5 4)) #t)
(test 'total-order?  (total-order? < '(1 3 5 4)) #f)
(test 'total-order?  (total-order? >= '(3 2 4 5 1)) #t)
(test 'total-order?  (total-order? string<=? (list "hi" "hey" "hello")) #t)
(test 'total-order?  (total-order? equal? (list "hi" "hey" "hello")) #f)

(test 'sorted? (sorted? <= '(1 4 5 8 10)) #t)
(test 'sorted? (sorted? >= '(10 9 4 7 6)) #f)
(test 'insert (insert <= 3 '(1 2 4 5)) '(1 2 3 4 5))
(test 'insert (insert string>=? "hello" (list "the" "best" "arrangement")) '("the" "hello" "best" "arrangement"))
(test 'merge (merge >= '(10 7 4 2 1) '(22 9 5)) '(22 10 9 7 5 4 2 1))
(test 'merge (merge string<=? (list "a" "novel" "thought") (list "more" "predictive")) '("a" "more" "novel" "predictive" "thought"))

(test 'isort (isort string<=? (list "predictive" "novel" "a" "more" "thought")) '("a" "more" "novel" "predictive" "thought"))
(test 'msort (msort string>=? (list "predictive" "novel" "a" "more" "thought")) '("thought" "predictive" "novel" "more" "a"))

(define q1 (make-queue 'queue1))

(test 'make-queue (q1 'name)  'queue1)
(test 'make-queue (q1 'empty)  'invalid-method)
(test 'make-queue (q1 'show)  '())
(test 'make-queue (q1 'enqueue)  'Error:usage:enqueue_element)
(test 'make-queue (q1 'enqueue 4)  4)
(test 'make-queue (q1 'enqueue 5)  5)
(test 'make-queue (q1 'enqueue 6)  6)
(test 'make-queue (q1 'peek)  4) 
(test 'make-queue (q1 'enqueue '(1 2 3))  '(1 2 3))
(test 'make-queue (q1 'size)  4)
(define q2 (q1 'copy 'queue2))
(test 'make-queue (q2 'name)  'queue2)
(test 'make-queue (q2 'empty?)  #f)
(test 'make-queue (q2 'show)  '((1 2 3) 6 5 4))
(test 'make-queue (q1 'equal? q2)  #t)
(test 'make-queue (q2 'equal q1)  'invalid-method)
(test 'make-queue (q2 'equal? q1)  #t)
(test 'make-queue (q1 'equal? q1)  #t)
(test 'make-queue (q1 'dequeue)  4)
(test 'make-queue (q1 'dequeue)  5)
(test 'make-queue (q1 'dequeue)  6)
(test 'make-queue (q1 'dequeue)  '(1 2 3))
(test 'make-queue (q1 'dequeue)  'Error:queue-empty )
(test 'make-queue (q1 'size)  0)


;************************************************************


;********* end of hw8, end of hws! **************************


