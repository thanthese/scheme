;;
; Stephen Mann
; Dec 12, 2010
;
; Building up to a general GCD function using only scheme primatives.
; Along the way builds up a simplistic unit testing "framework".
;
; Uses scheme implementation guile-1.8.
;

;; import macros functionality
(use-syntax (ice-9 syncase))

; define assert form that takes
; - op: operation to test correctness with
; - expected: expected value of test
; - test: expression to test
;
; prints lots of information on failure
(define-syntax assert
  (syntax-rules
    ()
    ((assert op expected test)
     (if (op expected test)
       '_
       (list 'op expected 'test "incorrect result:" test)))))

; demonstrate that assert works
(list
  (assert = 4 (+ 1 3))
  (assert = 4 (+ 1 2 1))
  (assert = 4 (+ 2 3))
  (assert = 4 (+ 1 1 1 1))
  (assert = 4 (+ 1 3)))

; output from above tests of assert:
; (_ _ (= 4 (+ 2 3) "incorrect result:" 5) _ _)


; function aliases
(define first car)
(define rest cdr)

; value aliases
(define else #t)

; return list of all items in ns for which (pred <elem> n) holds true,
; where
; - pred is the predicate function
; - n is the value to compare each element in ns against
(define (filter pred n ns)
  (if (null? ns) '()
    (let ((head (first ns))
          (tail (filter pred n (rest ns))))
      (if (pred head n)
        (cons head tail)
        tail))))

; grossly inefficient implementation of quicksort
(define (sort ns)
  (if (null? ns) '()
    (let ((pivot (first ns))
          (tail (rest ns)))
      (append (sort (filter <= pivot tail))
              (list pivot)
              (sort (filter > pivot tail))))))

; Haskell version of quicksort, as a point of comparison
; qsort []     = []
; qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

; returns the i-th element of list ns, starting from 1 (aka, the []
; operator from most languages)
(define (nth i ns)
  (if (= i 1)
    (first ns)
    (nth (- i 1) (rest ns))))

; helper function to 'gcd'; does the actual GCD work, but doesn't perform
; checks on the input
(define (unguarded-gcd ns)
  (let ((sorted (reverse (sort ns))))
    (let ((biggest (nth 1 sorted))
          (smaller (nth 2 sorted)))
      (if (= smaller 0)
        biggest
        (unguarded-gcd (cons (- biggest smaller)
                             (rest sorted)))))))

; find the GCD of a list of numbers
(define (gcd ns)
  (let ((size (length ns)))
    (cond ((= size 0) 0)
          ((= size 1) (first ns))
          (else (unguarded-gcd ns)))))

(list
  (assert = 7 (gcd '(42 49 7 21 35)))
  (assert = 1 (gcd '(27 50 6)))
  (assert = 5 (gcd '(15 35 65)))
  (assert = 81 (gcd (list (* 27 3) (* 27 9) (* 27 6))))
  (assert = 0 (gcd '()))
  (assert = 1 (gcd '(1)))
  (assert = 4 (gcd '(4)))
  (assert = 1 (gcd '(1 1))))


;; a much shorter GCD version
(define (gcd-tiny a b)
  (let ((small (min a b))
        (big (max a b)))
    (if (= small 0)
      big
      (gcd-tiny small (- big small)))))

(list
  (assert = 2 (gcd-tiny 4 2))
  (assert = 1 (gcd-tiny 3 7))
  (assert = 1 (gcd-tiny 7 3))
  (assert = 12 (gcd-tiny 12 24))
  (assert = 1 (gcd-tiny 130 231))
  (assert = 15 (gcd-tiny 45 210))
  (assert = 9 (gcd-tiny 12341241 9870219)))
