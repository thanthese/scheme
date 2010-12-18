;;
; Stephen Mann
; Dec 18, 2010
;
; Mergesort, implemented in scheme from basic, basic primitives.
;

; aliases to consts
(define true #t)
(define false #f)
(define else #t)

; null-safe head
(define (head ls)
  (if (null? ls)
    '()
    (car ls)))

; null-safe tail
(define (tail ls)
  (if (< (length ls) 1)
    '()
    (cdr ls)))

; Return true if the head of the smaller list is smaller than or equal
; to the head of the second list. Null-safe. Consider '() to be smaller
; than any value.
(define (first-is-smaller list-a list-b)
  (let ((a (head list-a))
        (b (head list-b)))
    (cond ((null? a) true)
          ((null? b) false)
          (else (<= a b)))))

; return largest list, based on definition in "first-is-smaller"
(define (max-list list-a list-b)
  (if (first-is-smaller list-a list-b)
    list-b
    list-a))

; return smallest list, based on definition in "first-is-smaller"
(define (min-list list-a list-b)
  (if (first-is-smaller list-a list-b)
    list-a
    list-b))

; merge two sorted lists into a single, larger sorted list
(define (merge list-a list-b)
  (let ((smaller (min-list list-a list-b))
        (larger (max-list list-a list-b)))
    (if (null? smaller)
      larger
      (cons (head smaller) (merge larger (tail smaller))))))

; split list into equal parts, return as 2-n list; order is destroyed
(define (split ls)
  (define (helper list-a list-b rest toggle)
    (if (null? (head rest))
      (list list-a list-b)
      (if toggle
        (helper (cons (head rest) list-a)
                list-b
                (tail rest)
                (not toggle))
        (helper list-a
                (cons (head rest) list-b)
                (tail rest)
                (not toggle)))))
  (helper '() '() ls true))

; at last, a mergesort
(define (mergesort ls)
  (if (< (length ls) 2)
    ls
    (let ((splits (split ls)))
      (let ((a (head splits))
            (b (head (tail splits))))
        (merge (mergesort a)
               (mergesort b))))))

; the proof
(mergesort '(3 9 8 1 6 4 2 7 5))
(mergesort '(3 9 8 3 1 6 3 4 2 3 7 5))
(mergesort '())
(mergesort '(2 1))


;; RANDOM UTILITIES
;
; Things I didn't end up needing, but which are interesting nonetheless.
;

; null-safe equals for comparing two scalars
(define (equals? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        (else (= a b))))

; null-safe equals for comparing two lists
(define (equal-lists? list-a list-b)
  (cond ((and (null? (head list-a))
              (null? (head list-b)))
         true)
        ((equals? (head list-a)
                  (head list-b))
         (equal-lists? (tail list-a)
                       (tail list-b)))
        (else false)))

(equal-lists? '(1 2 3 4 5) (merge '(2 4) '(1 3 5)))
(equal-lists? '(1 1 3 4 5) (merge '(1 4) '(1 3 5)))
