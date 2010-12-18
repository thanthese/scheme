;;
; Stephen Mann
; Dec 18, 2010
;
; Mergesort, implemented in scheme from basic, basic primitives.
;

; aliases for consts
(define true #t)
(define false #f)
(define else #t)

; return second element of list
(define second cadr)

; apply function to list if list is not null, otherwise return null
(define (null-safe f ls)
  (if (null? ls)
    '()
    (f ls)))

; null-safe head
(define (head ls)
  (null-safe car ls))

; null-safe tail
(define (tail ls)
  (null-safe cdr ls))

; split list into equal parts, return as 2-n list; order is destroyed
;
; It does this in a bizarre way. Imagine taking a deck of cards, pulling
; the cards off the top one at a time, and alternating putting them in
; two piles. This is what this function does. Therefore, you end up with
; two, alternating, reversed lists.
(define (split ls)
  (define (put-card pile-a pile-b deck toggle)
    (if (null? deck)
      (list pile-a pile-b)
      (if toggle
        (put-card (cons (head deck) pile-a) pile-b (tail deck) (not toggle))
        (put-card pile-a (cons (head deck) pile-b) (tail deck) (not toggle)))))
  (put-card '() '() ls true))

; Return true if the head of the smaller list is smaller than or equal
; to the head of the second list. Null-safe. Consider '() to be smaller
; than any value.
(define (first-is-smaller list-a list-b)
  (let ((a (head list-a))
        (b (head list-b)))
    (cond ((null? a) true)
          ((null? b) false)
          (else (<= a b)))))

; return smaller list, based on definition in "first-is-smaller"
(define (min-list list-a list-b)
  (if (first-is-smaller list-a list-b)
    list-a
    list-b))

; return larger list, based on definition in "first-is-smaller"
(define (max-list list-a list-b)
  (if (first-is-smaller list-a list-b)
    list-b
    list-a))

; return if the given list is shorter than two elements
(define (shorter-than-two? ls)
  (null? (head (tail ls))))

; merge two sorted lists into a single sorted list
(define (merge list-a list-b)
  (let ((small (min-list list-a list-b))
        (large (max-list list-a list-b)))
    (if (null? small)
      large
      (cons (head small)
            (merge (tail small) large)))))

; at last, a mergesort
(define (mergesort ls)
  (if (shorter-than-two? ls)
    ls
    (let ((pair (split ls)))
      (merge (mergesort (head pair))
             (mergesort (second pair))))))

; null-safe equals for comparing two scalars
(define (equals? a b)
  (cond ((and (null? a) (null? b)) true)
        ((or (null? a) (null? b)) false)
        (else (= a b))))

; null-safe equals for comparing two lists
(define (equal-lists? list-a list-b)
  (cond ((and (null? list-a)
              (null? list-b))
         true)
        ((equals? (head list-a)
                  (head list-b))
         (equal-lists? (tail list-a)
                       (tail list-b)))
        (else false)))

; the proof
(list
  (equal-lists? '(1 2 3 4 5 6 7 8 9)
                (mergesort '(3 9 8 1 6 4 2 7 5)))
  (equal-lists? '() (mergesort '()))
  (equal-lists? '(1 2 3 3 3 4 5 6 7 8 9)
                (mergesort '(3 9 8 3 1 6 4 2 3 7 5)))
  (equal-lists? '() (mergesort '()))
  (equal-lists? '(1 2) (mergesort '(2 1))))
