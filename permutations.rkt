#lang racket

(require control)

(define (swap! vec a b)
    (let ([tmp (vector-ref vec a)])
      (begin (vector-set! vec a (vector-ref vec b))
             (vector-set! vec b tmp))))

(struct permutations (a n p i initial-call)
  #:mutable
  #:methods gen:stream
  [(define (stream-empty? perm)
     (= (permutations-i perm) (permutations-n perm)))
   (define (stream-first perm)
     (if (permutations-initial-call perm)
         (begin
           (set-permutations-initial-call! perm #f)
           (vector-copy (permutations-a perm)))
         (begin
           (vector-set! (permutations-p perm)
                        (permutations-i perm)
                        (- (vector-ref (permutations-p perm)
                                       (permutations-i perm))
                           1))
           (let ([j (if (odd? (permutations-i perm))
                        (vector-ref (permutations-p perm)
                                    (permutations-i perm))
                        0)])
             (swap! (permutations-a perm) j (permutations-i perm)))
           (set-permutations-i! perm 1)
           (while (eq? (vector-ref (permutations-p perm)
                                   (permutations-i perm))
                       0)
             (vector-set! (permutations-p perm)
                          (permutations-i perm)
                          (permutations-i perm))
             (set-permutations-i! perm (+ (permutations-i perm) 1)))
           (vector-copy (permutations-a perm)))))
   (define (stream-rest perm)
     perm)])
     

(define (make-integer-vector nmb)
  (list->vector (range 0 (+ nmb 1))))

(define (new-permutations lst)
  (let* ([a (list->vector lst)]
        [n (vector-length a)]
        [p (make-integer-vector n)])
    (permutations a n p 1 #t)))
