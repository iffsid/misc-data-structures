(module misc-data-structures *
(import chicken scheme extras)
(use define-structure srfi-1 traversal)

;;; Zippers

(define-structure zipper tree thread)

(define (zipper:initialize tree) (make-zipper tree '()))

(define (zipper:descend zipper i)
 (unless (<= 0 i (- (length (zipper-tree zipper)) 1))
  (error "zipper: cannot descend to out-of-bounds index"))
 (let ((elem (list-ref (zipper-tree zipper) i)))
  (make-zipper
   elem
   (cons (map-indexed (lambda (e j) (if (= i j) #f e)) (zipper-tree zipper)) (zipper-thread zipper)))))

(define (zipper:ascend zipper)
 (when (null? (zipper-thread zipper)) (error "zipper: cannot ascend any further"))
 (make-zipper
  (map (lambda (e) (if e e (zipper-tree zipper))) (car (zipper-thread zipper)))
  (cdr (zipper-thread zipper))))

(define (zipper:descend-seq zipper seq)
 (let loop ((z zipper) (s seq))
  (if (null? s) z (loop (zipper:descend zipper (car s)) (cdr s)))))

(define (zipper:ascend-n zipper n)
 (unless (<= 0 n) (error "zipper: cannot ascend negative levels"))
 (case n
  ((0) zipper)
  ((1) (zipper:ascend zipper))
  (else (zipper:ascend-n zipper (- n 1)))))

(define (zipper:can-ascend? zipper) (not (null? (zipper-thread zipper))))

(define (zipper:ascend-until zipper predicate)
 (let loop ((z zipper))
  (if (predicate z) z (loop (zipper:ascend zipper)))))

(define (zipper:ascend-height zipper) (length (zipper-thread zipper)))
)
