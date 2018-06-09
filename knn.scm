(include "db")
(use data-structures)
(use srfi-1)

;; TODO: Abstract ranking by including distance function parameter
;; TODO: Perform benchmarks for space and memory and eliminate bottlenecks
;; TODO: Add way to create dummy data for testing

;; Uses a listing of all possible features an 
;; object can have(fullf) to create a feature vector containing
;; all fields populated not just those in objf
(define (create-feature-vector objf fullf)
  (map (lambda (x) (if (member x objf) 1 0)) fullf))

;; Creates listing of all possible feature vectors
(define (make-fullf t1 t2)
  (delete-duplicates
    (sort (append t1 t2) string-ci<)))

;; Calculates the hamming distance of two feature 
;; vectors s1 and s2
(define (hamming-distance s1 s2)
  (apply + (map 
      (lambda (x) (bitwise-xor (car x) (cadr x))) 
      (zip s1 s2))))

;; Fetched the movieIDs of all other movies except
;; the passed in one
(define (fetch-other-movies db movieID)
  (let ((q "SELECT DISTINCT movieID FROM tags where movieID!=?;"))
    (map 
      (lambda (x) (car x)) 
      (query fetch-all (sql db q) movieID))))

(define (calculate-hamming db movie1 movie2)
  (let* ((tags1 (fetch-unique-tags db movie1))
         (tags2 (fetch-unique-tags db movie2))
         (fullf (make-fullf tags1 tags2)))
    (hamming-distance (create-feature-vector 
                        tags1 fullf)
                      (create-feature-vector
                        tags2 fullf))))

(define (hamming-nearest db movieID)
  (map (lambda (x) (cons 
                     x
                     (calculate-hamming db movieID x)))
       (fetch-other-movies db movieID)))

(define-syntax print-report
  (syntax-rules ()
    ((_ macro formatter)
     (begin
      (printf "Macro:~S\nExpression: ~S\n" 
              (take 'macro (sub1 (length 'macro))) (last 'macro))
      (formatter macro)))))

(define movie1 32)        ;Twelve Monkeys
(define movie2 541)       ;Blade Runner
(define movie3 527)       ;Schindler's list

(define db (open-database "ml.db"))

(print-report (benchmark-measure 
                (hamming-nearest db 541)) (lambda (x) (printf "~S\n" x)))
;(print-report (lambda (x) (+ 5 x)) 5)
(newline)
(close-database db)

