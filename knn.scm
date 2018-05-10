(include "db")
(use data-structures)

(define-syntax in
  (syntax-rules ()
    ((_ lst obj)
     (let loop ((l (sort lst string-ci<)))
       (if (null? l)
           #f
           (or (string-ci=? obj (car l)) (loop (cdr l))))))))

(define (create-feature-vector objf fullf)
  (map (lambda (x) (if (in objf x) 1 0)) fullf))

(define (make-fullf id1 id2 db)
  (delete-duplicates
    (sort (append (fetch-unique-tags db id1) (fetch-unique-tags db id2)) string-ci<)))

(define db (open-database "ml.db"))
(define fv1 (fetch-unique-tags db 1))
(define fv2 (fetch-unique-tags db 2))
(display (create-feature-vector fv1 (make-fullf 1 2 db)))
(newline)
(display (create-feature-vector fv2 (make-fullf 1 2 db)))
(newline)
