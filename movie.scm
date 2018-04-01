#!/usr/bin/csi -script
(use csv-string)

;; creates a single entry as a list
(define (make-entry name . attr)
  (cons name attr))

;; Intergrates printing an atom and newline into single function
(define (ndisp x . port)
  (display x port)
  (newline port))

;; Formats a list and prints it in more human readable form
(define (pprint x)
  (cond
    ((not (list? x)) (ndisp x))
    (else
      (do 
        ((l (cdr x) (cdr l))
         (a (car x) (car l)))
        ((null? l) (ndisp a))
        (ndisp a)))))

;; Returns the index of a field in a record that follows the
;; given Schema
(define (find-field field schema)
  (cond 
    ((or (null? schema) (= (string-length field) 0)) -1)
    ((boolean? (member field schema)) -1)
    (else
      (do
        ((i 0 (add1 i)))
        ((string=? (list-ref schema i) field) i)))))

;; Returns the item in the record corresponding to the 
;; given field.
(define (get-field field record schema)
  (let ((rn (find-field field schema)))
    (if (= rn -1)
        #f
        (list-ref record rn))))

;; Reads a csv file r_file into adds each record as
;; a member of a list
(define (read-records in del_char)
    (do
      ((l '() (cons (parse-line s del_char) l))
       (s (read-line in) (read-line in)))
      ((null? (parse-line s del_char)) (reverse l))))
      

;; Processes a csv-record string into an list and returns
;; the resulting list
(define (parse-line s del_char)
  (let ((parser (csv-parser del_char)))
    (cond
      ((not (string? s)) '())
      (else
        (car (map csv-record->list (parser s)))))))

(define (write-line x . port)
  (write x port)
  (write-char #\newline))

(define (filename f)
  (apply string-append (butlast (string-split f "."))))

(define (sexp-write l f)
  (let ((out (open-output-file f)))
    ; (ndisp (zip schema '("str" "int" "int" "lst" "int" "int")) out)
    (for-each (lambda (x) (write-line x out)) l)
    (close-output-port out)))


;; Splits genres that are a delimited list into individual
;; strings of a list
(define (split-genres l s)
  (for-each 
    (lambda (x)
         (set! 
           (list-ref x (find-field "genres" s)) 
           (string-split (get-field "genres" x s) ",")))
    l))

;; Returns true if an atom a occurs in a list l
(define (inside? a l)
  (cond 
    ((null? l) #f)
    (else
      (or (string-ci=? a (car l)) (inside? a (cdr l))))))

;; Adds to l1 all the items from l2 that are not already
;; present
(define (add-if-missing l1 l2)
  (let ((res l1))
      (do
        ((i 0 (add1 i)))
        ((= i (length l2)) res)
        (if 
          (not (inside? (list-ref l2 i) l1)) 
           (set! res (append res (list (list-ref l2 i))))))))


; (define in (open-input-file "test_imdb.tsv"))
; (define l1 (read-records in #\tab))
; (ndisp (cadr l1))
; (define schema '("title" "year" "runtime" "genres" "score" "number_of_ratings"))
; (split-genres l1 schema)
; (ndisp (cadr l1))
; (sexp-write l1 "test_imdb.sexp")
