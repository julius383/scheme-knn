#!/usr/bin/csi -script
(use csv-string)
; (use srfi-1)

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
    ((or (null? schema) (= (string-length field) 0) (null? field)) -1)
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
(define (read-records port del_char)
  (let ((l '()))
      (do
        ((s (read-line port) (read-line port)))
        ((eof-object? s) (filter (lambda (x) (> (length x) 3)) l))
        (set! l (append l (list (parse-line s del_char)))))))

;; Processes a csv-record string into an list and returns
;; the resulting list
(define (parse-line s del_char)
  (let ((parser (csv-parser del_char)))
    (cond
      ((not (string? s)) '())
      (else
        (car (map csv-record->list (parser s)))))))

(define (write-line x port)
  (write x port)
  (write-char #\newline port))

(define (filename f)
  (apply string-append (butlast (string-split f "."))))

(define (sexp-write l out)
    (for-each (lambda (x) (write-line x out)) l)
    (close-output-port out))

(define (csv->sexp csv_file)
  (let* ((in (open-input-file csv_file))
        (out (open-output-file 
               (string-append (filename csv_file) ".sexp")))
        (schema (parse-line (read-line in) #\tab))
        (records (read-records in #\tab)))
    (split-genres records schema)
    (set! (list-ref records 0) (zip schema '("str" "int" "int" "lst" "int" "int")))
    (sexp-write records out)))

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
(csv->sexp "test_imdb.tsv")
