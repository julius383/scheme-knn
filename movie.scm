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

;; Writes x to the given port as a single independent line
(define (write-line x port)
  (write x port)
  (write-char #\newline port))

;; Returns a filename from a string in the name
;; filename.ext
(define (filename f)
  (let ((name (butlast (string-split f "."))))
    (if (> (length name) 1)
        (apply string-append (intersperse name "."))
        (apply string-append name))))

;; Writes each element of l to the given port on
;; a single line
(define (sexp-write l out)
    (for-each (lambda (x) (write-line x out)) l)
    (close-output-port out))

(define (csv->sexp csv_file)
  (let* ((in (open-input-file csv_file))
        (out (open-output-file 
               (string-append (filename csv_file) ".sexp")))
        (schema (parse-line (read-line in) #\tab))
        (records (read-records in #\tab))
        (types '("str" "int" "int" "lst" "int" "int")))
    (split-genres records schema)
    (set! (list-ref records 0) (zip schema types))
    (do
      ((i 1 (add1 i)))
      ((= i (length records)))
      (set! 
        (list-ref records i) 
        (map (lambda (x) (make-proper-type (list-ref x 0) (list-ref x 1)))
             (zip (list-ref records i) types))))
    (sexp-write records out)))

(define (make-proper-type atom type)
  (cond
    ((string-ci=? type "int") (string->number atom))
    (else atom)))

(define (sexp-read f)
  (let* ((records '())
         (in (open-input-file f)))
    (do
      ((s (read-line in) (read-line in)))
      ((eof-object? s) (reverse records))
      (set! records (cons (with-input-from-string s read) records)))))


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

(define (print-n l n)
  (do
    ((i 0 (add1 i)))
    ((= i n))
    (display (list-ref l i))
    (newline)))

(define (get-comparator type)
  (cond
    ((string-ci=? type "str") string-ci<?)
    ((string-ci=? type "int") <)
    ((string-ci=? type "lst") (lambda (x y) (< (length x) (length y))))
    (else equal?)))

(define (compare-elements index fn a b)
  (fn (list-ref a index) (list-ref b index)))

(define (sort-by attr l)
  (let* ((schema (map car (car l)))
         (types (map last (car l)))
         (fn (find-field attr schema)))
    (display fn)
    (newline)
    (if (>= fn 0)
        (cons (car l) (sort (cdr l) 
                            (lambda (x y) 
                              (compare-elements fn 
                                                (get-comparator (list-ref types fn)) 
                                                x y)
                                           )))
        l)))

; (csv->sexp "test_500.tsv")
; (display (number? (make-proper-type "123" "int")))
; (newline)
(define r (sexp-read "test_500.sexp"))
(print-n r 5)
(newline)
(print-n (sort-by "year" r) 5)
(newline)
(print-n (sort-by "score" r) 5)
