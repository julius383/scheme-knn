(use csv-string)
(use sql-de-lite)
(use micro-benchmark)
(use (prefix traversal "trav-"))
(use fmt)

;; Processes a csv-record string into an list and returns
;; the resulting list
(define (parse-line s del_char)
  (let ((parser (csv-parser del_char)))
    (cond
      ((not (string? s)) '())
      (else
        (car (map csv-record->list (parser s)))))))

(define (read-records port del_char)
  (map (lambda (x) (parse-line x del_char)) (read-lines port)))

;; Converts a function call with a list into one with
;; multiple separate arguments
(define-syntax list-call
  (syntax-rules ()
    ((_ f args ex_args)
     (let loop ((head (list f args))(tail ex_args))
       (if (null? tail)
           (eval head)
           (loop (append head (list (car tail))) (cdr tail)))))))

;; Inserts a single record into the tags table
;; a record is a list of userID, movieID, tag and timestamp
(define (insert-single-tag db r)
  (let ((qstr "INSERT INTO tags(userID, movieID, tag, timestamp) values (?, ?, ?, ?);")) 
    (list-call exec `(sql db ,qstr) r)))

;; Fetches rows from the tags tablew that match narrowing by
;; a user specified field that matches data
(define (fetch-tags-by field db data)
  (let ((q (string-join (list "SELECT * FROM tags where " field "=?;"))))
    (query fetch-all (sql db q) data)))

(define (fetch-movie-name db movieID)
  (let ((q "SELECT title FROM movies where movieID=?;"))
    (query fetch (sql db q) movieID)))

(define (fetch-unique-tags db movieID)
  (let ((q "SELECT DISTINCT tag from tags where movieID=?;"))
    (map (lambda (x) (car x)) (query fetch-all (sql db q) movieID))))

;; Macro simplifying fetching tags based on userID
(define-syntax fetch-tags-by-userID
  (syntax-rules ()
    ((_ db data)
     (fetch-tags-by "userID" db data))))

;; Macro simplifying fetching tags by its name
(define-syntax fetch-tags-by-name
  (syntax-rules ()
    ((_ db data)
     (fetch-tags-by "tag" db data))))

;; Macro simplifying fetching tags based on the movie
(define-syntax fetch-tags-by-movie
  (syntax-rules ()
    ((_ db data)
     (fetch-tags-by "movieID" db data))))

;; Imports all the tags in tags.csv into a database opened
;; containing a corresponding tags table
(define (import-tags db #!optional (in-file "data/tags.csv"))
  (let ((recs (read-records (open-input-file in-file)#\,))
        (q "INSERT INTO tags(userID, movieID, tag, timestamp) values (?,?,?,?)"))
    (printf "Importing ~S records into tags\n" (length recs))
    (insert-multiple db recs q)))

;; Imports all entries in movielens.csv into the database given
;; that contains a movies table
(define (import-movies db #!optional (in-file "data/movielens.csv"))
  (let ((recs (read-records (open-input-file in-file) #\,))
        (q "INSERT INTO movies(movieID, title, genres) values (?,?,?)"))
    (printf "Importing ~S records into movies\n" (length recs))
    (insert-multiple db recs q)))


;; Insert all records in list into table multiple maximum records
;; at a time
(define (insert-multiple db l qstr)
  (if (< (length l) 1000)
      (list-call exec `(sql db 
                            ,(build-query 
                               (sub1 (length l)) 
                               qstr 
                               (length (list-ref l 0)))) (flatten l))
      (begin
        (list-call exec `(sql db ,(build-query 998 qstr (length (list-ref l 0)))) 
                   (flatten (trav-sublist l 0 999)))
        (insert-multiple db (trav-sublist l 999 (length l)) qstr))))

;; Prints micro-benchmark output in a nicely formated way
(define (disp-benchmark alist)
  (fmt #t
       (tabular
         "|"
         (fmt-join/suffix dsp (map car alist) "\n") 
         "|"
         (fmt-join/suffix dsp (map cdr alist) "\n") 
         "|")))

;; Function to build sql query string. Handles adding
;; the appropriate number of host parameters
(define-syntax make-values
  (syntax-rules ()
    ((_ n)
     (let loop ((s "(") (i n))
       (if (= 0 i)
           (string-append (string-chomp s ",") ")")
           (loop (string-append s "?,") (sub1 i))
           )))))

;; Builds a final sql query that is passed to whichever
;; sqlite3 function requires it
(define (build-query n base_str #!optional (fields 4))
  (let loop (
     (base base_str)
     (counter 0))
    (if (= counter n)
        (string-append base ";")
        (loop (string-append base ", " (make-values fields)) (add1 counter)))))

(define (pretty-nl l)
  (fmt #t (columnar (dsp (string-intersperse l "\n")))))

; (define db (open-database "ml.db"))
; (pretty-nl (fetch-unique-tags db 1))
; (newline)
