#!/usr/bin/csi -script
(use srfi-1)


(define (read-in prompt)
  (printf "~%~A> " prompt)
  (let ((c #\0))
    (do
      ((command '() (cons c command)))
      ((char-ci=? c #\newline) (list->string (reverse (cdr command))))
      (set! c (read-char)))))

(display (read-in "Enter a word"))
(newline)
