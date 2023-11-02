(import (only (scheme base)
              write-u8
              write-string
              define-record-type
              open-output-bytevector
              get-output-bytevector))

(define-record-type <source-file>
  (source-file path content line-num-offsets)
  source-file?
  (path source-file-path)
  (content source-file-content)
  (line-num-offsets source-file-line-num-offsets))

(define (make-source-file path)
  (with-input-from-file path
    (lambda ()
      (call-with-port (open-output-string)
        (lambda (output)
          (let recur ((c (read-char))
                      (offset 0)
                      (line-num-offsets '(0)))
            (cond
             ((eof-object? c)
              (source-file path
                           (get-output-string output)
                           (list->vector
                            (reverse line-num-offsets))))
             ((char=? c #\newline)
              (write-char c output)
              (let ((offset (+ offset 1)))
                (recur (read-char) offset (cons offset line-num-offsets))))
             (else
              (write-char c output)
              (recur (read-char) (+ offset 1) line-num-offsets)))))))))

(define (source-file-line self line-num)
  (let ((line-num-offsets (source-file-line-num-offsets self)))
    (substring (source-file-content self)
               (vector-ref line-num-offsets line-num)
               (vector-ref line-num-offsets (+ line-num 1)))))

(define (source-file-position-by-offset self offset)
  (let ((line-num-offsets (source-file-line-num-offsets self)))
    (let recur ((lo 0)
                (hi (vector-length line-num-offsets)))
      (cond
       ((< lo hi) (let* ((mid (floor-quotient (+ lo hi) 2))
                         (mid-offset (vector-ref line-num-offsets mid)))
                    (cond
                     ((= mid-offset offset) (cons mid 0))
                     ((< mid-offset offset) (recur (+ mid 1) hi))
                     (else (recur lo mid)))))
       (else (let ((line-num (- lo 1)))
               (cons line-num
                     (- offset (vector-ref line-num-offsets line-num)))))))))


(define-record-type <diagnostic>
  (diagnostic kind offset)
  diagnostic?
  (kind diagnostic-kind)
  (offset diagnostic-offset))

(define (emit-diagnostic-level-message diag)
  (define (diagnostic-level-message self)
    (case (diagnostic-kind self)
      ((incomplete-byte) '(error . "incomplete byte"))
      ((invalid-char) '(error . "invalid char"))))
  (let* ((level-message (diagnostic-level-message diag))
         (level (car level-message))
         (message (cdr level-message)))
    (write level)
    (write-string ": ")
    (write-string message)
    (newline)))

(define (emit-diagnostic sf diag)
  (parameterize ((current-output-port (current-error-port)))
    (emit-diagnostic-level-message diag)
    (write-string (source-file-path sf))
    (write-char #\:)
    (let* ((position (source-file-position-by-offset sf
                                                     (diagnostic-offset diag)))
           (line-num (car position))
           (column-num (cdr position)))
      (write (+ line-num 1))
      (write-char #\:)
      (write column-num))
    (newline)))

(define (diagnose-incomplete-byte sf offset)
  (emit-diagnostic sf (diagnostic 'incomplete-byte (- offset 1))))

(define (diagnose-invalid-char sf offset)
  (emit-diagnostic sf (diagnostic 'invalid-char offset)))

(define (char-hex? c)
  (or (and (char>=? c #\0) (char<=? c #\9))
      (and (char>=? c #\a) (char<=? c #\f))
      (and (char>=? c #\A) (char<=? c #\F))))

(define (parse-hex-char c)
  (cond
   ((and (char>=? c #\0) (char<=? c #\9))
    (- (char->integer c) (char->integer #\0)))
   ((and (char>=? c #\a) (char<=? c #\f))
    (- (char->integer c) (char->integer #\a) -10))
   ((and (char>=? c #\A) (char<=? c #\F))
    (- (char->integer c) (char->integer #\A) -10))
   (else (error "cannot parse char to hex" c))))

(define (compile sf output)
  (call-with-port (open-input-string (source-file-content sf))
    (lambda (input)
      (let recur ((c (read-char input))
                  (offset 0)
                  (comment? #f)
                  (start? #t)
                  (hex 0))
        (cond
         ((eof-object? c)
          (cond
           (start?)
           (else (diagnose-incomplete-byte sf offset))))
         (comment?
          (cond
           ((char=? c #\newline)
            (recur (read-char input) (+ offset 1) #f start? hex))
           (else (recur (read-char input) (+ offset 1) #t start? hex))))
         ((char=? c #\;)
          (recur (read-char input) (+ offset 1) #t start? hex))
         ((char-hex? c)
          (cond
           (start? (recur (read-char input)
                          (+ offset 1)
                          #f #f
                          (* 16 (parse-hex-char c))))
           (else (write-u8 (+ hex (parse-hex-char c)) output)
                 (recur (read-char input)
                        (+ offset 1)
                        #f #t
                        0))))
         ((or (char=? c #\space) (char=? c #\newline))
          (recur (read-char input) (+ offset 1) #f start? hex))
         (else (diagnose-invalid-char sf offset)))))))

(define (transform in-path out-path)
  (let ((sf (make-source-file in-path)))
    (call-with-output-file out-path
      (lambda (output)
        (compile sf output)))))

(define (main)
  (let ((cmd (command-line)))
    (cond
     ((= (length cmd) 3) (transform (cadr cmd) (caddr cmd)))
     (else (parameterize ((current-output-port (current-error-port)))
             (write-string "usage: ")
             (write-string (car cmd))
             (write-string " <input> <output>")
             (newline))))))

(main)
