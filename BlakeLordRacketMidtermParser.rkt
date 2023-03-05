#lang racket
; Blake Lord
; CS441 Midterm Project
; Parser in Racket
(define (parse [file "file01.txt"])
 (program file)
)
(define (program file)
  (define inputlines (file->lines file))
  (define listoflines (scanner inputlines))
  (println listoflines)
  (linelist listoflines)
)
(define (scanner lines)
  (define tokens '())
  (for ([line lines])
    (define linetoken (string-split line))
    (define fixlinetoken '())
    (define reverseflag #f)
    (for ([token linetoken])
      (cond
      [(equal? (substring token 0 1) "(")
      ;(println "Found")
       (define tf (substring token 0 1))
       (define tl (substring token 1 2))
       (set! reverseflag #t)
       (set! fixlinetoken (append (list tl tf) fixlinetoken))]
      [(equal? (substring token (- (string-length token) 1) (string-length token)) ")")
       ;(println "Found")
       (define tf (substring token (- (string-length token) 2) (- (string-length token) 1)))
       (define tl (substring token (- (string-length token) 1) (string-length token)))
       (set! fixlinetoken (append (list tl tf) fixlinetoken))
       (set! reverseflag #t)]
      [else
       (set! fixlinetoken (cons token fixlinetoken))])
      )
    ;(println fixlinetoken)
    (cond
      [(equal? reverseflag #t)
       ;(println "Reverse Flag Active")
       (set! tokens (cons (reverse fixlinetoken) tokens))]
      [else
       (set! tokens (cons (reverse fixlinetoken) tokens))]
    )
  )
  (set! tokens (reverse tokens))
  tokens
)
(define (linelist listoflines)
  (define linenumber void)
  (cond
    [(empty? listoflines)
     (println "Error, missing EOF symbol")]
    [else (set! linenumber (first (first listoflines)))
  
  (cond
    [(equal? (line (first listoflines)) #t)
     (println "Line Correct")
     (linelist (rest listoflines))]
    [(equal? listoflines (list(list "$$")))
     (println "Accept")]
    [else
     (cond
       [(equal? (idx linenumber) #f)
        (printf "Error, line number missing at ~s"
                  (first listoflines))]
       [else (printf "Error on line ~s: ~s"
                  linenumber
                  (first listoflines))])]
  )]))
(define (tokenizer item)
  (cond
    [(equal? item "=") 'equals]
    [(equal? item "+") 'plus]
    [(equal? item "-") 'minus]
    [(equal? item "0") 'zero]
    [(equal? item ":") 'colon]
    [(equal? item "(") 'openp]
    [(equal? item ")") 'closep]
    [(equal? item "write") 'write]
    [(equal? item "read") 'read]
    [(equal? item "goto") 'goto]
    [(equal? item "gosub") 'gosub]
    [(equal? item "return") 'return]
    [(equal? item "if") 'if]
    [(equal? item "then") 'then]
    [(equal? item "$$") 'EOF]
    [(equal? (idx item) #t) 'idx]
    [(equal? (id item) #t) 'id]
    [else 'unknown]))
(define (idx token)
  (define tokenconvert (string->number token 10 'number-or-false))
  (cond
    [(and (number? tokenconvert)(< 0 tokenconvert))#t]
    [else #f]))
(define (id item)
  (define tokenconvert (string->number item 10 'number-or-false))
  (cond
    [(char-alphabetic? (string-ref item 0))#t]
    [(void? item)
     #f]
    [(not (equal? tokenconvert #f))
     #f]))
(define (line lineitem)
  (let([tokenized (map tokenizer lineitem)])
    (cond
      [(and (equal? (first tokenized) 'idx)(equal? (stmt (rest tokenized)) #t))#t]
      [else #f])
    ))
(define (stmt tokens)
  ;(print "stmt tokens")(println tokens)
  (cond
    [(equal? (first tokens) 'id)
     ;(println "starts with ID")
     (cond
       [(equal? (second tokens) 'equals)
        (expr (rest(rest tokens)))]
     )]
    [(equal? (first tokens) 'goto)
     ;(println "Is Goto")
     (cond
       [(equal? (second tokens) 'idx)
        ;(println "After goto is idx")
        ])#t]
    [(equal? (first tokens) 'gosub)
     ;(println "Is Gosub")
     (cond
       [(equal? (second tokens) 'idx)
        ;(println "After gosub is idx")
       ])#t
     ]
    [(equal? (first tokens) 'read)
     ;(println "Is Read")
     (cond
       [(equal? (second tokens) 'id)
        ;(println "After read is id")
        #t])
     ]
    [(equal? (first tokens) 'write)
     ;(println "Is Write")
     (cond
       [(list? tokens)
       (and (equal? (second tokens) 'id)(equal? (etail (rest (rest tokens))) #f))
        ;(println "After write is id")
        #t]
       [(equal? (etail (rest (rest tokens))) #t)
        ;(println "Etail from write")
        #t]
       [else #f])
     ]
    [(equal? (first tokens) 'return)
     ;(println "Is Return")
     #t]
    [(equal? (first tokens) 'if)
     ;(println "Is If")
     (cond
       [(equal? (expr (rest tokens)) #t)
        ;(println "expr follows if")
        (cond
          [(equal? (stmt (rest (member 'then tokens))) #t)
           ;(println "then verified")
           #t])
     ]
    )]
  ))
(define (expr tokens)
  ;(print "expr tokens")(println tokens)
  (cond
    [(or (equal? (first tokens) 'idx)(equal? (first tokens) 'zero))
     (cond
       [(= (length tokens) 1)
        ;(println "expr id = idx True")
        #t]
       [(equal? (second tokens) 'then)
        ;(println "Exiting etail, then found")
        #t]
       [else (etail (rest tokens))])]
    [(equal? (first tokens) 'id)
     ;(println "expr starts with id")
     (etail (rest tokens))]
    [(equal? (first tokens) 'openp)
     ;(println "Found Open Parenthesis")
     (cond
       [(and (equal? (expr (rest tokens)) #t)(equal? (last tokens) 'closep))
        ;(println tokens)(println "Parenthesis Check")
        #t])]
    [else #f]))
(define (etail tokens)
  ;(print "etail tokens:")(println tokens)
  ;(println "Looking for etail")
  (cond
    [(and (list? tokens)(not (empty? tokens)))
    (cond
      [(equal? (last tokens) 'closep)
       ;(println "Closing Parenthesis")
       #t]
       [(or (equal? (first tokens) 'plus)(equal? (first tokens) 'minus)(equal? (first tokens) 'equals))
     ;(println "Found etail")
     (cond
       [(equal? (expr (rest tokens)) #t)
        ;(println "Found expr in etail")
        #t])]
    [(equal? (first tokens) 'colon)
     (linetail (rest tokens))]
    [(equal? (first tokens) 'closep)
     #t]
    [(equal? (first tokens) 'unknown)
     ;(println "Unknown Token Found")
     #f]
    [else
     #f]
)]))
(define (linetail tokens)
  (cond
    [(equal? (stmt tokens) #t)
     ;(println "Found linetail")
     #t]))