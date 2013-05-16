; jay kominek, 2005
; jkominek@miranda.org
; a part of pegleg
; yarrr!

; Procedures, created by other procedures (peg:*), will parse the
; user-provided PEG grammar. As they do so, they will invoke the procedures
; which created them, to construct new procedures on their own order, which
; will then act as a parser for the grammar they were fed.

; we need the list SRFI
(require (lib "1.ss" "srfi"))  ; lists
(require (lib "9.ss" "srfi"))  ; records

(define-record-type :peg-result
  (make-peg-result success consumed value)
  peg-result?
  (success succeeded?)
  (consumed consumed)
  (value value))

; we fail a lot. might as well only make the list once!
(define failure! (make-peg-result #f 0 '()))

(define memoize
  (lambda (f)
    (let ([table (make-hash-table)])
      (lambda (input-arg)
        (hash-table-get table input-arg
                        (lambda x
                         (let ( [r (f input-arg)] )
                           (hash-table-put! table input-arg r)
                           r)))))))

; important!
; this is just a variation on the way letrec works.
(define-syntax grammar 
   (syntax-rules () 
     ((_ start ((var init) ...)) 
      (let () 
        (define var (memoize init))
;        (define var (V 'var (memoize init)))
        ... 
        (let () start))))) 

(define join
  (letrec ( (subjoin (lambda (joinstr list)
                       (cond 
                         ((null? list)       "")
                         ((null? (cdr list)) (string-append joinstr (car list)))
                         (else               (string-append joinstr (car list) (subjoin joinstr (cdr list))))))) )
    (lambda (joinstr list)
      (if (null? list)
          ""
          (string-append (car list) (subjoin joinstr (cdr list)))))))

; peg:choice implements the / operator
; the predicate receives the value from the matching item
(define peg:choice
  (lambda choices
    ; our internal recursive procedure
    (letrec ((find-valid-choice
              (lambda (input choices)
                ; if we're out of choices, we reached the
                ; end without finding a winner.
                (if (null? choices)
                    ; #t: yup, ran out. fail.
                    failure!
                    ; #f: we still have a choice to try.
                    ; run it and store the result.
                    (let ( (result ((car choices) input)) )
                      ; check the choice for success
                      (if (succeeded? result)
                          ; #t: it was good, return it
                          result
                          ; #f: it failed, check the next
                          ; one and return its value.
                          (find-valid-choice input
                                             (cdr choices))))))))
          (lambda (input)
            ; our helper does all the work. woohoo.
            (find-valid-choice input choices)))))

; a two-choice peg:choice
;(define peg:choice
;  (lambda (choicea choiceb)
;    (lambda (input)
;      (let ( (resulta (choicea input)) )
;        (if (succeeded? resulta)
;            resulta
;            (let ( (resultb (choiceb input)) )
;              (if (succeeded? resultb)
;                  resultb
;                  failure!)))))))
  
; peg:sequence implements the concept of a sequence
; the predicate receives the list of values from the items
(define peg:sequence
   (lambda items
     (letrec ((process-items
               (lambda (input items)
                 ; if we're out of stuff, ret '()
                 (if (null? items)
                     '()
                     ; otherwise, run the car item on the input
                     (let ( (result ((car items) input)) )
                       ; check item for success
                       (if (succeeded? result)
                           ; #t: it worked, recurse to see if the rest finishes
                           (let ( (next-results
                                   (process-items (drop input (consumed result))
                                                  (cdr items))) )
                             ; either we were at the end, or we succeeded
                             (if (or (null? next-results) (succeeded? (car next-results)))
                                 ; #t: so cons the results on ours, and return it
                                 (cons result next-results)
                                 ; #f: something later failed, so we'll fail out
                                 (list failure!)))
                           ; #f: our item failed, fail out
                           (list failure!)))))))
       ; proc that is run while parsing
       (lambda (input)
         ; invoke our recursive procedure to get things done
         (let ( (tmp-result (process-items input items)) )
           ; for good measure, fold the truth of all the results
           ; and check it.
           (if (fold (lambda (x y) (and (succeeded? x) y)) #t tmp-result)
               ; #t: we succeeded! produce our return value.
               (make-peg-result #t                               ; safely assumed
                                (fold + 0 (map consumed tmp-result)) ; sum the bytes we ate
                                (map value tmp-result))   ; apply our pred to the
               ; results of our items'
               ; preds
               failure!))))))

; peg:kleene-star implements the * operator
; the predicate receives a list of the values from the items or '()
(define peg:kleene-star
  (lambda (item)
    ; internal recursive procedure
    (letrec ( (process-item
               (lambda (input)
                 ; run our item once
                 (let ( (result (item input)) )
                   ; check for success
                   (if (succeeded? result)
                       ; #t: it worked, recurse and get the rest
                       (cons result (process-item (drop input (consumed result))))
                       ; #f: it failed
                       '())))) )
      (lambda (input)
        (let ( (tmp-result (process-item input)) )
          ; for good measure, fold the truth of all the results
          ; and check it.
          (if (fold (lambda (x y) (and (succeeded? x) y)) #t tmp-result)
              ; #t: we succeeded! produce our return value.
              (make-peg-result #t                               ; safely assumed
                               (fold + 0 (map consumed tmp-result)) ; sum the bytes we ate
                               (map value tmp-result))   ; apply our pred to the
              ; results of our items'
              ; preds
              failure!))))))

; peg:kleene-plus implements the + operator
; the predicate receives a list of the values from the items
(define peg:kleene-plus
  (lambda (item)
    (letrec ( (process-item
               (lambda (input)
                 ; run our item once
                 (let ( (result (item input)) )
                   ; check for success
                   (if (succeeded? result)
                       ; #t: it worked, recurse and get the rest
                       (cons result (process-item (drop input (consumed result))))
                       ; #f: it failed
                       '())))) )
      (lambda (input)
        (let ( (tmp-result (process-item input)) )
          ; for good measure, fold the truth of all the results
          ; and check it.
          (if (and (pair? tmp-result) ; here is where we differ from
                                      ; kleene-star. require a result
                   (fold (lambda (x y) (and (succeeded? x) y)) #t tmp-result))
              ; #t: we succeeded! produce our return value.
              (make-peg-result #t                               ; safely assumed
                               (fold + 0 (map consumed tmp-result)) ; sum the bytes we ate
                               (map value tmp-result))   ; apply our pred to the
              ; results of our items'
              ; preds
              failure!))))))

; peg:optional implements the ? operator
; the predicate receives either the value that comes from item,
; or '() if no match
(define peg:optional
  (lambda (item)
    (lambda (input)
      ; run our item
      (let ( (result (item input)) )
        ; if it matched
        (if (succeeded? result)
            ; then great, run our pred and return
            (make-peg-result #t
                             (consumed result)
                             (value result))
            ; otherwise, claim we matched and return
            (make-peg-result #t 0 '()))))))

; peg:and implements the & operator
; the predicate operates on the value received from the item
(define peg:and
  (lambda (item)
    (lambda (input)
      ; run our item
      (let ( (result (item input)) )
        ; if it matched, run our predicate and return
        (if (succeeded? result)
            (make-peg-result #t 0 (value result))
            failure!)))))

; preg:not implements the ! operator
; the predicate always gets '()
(define peg:not
  (lambda (item)
    (lambda (input)
      ; run our item
      (let ( (result (item input)) )
        ; if it matched...
        (if (succeeded? result)
            ; then we fail
            failure!
            ; otherwise, return that we matched
            (make-peg-result #t 0 '()))))))

; checks to see if the first list is a prefix of the
; second list. returns #t if so, #f otherwise.
(define list-head-match?
  (lambda (prefix list)
    (if (null? prefix)
        #t
        (if (and (pair? prefix)
                 (pair? list)
                 (equal? (car prefix) (car list)))
            (list-head-match? (cdr prefix) (cdr list))
            #f))))

; peg:literal implements token matching
; the predicate receives the value of the token
(define peg:literal
  (lambda (token-string)
        ; convert our token into a list.
        (let ( (character-list (string->list token-string)) )
          (lambda (input)
            ; check our token as a prefix of the input
            (if (list-head-match? character-list input)
                ; #t: matches, return it, apply our predicate
                (make-peg-result #t (length character-list) token-string)
                ; #f: fails, return the failure thing.
                failure!)))))

; peg:character implements single character matching
; the predicate receives the value of the character
(define peg:character
  (lambda (character)
    (lambda (input)
      ; if there is a character left, and it is the
      ; right one, run our predicate and return the stuff
      (if (and (not (null? input))
               (equal? character (car input)))
          (make-peg-result #t 1 character)
          failure!))))

; peg:empty implements the zero-length terminal
(define peg:empty
  (lambda ()
    (lambda (input)
      (make-peg-result #t 0 '()))))

; peg:character implements character range matching
; the predicate receives the value of the character
(define peg:range
  (lambda (start stop)
    (lambda (input)
      ; if there is a byte left, and it happens to
      ; be within our defined range, run our
      ; predicate on it and return the stuff
      (if (and (not (null? input))
               (and (char<=? start (car input))
                    (char<=? (car input) stop)))
          (make-peg-result #t 1 (car input))
          failure!))))

; peg:character implements arbitrary character matching
; the predicate receives the value of the character
(define peg:dot
  (lambda ()
    (lambda (input)
      ; if there is still input left, we match a byte
      ; of it. otherwise, we fail.
      (if (not (null? input))
          (make-peg-result #t 1 (car input))
          failure!))))

; peg:character allows reference to other rules
; the predicate receives the other rule's value
;(define peg:nonterminal
;  (lambda (name)
;    (lambda (input)
      ; look up the parsing item in the grammar
;      ((eval name) input))))

(define-syntax peg:nonterminal
  (syntax-rules ()
      ((_ name)
         (lambda (input)
           (name input)))))

; applies a predicate to the semantic value of a
; parse result.
(define @
  (lambda (pred parser)
    (lambda (input)
      (let ( (result (parser input)) )
        (if (succeeded? result)
            (make-peg-result #t
                             (consumed result)
                             (pred (value result)))
            result)))))

(define *global-indent* -1)
(define *be-verbose* #f)
(define *count* 0)
(define V
  (lambda (name parser)
    (lambda (input)
      (set! *count* (+ 1 *count*))
      ;(if *be-verbose*
          ;(begin
            ;(display (list->string (repeat #\space *global-indent*)))
            ;(write name)
            ;(newline)))
      (set! *global-indent* (+ *global-indent* 1))
      (let ( (ret (parser input)) )
        (set! *global-indent* (- *global-indent* 1))
        ret))))

(define Y
  (lambda (X)
    ((lambda (procedure)
       (X (lambda arg (apply (procedure procedure) arg))))
     (lambda (procedure)
       (X (lambda arg (apply (procedure procedure) arg)))))))
(define repeat
  (Y (lambda (f) (lambda (v n) (if (> n 0) (cons v (f v (- n 1))) '())))))

(define fixed (lambda (value) (lambda (x) value)))

; helper function. takes a peg:* procedure and a string.
; runs the parsing chunk on the string
(define parse (lambda (start text) (start (string->list text))))

; walk the parse results
(define traverse
  (lambda (result)
    (if (peg-result? result)
        (traverse (value result))
        (if (pair? result)
            (map (lambda (x) (traverse x)) result)
            result))))
    
; filename goes in, list of characters comes out
(define load-grammar-file
  (lambda (filename)
    (letrec ( (fileport (open-input-file filename))
              (read-proc (lambda (port)
                           (let ( (character (read-char port)) )
                             (if (eof-object? character)
                                 '()
                                 (cons character (read-proc port)))))))
      (read-proc fileport))))

(define anbncn
  '(grammar S
           ( (S (peg:sequence
                 (peg:and (peg:sequence
                           (peg:nonterminal A)
                           (peg:not (peg:character #\b))))
                 (peg:kleene-star (peg:character #\a))
                 (peg:nonterminal B)
                 (peg:not (peg:character #\c))))
             (A (peg:sequence
                 (peg:character #\a)
                 (peg:optional (peg:nonterminal A))
                 (peg:character #\b)))
             (B (peg:sequence
                    (peg:character #\b)
                    (peg:optional (peg:nonterminal B))
                    (peg:character #\c)))
             )))

(define change-start
  (lambda (new-start grammar)
    (list 'grammar new-start (caddr grammar))))

