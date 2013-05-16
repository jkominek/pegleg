; jay kominek, 2005
; jkominek@miranda.org
; a part of pegleg
; yarrr!

(load "peglib.scm")

(define PEG-grammar
  (grammar
   PEG
   (
    ; Syntaxy bits
    (PEG
     (@
      (lambda (seq)
        (let ( (definitions (cadr seq)) )
          `(grammar ,(caar definitions)
                    ,definitions)))
      (peg:sequence
       (peg:nonterminal Spacing)
       (peg:kleene-star (peg:nonterminal Definition))
       (peg:nonterminal EndOfFile))))
    (Definition
     (@ (lambda (seq)
          (list (string->symbol (car seq))
                (caddr seq)))
        (peg:sequence
         (peg:nonterminal Identifier)
         (peg:nonterminal LEFTARROW)
         (peg:nonterminal Expression))))
    (Expression
     (@ (lambda (seq)
          (if (null? (cadr seq))
              (car seq)
              `(peg:choice ,@(cons (car seq) (cadr seq)))))
        (peg:sequence
         (peg:nonterminal Sequence)
         (peg:kleene-star
          (@ cadr
             (peg:sequence
              (peg:nonterminal SLASH)
              (peg:nonterminal Sequence)))))))
    (Sequence
     (@ (lambda (seq)
          (let ( (lst (car seq))
                 (code (cadr seq))
                 (expr #f) )
            (begin
              (set! expr 
                    (if (null? lst)
                        '(peg:empty)
                        (if (null? (cdr lst))
                            (car lst)
                            `(peg:sequence ,@lst))))
              (if (null? code)
                  expr
                  (list '@ code expr)))))
        (peg:sequence
         (peg:kleene-star
          (peg:nonterminal Prefix))
         (peg:optional
          (@ (lambda (seq)
               (let ( (code (read (open-input-string (caddr seq)))) )
                 (if (eof-object? code)
                     '()
                     code)))
             (peg:sequence
              (peg:nonterminal RIGHTARROW)
              (peg:nonterminal OPENCODE)
              (@ list->string
                 (peg:kleene-star
                  (@ cadr
                     (peg:sequence
                      (peg:not (peg:nonterminal CLOSECODE))
                      (peg:dot)))))
              (peg:nonterminal CLOSECODE)))))))

    (Prefix
     (@ (lambda (seq)
          (if (null? (car seq))
              (cadr seq)
              `(,(car seq) ,@(cdr seq))))
        (peg:sequence
         (peg:optional
          (peg:choice
           (@ (fixed 'peg:and) (peg:nonterminal AND))
           (@ (fixed 'peg:not) (peg:nonterminal NOT))))
         (peg:nonterminal Suffix))))
    (Suffix
     (@ (lambda (seq)
          (if (null? (cadr seq))
              (car seq)
              `(,(cadr seq) ,(car seq))))
        (peg:sequence
         (peg:nonterminal Primary)
         (peg:optional
          (peg:choice
           (@ (fixed 'peg:optional) (peg:nonterminal QUESTION))
           (@ (fixed 'peg:kleene-star) (peg:nonterminal STAR))
           (@ (fixed 'peg:kleene-plus) (peg:nonterminal PLUS)))))))
    (Primary
     (peg:choice
      (@ (lambda (seq)
           `(peg:nonterminal ,(string->symbol (car seq))))
         (peg:sequence
          (peg:nonterminal Identifier)
          (peg:not (peg:nonterminal LEFTARROW))))
      (@ cadr
         (peg:sequence
          (peg:nonterminal OPEN)
          (peg:nonterminal Expression)
          (peg:nonterminal CLOSE)))
      (@ (lambda (literal)
           `(peg:literal ,(list->string literal)))
         (peg:nonterminal Literal))
      (peg:nonterminal Class)
      (@ (fixed '(peg:dot)) (peg:nonterminal DOT))))

    ; Lexy bits
    (Identifier
     (@ (lambda (seq)
          (list->string (cons (car seq) (cadr seq))))
        (peg:sequence
         (peg:nonterminal IdentStart)
         (peg:kleene-star
          (peg:nonterminal IdentCont))
         (peg:nonterminal Spacing))))
    (IdentStart
     (peg:choice
      (peg:range #\a #\z)
      (peg:range #\A #\Z)
      (peg:character #\_)))
    (IdentCont
     (peg:choice
      (peg:nonterminal IdentStart)
      (peg:character #\-) ; nonstandard!
      (peg:range #\0 #\9)))
    (Literal
     (peg:choice
      (@ cadr
         (peg:sequence
          (peg:character #\')
          (peg:kleene-star
           (@ cadr
              (peg:sequence
               (peg:not
                (peg:character #\'))
               (peg:nonterminal Char))))
          (peg:character #\')
          (peg:nonterminal Spacing)))
      (@ cadr
         (peg:sequence
          (peg:character #\")
          (peg:kleene-star
           (@ cadr
              (peg:sequence
               (peg:not
                (peg:character #\"))
               (peg:nonterminal Char))))
          (peg:character #\")
          (peg:nonterminal Spacing)))))
    (Class
     (@ cadr
        (peg:sequence
         (peg:character #\[)
         (@ (lambda (lst)
              (if (null? (cdr lst))
                  (car lst)
                  (cons 'peg:choice lst)))
            (peg:kleene-star
             (@ cadr
                (peg:sequence
                 (peg:not
                  (peg:character #\]))
                 (peg:nonterminal Range)))))
         (peg:character #\])
         (peg:nonterminal Spacing))))
    (Range
     (peg:choice
      (@
       (lambda (rng)
         `(peg:range ,(car rng) ,(caddr rng)))
       (peg:sequence
        (peg:nonterminal Char)
        (peg:character #\-)
        (peg:nonterminal Char)))
      (@ (lambda (chr)
           `(peg:character ,chr))
         (peg:nonterminal Char))))
    (Char
     (peg:choice
      (@ cadr
         (peg:sequence
          (peg:character #\\)
          (peg:choice
           (@ (fixed #\newline) (peg:character #\n))
           (@ (fixed #\return) (peg:character #\r))
           (@ (fixed #\tab) (peg:character #\t))
           (@ (fixed #\') (peg:character #\'))
           (@ (fixed #\") (peg:character #\"))
           (@ (fixed #\[) (peg:character #\[))
           (@ (fixed #\]) (peg:character #\]))
           (@ (fixed #\\) (peg:character #\\)))))
      (@
       (lambda (seq)
         (integer->char (string->number (list->string (cdr seq)))))
       (peg:choice
        (peg:sequence
         (peg:character #\\)
         (peg:range #\0 #\2)
         (peg:range #\0 #\7)
         (peg:range #\0 #\7))
        (peg:sequence
         (peg:character #\\)
         (peg:range #\0 #\7)
         (peg:optional
          (peg:range #\0 #\7)))))
      (@ cadr
         (peg:sequence
          (peg:not
           (peg:character #\\))
          (peg:dot)))))
    (LEFTARROW
     (peg:sequence (peg:literal "<-") (peg:nonterminal Spacing)))
    (RIGHTARROW
     (peg:sequence (peg:literal "->") (peg:nonterminal Spacing)))
    (SLASH
     (peg:sequence (peg:character #\/) (peg:nonterminal Spacing)))
    (AND
     (peg:sequence (peg:character #\&) (peg:nonterminal Spacing)))
    (NOT
     (peg:sequence (peg:character #\!) (peg:nonterminal Spacing)))
    (QUESTION
     (peg:sequence (peg:character #\?) (peg:nonterminal Spacing)))
    (STAR
     (peg:sequence (peg:character #\*) (peg:nonterminal Spacing)))
    (PLUS
     (peg:sequence (peg:character #\+) (peg:nonterminal Spacing)))
    (OPEN
     (peg:sequence (peg:character #\() (peg:nonterminal Spacing)))
    (CLOSE
     (peg:sequence (peg:character #\)) (peg:nonterminal Spacing)))
    (OPENCODE
     (peg:sequence (peg:character #\{) (peg:nonterminal Spacing)))
    (CLOSECODE
     (peg:sequence (peg:character #\}) (peg:nonterminal Spacing)))
    (COLON
     (peg:sequence (peg:character #\:) (peg:nonterminal Spacing)))
    (DOT
     (peg:sequence (peg:character #\.) (peg:nonterminal Spacing)))
    (Spacing
     (peg:kleene-star (peg:choice (peg:nonterminal Space)
                                  (peg:nonterminal Comment))))
    (Comment
     (peg:sequence (peg:character #\#)
                   (peg:kleene-star
                    (peg:sequence (peg:not (peg:nonterminal EndOfLine))
                                  (peg:dot)))
                   (peg:nonterminal EndOfLine)))
    (Space
     (peg:choice (peg:character #\space)
                 (peg:character #\tab)
                 (peg:nonterminal EndOfLine)))
    (EndOfLine
     (peg:choice (peg:literal "\r\n")
                 (peg:literal "\n")
                 (peg:literal "\r")))
    (EndOfFile
     (peg:not (peg:dot)))
    )))
