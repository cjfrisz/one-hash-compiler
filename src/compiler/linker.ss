;;----------------------------------------------------------------------
;; File linker.ss
;; Written by Chris Frisz
;; 
;; Created 20 Feb 2012
;; Last modified 12 Mar 2012
;; 
;; Linker for the 1# compiler.
;;----------------------------------------------------------------------

(library (compiler linker)
  (export link)
   (import
     (rnrs) (chezscheme))

(define-record-type Linker-code
  (fields
    (mutable Expr+)))

(define-record-type Linker-expr
  (fields
    (immutable opnd)))

(define-record-type Snoc-one (parent Linker-expr))
(define-record-type Snoc-hash (parent Linker-expr))
(define-record-type Jump (parent Linker-expr))
(define-record-type Case-on (parent Linker-expr))
(define-record-type Label (parent Linker-expr))

;;--------------------------------------------------
;; Procedure:
;;      link
;;
;; Description:
;;      Takes a block with of code with labels and jumps to labels and
;;      returns the code with the labels removed and label jumps replaced
;;      with absolute jumps.
;;
;; Input grammar:
;;      Program ::= (Code Expr+)
;;      Expr    ::= (Snoc-one [register])
;;                  (Snoc-hash [register])
;;                  (Jump [label])
;;                  (Case-on [register])
;;                  [label]
;;
;; Output grammar:
;;      Program ::= (Code Expr+)
;;      Expr    ::= (Snoc-one [register])
;;                  (Snoc-hash [register])
;;                  (Jump-fwd [int])
;;                  (Jump-bwd [int])
;;                  (Case-on [register])
;;--------------------------------------------------
(library (compiler linker)

  (export linker)

  (import
    (compiler lang-forms))

(define (gather-labels code)
  (cond
    [(Code? code)
     (let loop ([expr* (Code-expr* code)]
                [line-num 0])
       (cond
         [(null? expr*) '()]
         [(Label? (car expr*))
          (cons `(,(car expr*) . ,line-num)
                (loop (cdr expr*) (add1 line-num)))]
         [else (loop (cdr expr*) (add1 line-num))]))]
    [else (error "input is note Code record")]))

)

  

