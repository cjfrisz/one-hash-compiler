;;----------------------------------------------------------------------
;; File linker.ss
;; Written by Chris Frisz
;; 
;; Created 20 Feb 2012
;; Last modified 20 Feb 2012
;; 
;; Linker for the 1# compiler.
;;----------------------------------------------------------------------

(library (compiler linker)
  (export link)
   (import
     (rnrs) (chezscheme) (util match))

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
;;      Program ::= (code Expr+)
;;      Expr+   ::= (snoc-one [register])
;;                  (snoc-hash [register])
;;                  (jump [label])
;;                  (case-on [register])
;;                  [label]
;;
;; Output grammar:
;;      Program ::= (code Expr+)
;;      Expr+   ::= (snoc-one [register])
;;                  (snoc-hash [register])
;;                  (jump-fwd [int])
;;                  (jump-bwd [int])
;;                  (case-on [register])
;;--------------------------------------------------
(define (link code))
  
)