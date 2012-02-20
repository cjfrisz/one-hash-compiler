;;----------------------------------------------------------------------
;; File one-hash-runtime.ss
;; Written by Chris Frisz
;; 
;; Created 13 Jan 2012
;; Last modified 19 Feb 2012
;; 
;; The file one-hash-runtime.ss defines the one-hash-runtime library containing an
;; interpreter for the 1# language.
;;----------------------------------------------------------------------

(library (runtime one-hash-runtime)

  (export run)

  ;; Testing export
  (export
    registers clear-all-registers register-set! register-ref)

  (import (chezscheme))

(define registers
  (make-parameter '()))

(define (clear-registers)
  (registers '()))

(define (extend-registers new-len)
  (when (< (length (registers)) new-len)
    (let ([length-diff (- new-len (length (registers)))])
      (let ([ext (make-list length-diff '())])
        (registers (append (registers) ext))))))

(define (register-set! rnum val)
  (begin
    ;; In case we don't have enough registers
    (extend-registers rnum)
    (set-car! (list-tail (registers) (sub1 rnum)) val)))

(define (register-enqueue val rnum)
  (register-set! rnum (cons val (register-ref rnum))))

(define (register-dequeue rnum)
  (begin
    (extend-registers rnum)
    (let ([reg (register-ref rnum)])
      (if (null? reg)
          '()
          (let ([flip-reg (reverse reg)])
            (let ([head (car flip-reg)])
              (begin
                (register-set! rnum (reverse (cdr flip-reg)))
                head)))))))

(define (register-ref rnum)
  (begin
    (extend-registers rnum)
    (list-ref (registers) (sub1 rnum))))

(define (get-instr prog idx)
  (list-ref prog idx))

(define (get-instr-n instr)
  (car instr))

(define (get-instr-inum instr)
  (cdr instr))

(define (exec idx prog)
  (cond
    [(= idx (length prog)) (registers)]
    [(or (< idx 0) (> idx (length prog))) 'improper-halt]
    [else 
      (let ([instr (get-instr prog idx)])
        (let ([n (get-instr-n instr)]
              [inum (get-instr-inum instr)])
          (case inum
              [(1) (begin
                     (register-enqueue 1 n)
                     (exec (add1 idx) prog))]
              [(2) (begin
                     (register-enqueue 0 n)
                     (exec (add1 idx) prog))]
              [(3) (exec (+ idx n) prog)]
              [(4) (exec (- idx n) prog)]
              [(5) (let ([reg-head (register-dequeue n)])
                     (let ([off (or (and (null? reg-head) 1)
                                    (and (= reg-head 1) 2)
                                    3)])
                       (exec (+ idx off) prog)))])))]))

(define one-raw #\1)
(define hash-raw #\#)

(define (parse ip)
  (let ([prog (string->list ip)])
    (let loop ([prog prog])
      (if (null? prog)
          '()
          (let ([fst (car prog)] [rst (cdr prog)])
            (cond
              [(char=? fst #\1) (cons 1 (loop rst))]
              [(char=? fst #\#) (cons 0 (loop rst))]
              [(char=? fst #\space) (loop rst)]
              [(char=? fst #\newline) (loop rst)]
              [else (errorf 'parse "Invalid symbol ~s" fst)]))))))

(define one 1)
(define hash 0)

(define (tokenize whole-prog)
  (let loop ([n 0] [inum 0] [prog whole-prog])
    (if (null? prog)
        '()
        (let ([fst (car prog)] [rst (cdr prog)])
          (cond
            [(and (null? rst) (= fst one))
             (errorf "Invalid program ~s" whole-prog)]
            [(or (null? rst) (not (eq? fst (car rst))))
             (cons `(,n . ,(add1 inum)) (loop 0 0 rst))]
            [(= fst one) (loop (add1 n) inum rst)]
            [else (loop n (add1 inum) rst)])))))
  
)
