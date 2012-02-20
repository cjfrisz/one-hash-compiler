;;----------------------------------------------------------------------
;; File one-hash-runtime.ss
;; Written by Chris Frisz
;; 
;; Created 13 Jan 2012
;; Last modified 20 Feb 2012
;; 
;; The file one-hash-runtime.ss defines the one-hash-runtime library
;; containing an interpreter for the 1# language.
;;----------------------------------------------------------------------

(library (runtime one-hash-runtime)

  (export exec)

  (import (chezscheme))

;;----------------------------------------
;; Record type:
;;      trm
;; Description:
;;    Maintains the state for a 1# program
;; Fields:
;;      instr*: The instruction list for the program
;;      plen: The number of instructions in the program
;;      pc: The program counter (current instruction) of the program
;;      reg*: The register set used by the program.
;;----------------------------------------
(define-record-type
  trm
  (fields
    (immutable instr*)
    (immutable plen)
    (mutable pc)
    (mutable reg*))
  (protocol
    (lambda (new)
      (lambda (i*)
        (new i* (length i*) 0 '())))))

;;----------------------------------------
;; Representation for the tokens in the 1# language
;;----------------------------------------
(define one 1)
(define hash '\x23;
  )


;;----------------------------------------
;; Retrieves a register from a register set as used by the trm
;; record. If the record to retrieve represents one outside the
;; current set, the registers are extended until that register is
;; available.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (get-register reg* rnum)
  (begin
    (when (< (length reg*) rnum)
      (let* ([diff (- rnum (length reg*))]
             [ext (make-list diff '())])
        (set! reg* (append reg* ext))))
    (list-ref reg* rnum)))

;;----------------------------------------
;; Generalized function for enqueuing an object onto a register.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (register-enqueue reg* rnum val)
  (let ([reg (get-register reg* rnum)])
    (set! reg `(,@reg ,val))))

;;----------------------------------------
;; Enqueue a one onto a register.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (register-enqueue-one reg* rnum)
  (register-enqueue rnum reg* one))

;;----------------------------------------
;; Enqueue a hash onto a register.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (register-enqueue-hash reg* rnum)
  (register-enqueue rnum reg* hash))

;;----------------------------------------
;; Retrieve the value from the front of a register. If the register is
;; empty, returns the null list.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (register-dequeue reg* rnum)
  (let ([reg (get-register reg* rnum)])
    (if (not (null? reg))
        (let ([val (car reg)])
          (begin
            (set! reg (cdr reg))
            val))
        '())))

;;----------------------------------------
;; Accessor for instructions from the instr* field of the trm record
;; type.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (get-instr-num instr* inum) (list-ref instr* inum))

;;----------------------------------------
;; Accessors for the instruction representation for the count and
;; instruction type, respectively.
;;----------------------------------------
(define (instr-get-cnt instr) (car instr))
(define (instr-get-type instr) (cdr instr))

;;----------------------------------------
;; Procedures for moving the program counter of the program
;; represented by a trm record.
;;
;; Used for the transfer-forward and transfer-backward instructions.
;;----------------------------------------
(define (pc-move-fwd pc amnt) (set! pc (+ pc amnt)))
(define (pc-move-bwd pc amnt) (set! pc (- pc amnt)))
(define (pc-inc pc) (pc-move-fwd pc 1))

;;----------------------------------------
;; Implementation of the case instruction.
;;----------------------------------------
(define (case-on-reg pc reg* rnum)
  (let ([reg (get-register reg* rnum)])
    (let ([val (register-dequeue reg* rnum)])
      (cond
        [(null? val) (pc-inc pc)]
        [(eq? val one) (pc-move-fwd pc 2)]
        [(eq? val hash) (pc-move-fwd pc 3)]
        [else (error 'case-on-reg
                "Invalid value in register: ~s"
                val)]))))

;;----------------------------------------
;; Prune empty registers from the end of the register set.
;;
;; Used as a convenience function for cleaning up registers on return
;; from exec.
;;----------------------------------------
(define (prune-empty-regs reg*)
  (let loop ([reg* reg*] [tail '()])
    (cond
      [(null? reg*) '()]
      [(null? (car reg*)) (loop (cdr reg*) (cons '() tail))]
      [else (append tail (list (car reg*)) (loop (cdr reg*) '()))])))

;;----------------------------------------
;; Pretty-print the registers
;;----------------------------------------
(define (pretty-print-regs reg*)
  (let loop ([reg* reg*] [rnum 1])
    (unless (null? reg*)
      (begin
        (printf "R~s: ~s~n" rnum (car reg*))
        (loop (cdr reg*) (add1 rnum))))))

;;----------------------------------------
;; Format the output from the interpreter
;;----------------------------------------
(define (format-output reg*)
  (let ([pruned (prune-empty-regs reg*)])
    (begin
      (printf "Register values:~n")
      (printf "-------------------------~n")
      (pretty-print-regs reg*))))

;;----------------------------------------
;; Executes the program represented by a trm record.
;;
;; Returns the value of the registers on halting of the program.
;;----------------------------------------
(define (exec instr*)
  (let loop ([prog (make-trm instr*)])
    (cond
      [(= (trm-pc prog) (trm-plen prog)) (format-output (trm-reg* prog))]
      [(or (< (trm-pc prog) 0) (> (trm-pc prog) (trm-plen prog)))
       (errorf 'exec
         "Program halted improperly:\npc: ~s\nregisters: ~s"
         (trm-pc prog)
         (trm-reg* prog))]
      [else
        (let ([instr (get-instr-num (trm-instr* prog) (trm-pc prog))])
          (let ([cnt (instr-get-cnt instr)]
                [type (instr-get-type instr)])
            (begin
              (case type
                [(1) (register-enqueue-one (trm-reg* prog) cnt)]
                [(2) (register-enqueue-hash (trm-reg* prog) cnt)]
                [(3) (pc-move-fwd (trm-pc prog) cnt)]
                [(4) (pc-move-bwd (trm-pc prog) cnt)]
                [(5) (case-on-reg (trm-pc prog) (trm-reg* prog) cnt)])
              (loop prog))))])))
  
)
