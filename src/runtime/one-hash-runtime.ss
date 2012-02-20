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
        (new
          (if (pair? i*) (list->vector i*) i*)
          (length i*)
          0
          '())))))

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
(define (get-register prog rnum)
  (begin
      (when (< (length (trm-reg* prog)) rnum)
        (let* ([diff (- rnum (length (trm-reg* prog)))]
               [ext (make-list diff '())])
          (trm-reg*-set! prog (append (trm-reg* prog) ext))))
      (list-ref (trm-reg* prog) (sub1 rnum))))

(define (register-set! prog rnum val)
  (set-car! (list-tail (trm-reg* prog) (sub1 rnum)) val))

;;----------------------------------------
;; Generalized function for enqueuing an object onto a register.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (register-enqueue prog rnum val)
  (let ([reg (get-register reg* rnum)])
    (let ([reg^ `(,@reg ,val)])
      (register-set! prog rnum reg^))))

;;----------------------------------------
;; Enqueue a one onto a register.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (register-enqueue-one prog rnum)
  (register-enqueue prog rnum one))

;;----------------------------------------
;; Enqueue a hash onto a register.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (register-enqueue-hash prog rnum)
  (register-enqueue prog rnum hash))

;;----------------------------------------
;; Retrieve the value from the front of a register. If the register is
;; empty, returns the null list.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (register-dequeue prog rnum)
  (let ([reg (get-register prog rnum)])
    (if (not (null? reg))
        (let ([val (car reg)]
              [reg^ (cdr reg)])
          (begin
            (set! reg (cdr reg))
            (register-set! prog rnum reg^)
            val))
        '())))

;;----------------------------------------
;; Accessor for instructions from the instr* field of the trm record
;; type.
;;
;; The current implementation is zero-based.
;;----------------------------------------
(define (get-instr-num instr* inum) (vector-ref instr* inum))

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
(define (pc-move-fwd prog amnt)
  (let ([pc (trm-pc prog)])
    (trm-pc-set! prog (+ pc amnt))))

(define (pc-move-bwd prog amnt)
    (let ([pc (trm-pc prog)])
      (trm-pc-set! prog (- pc amnt))))

(define (pc-inc prog) (pc-move-fwd prog 1))

;;----------------------------------------
;; Implementation of the case instruction.
;;----------------------------------------
(define (case-on-reg prog rnum)
  (let ([reg (get-register prog rnum)])
    (let ([val (register-dequeue prog rnum)])
      (cond
        [(null? val) (pc-inc prog)]
        [(eq? val one) (pc-move-fwd prog 2)]
        [(eq? val hash) (pc-move-fwd prog 3)]
        [else (error 'case-on-reg
                "Invalid value in register: ~s"
                val)]))))

;;----------------------------------------
;; Prune empty registers from the end of the register set.
;;
;; Used as a convenience function for cleaning up registers on return
;; from exec.
;;----------------------------------------
(define (prune-empty-regs prog)
  (let loop ([reg* (trm-reg* prog)] [tail '()])
    (cond
      [(null? reg*) '()]
      [(null? (car reg*)) (loop (cdr reg*) (cons '() tail))]
      [else (append tail (list (car reg*)) (loop (cdr reg*) '()))])))

;;----------------------------------------
;; Pretty-print the registers
;;----------------------------------------
(define (pretty-print-regs prog)
  (let loop ([reg* (trm-reg* prog)] [rnum 1])
    (unless (null? reg*)
      (begin
        (printf "R~s: ~s~n" rnum (car reg*))
        (loop (cdr reg*) (add1 rnum))))))

;;----------------------------------------
;; Format the output from the interpreter
;;----------------------------------------
(define (format-output prog)
  (let ([pruned (prune-empty-regs prog)])
    (begin
      (printf "Register values:~n")
      (printf "-------------------------~n")
      (pretty-print-regs prog))))

(define (trm-proper-halt? prog)
  (= (trm-pc prog) (trm-plen prog)))

(define (trm-improper-halt? prog)
  (or (< (trm-pc prog) 0) (> (trm-pc prog) (trm-plen prog))))

;;----------------------------------------
;; Executes the program represented by a trm record based on an input
;; list of instructions.
;;
;; Returns the value of the registers on halting of the program.
;;----------------------------------------
(define (exec instr*)
  (let loop ([prog (make-trm instr*)])
    (cond
      [(trm-proper-halt? prog) (format-output (trm-reg* prog))]
      [(trm-improper-halt? prog)
       (errorf 'exec
         "Program halted improperly:\npc: ~s\nregisters: ~s"
         (trm-pc prog)
         (trm-reg* prog))]
      [else
        (let ([instr (get-instr-num prog)])
          (let ([cnt (instr-get-cnt instr)]
                [type (instr-get-type instr)])
            (begin
              (case type
                [(1) (begin
                       (register-enqueue-one trm cnt)
                       (pc-inc trm))]
                [(2) (begin
                       (register-enqueue-hash trm cnt)
                       (pc-inc trm))]
                [(3) (pc-move-fwd trm cnt)]
                [(4) (pc-move-bwd trm cnt)]
                [(5) (case-on-reg trm cnt)])
              (loop prog))))])))
  
)
