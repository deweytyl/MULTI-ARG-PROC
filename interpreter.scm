#lang eopl

;;; An interpreter for the DYNAMIC-PROC language

;;; Daniel P. Friedman (dfried@cs.indiana.edu)
;;; Mitchell Wand (wand@ccs.neu.edu)
;;; From _Essentials of programming languages_, third edition (Cambridge,
;;;     Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4).

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; stone@cs.grinnell.edu

;;; Fiona Byrne & Tyler Dewey
;;; Certified killing it
;;; 14 September 2015

;;; Tyler Dewey
;;; Modified for multi-argument procs
;;; with default values
;;; October 3, 2015

;;; last revised October 5, 2015

(require "expvals-and-environments.scm")
(require "parser.scm")
(require "syntax-trees.scm")

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (- num1 num2)))))
      (zero?-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))
      (let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body (extend-env var val1 env))))
      (proc-exp (standard-vars optional-var-pairs body)
        (let* ((optional-vars (map car optional-var-pairs))
               (optional-var-values (map (lambda (p) (value-of (cdr p) env))
                                         optional-var-pairs))
               (vars (append standard-vars optional-vars)))
        (proc-val (a-proc vars
                          (length standard-vars)
                          body
                          (extend-env* optional-vars optional-var-values env)))))
      (call-exp (operator operands)
        (let ((proc (expval->proc (value-of operator env)))
              (args (map (lambda (operand) (value-of operand env))
                         operands)))
          (apply-procedure proc args env))))))

;;; The apply-procedure procedure
;;; applies the procedure represented by a given proc
;;; to a given value
;;; by evaluating the proc's body in an environment
;;; obtained by adding the binding for the proc's parameter
;;; to its stored environment.

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (applicand arguments env)
    (cases proc applicand
      (a-proc (parameters min-arity body saved-env)
        (let ((num-args (length arguments)))
          (if (>= num-args min-arity)
              (value-of body (extend-env* parameters 
                                          arguments 
                                          saved-env))
              (report-arity-error min-arity num-args)))))))

;; report-apply-procedure-error : String -> ()
(define report-arity-error
  (lambda (expected found)
    (eopl:error 'apply-procedure
                "arity mismatch: expected at least ~a arguments, found ~a.~%"
                expected
                found)))

;;; Tests

(require "../test.scm")
(require "../character-sources.scm")
(require "scanner.scm")

;;; Constants

(test 0 (equal? (value-of (const-exp 0) (empty-env))
                (num-val 0)))
(test 1 (equal? (value-of-program (a-program (const-exp 0)))
                (num-val 0)))
(test 2 (equal? (run "0") (num-val 0)))

;;; Variables

(test 3 (equal? (value-of (var-exp 'alpha)
                          (extend-env 'alpha (num-val 1) (empty-env)))
                (num-val 1)))
(test 4 (equal? (value-of-program (a-program (var-exp 'i)))
                (num-val 1)))
(test 5 (equal? (run "v") (num-val 5)))

;;; Diff-expressions

(test 6 (equal? (value-of (diff-exp (const-exp 2) (const-exp 3))
                          (empty-env))
                (num-val -1)))
(test 7 (equal? (value-of-program
                  (a-program (diff-exp (var-exp 'x) (const-exp 4))))
                (num-val 6)))
(test 8 (equal? (run "-(i, 5)") (num-val -4)))

;;; Zero?-expressions

(test 9 (equal? (value-of (zero?-exp (const-exp 0)) (empty-env))
                (bool-val #t)))
(test 10 (equal? (value-of (zero?-exp (const-exp 6)) (empty-env))
                 (bool-val #f)))
(test 11 (equal? (value-of-program
                   (a-program (zero?-exp (const-exp 0))))
                 (bool-val #t)))
(test 12 (equal? (value-of-program
                   (a-program (zero?-exp (const-exp 6))))
                 (bool-val #f)))
(test 13 (equal? (run "zero?(0)") (bool-val #t)))
(test 14 (equal? (run "zero?(6)") (bool-val #f)))

;;; If-expressions

(test 15 (equal? (value-of (if-exp (zero?-exp (const-exp 0))
                                   (const-exp 7)
                                   (const-exp 8))
                           (empty-env))
                 (num-val 7)))
(test 16 (equal? (value-of (if-exp (zero?-exp (const-exp 9))
                                   (const-exp 10)
                                   (const-exp 11))
                           (empty-env))
                 (num-val 11)))
(test 17 (equal? (value-of-program
                   (a-program (if-exp (zero?-exp (const-exp 0))
                                      (const-exp 7)
                                      (const-exp 8))))
                 (num-val 7)))
(test 18 (equal? (value-of-program
                   (a-program (if-exp (zero?-exp (const-exp 9))
                                      (const-exp 10)
                                      (const-exp 11))))
                 (num-val 11)))
(test 19 (equal? (run "if zero?(0) then 7 else 8") (num-val 7)))
(test 20 (equal? (run "if zero?(9) then 10 else 11") (num-val 11)))

;;; Let-expressions

(test 21 (equal? (value-of (let-exp 'alpha (const-exp 12) (var-exp 'alpha))
                           (empty-env))
                 (num-val 12)))
(test 22 (equal? (value-of-program
                   (a-program (let-exp 'alpha
                                       (const-exp 12)
                                       (var-exp 'alpha))))
                 (num-val 12)))
(test 23 (equal? (run "let alpha = 12 in alpha") (num-val 12)))

;;; Proc-expressions

(test 24 (equal? (value-of (proc-exp '(pi) '() (const-exp 14)) (empty-env))
                 (proc-val (a-proc '(pi) 1 (const-exp 14) (empty-env)))))
(test 25 (equal? (value-of-program
                   (a-program (proc-exp '(rho) '() (var-exp 'rho))))
                   (proc-val (a-proc '(rho) 1 (var-exp 'rho) (init-env)))))
(test 26 (equal? (run "proc (sigma) 15")
                 (proc-val (a-proc '(sigma) 1 (const-exp 15) (init-env)))))

;;; Call-expressions

(test 27 (equal? (value-of
                   (call-exp (proc-exp '(tau) '() (diff-exp (var-exp 'tau)
                                                      (const-exp 16)))
                             (list (const-exp 17)))
                   (empty-env))
                 (num-val 1)))
(test 28 (equal? (value-of-program
                   (a-program
                     (call-exp (proc-exp '(upsilon)
                                         '()
                                         (zero?-exp (var-exp 'upsilon)))
                               (list (var-exp 'x)))))
                 (bool-val #f)))
(test 29 (equal? (run "(proc (phi) -(0, phi) 18)")
                 (num-val -18)))

;;; Programs from the textbook

(test 30 (equal? (run "-(55, -(x, 11))") (num-val 56)))
(test 31 (equal? (run "-(-(x, 3), -(v, i))") (num-val 3)))
(test 32 (equal? (value-of
                   (parse-expression
                     (scanner
                      (make-character-source
                        "if zero?(-(x, 11)) then -(y, 2) else -(y, 4)")))
                   (extend-env 'x (num-val 33)
                               (extend-env 'y (num-val 22) (empty-env))))
                 (num-val 18)))
(test 33 (equal? (run "let x = 5 in -(x, 3)") (num-val 2)))
(test 34 (equal? (run "let z = 5
                       in let x = 3
                          in let y = -(x, 1)      % here x = 3
                             in let x = 4
                                in -(z, -(x, y))  % here x = 4")
                 (num-val 3)))
(test 35 (equal? (run "let x = 7
                       in let y = 2
                          in let y = let x = -(x, 1)
                                     in -(x, y)
                             in -(-(x, 8), y)")
                 (num-val -5)))
(test 36 (equal? (run "let f = proc (x) -(x, 11)
                       in (f (f 77))")
                 (num-val 55)))
(test 37 (equal? (run "(proc (f) (f (f 77))
                        proc (x) -(x, 11))")
                 (num-val 55)))
(test 38 (equal? (run "let free = 31
                       in let addfree = proc (augend) -(augend, -(0, free))
                          in let free = 53
                             in (addfree free)")
                 (num-val 84)))

;; The following expression should signal errors:

;; (run "-(zero?(0), 5)")
;; (run "-(5, zero?(0))")
;; (run "zero?(zero?(0))")
;; (run "if 0 then 1 else 2")
;; (run "(35 42)")
;; (run "(zero?(0) 42)")
;; (run "zero?(proc (x) x)")

;;; The procedure definitions are the work of Friedman and Wand,
;;; who published them on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; The test cases are
;;; copyright (C) 2009, 2015 by John David Stone
;;; and are similarly released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.
