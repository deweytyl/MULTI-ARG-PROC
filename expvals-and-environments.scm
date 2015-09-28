#lang eopl

;;; Expressed values and environments for DYNAMIC-PROC

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; stone@cs.grinnell.edu

;;; Fiona Byrne & Tyler Dewey
;;; Certified killing it
;;; 14 September 2015

;;; created February 5, 2009
;;; last revised September 13, 2015

;;; This module defines a data type
;;; for expressed values of the PROC programming language,
;;; as described in section 3.2 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; It also defines simple environments,
;;; as described in section 2.2 of that book.
;;; The two datatypes are presented together
;;; because they are mutually recursive.

(require "../natural-numbers.scm")
(require "../list-of.scm")
(require "syntax-trees.scm")

; ======= Expressed values ==================================================

;;; An expressed value in PROC
;;; is either an exact integer, a Boolean,
;;; or a value of the proc (i.e., closure)
;;; data type defined below.

(define-datatype expval expval?
  (num-val (num exact-integer?))
  (bool-val (bool boolean?))
  (proc-val (proc proc?)))

;;; We supplement the data type interface
;;; with projection functions that recover the values
;;; stored in the respective fields of the variants.

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (ev)
    (cases expval ev
      (num-val (num) num)
      (bool-val (bool)
        (report-domain-error 'expval->num "boolean" ev))
      (proc-val (proc)
        (report-domain-error 'expval->num "procedure" ev)))))

;; report-domain-error : Symbol * String * ExpVal -> (aborts the computation)
(define report-domain-error
  (lambda (location bad-type bad-ev)
    (eopl:error location
                "undefined for expressed ~a value ~s~%"
                bad-type
                bad-ev)))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (ev)
    (cases expval ev
      (num-val (num)
        (report-domain-error 'expval->bool "numeric" ev))
      (bool-val (bool) bool)
      (proc-val (proc)
        (report-domain-error 'expval->bool "procedure" ev)))))

;; expval->bool : Expval -> Proc
(define expval->proc
  (lambda (ev)
    (cases expval ev
      (num-val (num)
        (report-domain-error 'expval->proc "numeric" ev))
      (bool-val (bool)
        (report-domain-error 'expval->proc "boolean" ev))
      (proc-val (proc) proc))))

; ======= Closures ==========================================================

;; The identifiers used in this data type definition
;; differ slightly from those used in Friedman and Wand's book,
;; to avoid conflicts with standard Scheme's built-in procedure? procedure
;; and the identifier? procedure built into some languages under Racket.

(define-datatype proc proc?
  (a-proc (parameters (list-of symbol?))
          (body expression?)
          (saved-env environment?)))

; ======= Environments ====================================================

;;; An environment is either empty or extends another environment
;;; by adding one new variable,
;;; to which some denoted value is bound.
;;; In the PROC language
;;; that Friedman and Wand introduce
;;; in section 3.3 of _Essentials of programming languages_,
;;; denoted values and expressed values are the same,
;;; so we'll use values of the expval data type in this role.

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (val expval?)
    (saved environment?)))

(define extend-env* 
  (lambda (vars vals saved)
    (if (= (length vars) (length vals))
        (if (null? vars)
            saved
            (extend-env (car vars) (car vals)
                        (extend-env* (cdr vars) (cdr vals) saved)))
        (eopl:error 'extend-env*
                    "vars and vals must be of equal length~%"))))

;;; The apply-env procedure looks up a given variable
;;; in a given environment
;;; and returns the denoted value bound to it.
;;; It is an error to apply apply-env
;;; to a variable that is not bound in the given environment.

;; apply-env : Env * Sym -> ExpVal
(define apply-env
  (lambda (env sought)
    (let kernel ((remaining env))
      (cases environment remaining
        (empty-env ()
          (report-no-binding-found sought env))
        (extend-env (var val saved)
          (if (eqv? var sought)
              val
              (kernel saved)))))))

(define report-no-binding-found
  (lambda (sought env)
    (eopl:error 'apply-env
                "No binding for ~s was found in environment ~s.~%"
                sought
                env)))

;;; PROC programs are evaluated
;;; in an initial environment
;;; containing bindings for a few Roman numerals.
;;; The init-env procedure constructs and returns this environment.

;;; This code is taken
;;; from section 3.2 of _Essentials of programming languages_.

;; init-env : () -> Env
(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
      (extend-env 'v (num-val 5)
        (extend-env 'x (num-val 10) (empty-env))))))

(provide expval expval? num-val bool-val proc-val expval->num expval->bool
    expval->proc proc proc? a-proc environment environment? empty-env
    extend-env extend-env* apply-env init-env)

;;; The definition of the init-env procedure
;;; is due to Daniel P. Friedman (dfried@cs.indiana.edu)
;;; and Mitchell Wand (wand@ccs.neu.edu),
;;; who made it available as part of the Git repository
;;; at https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license
;;; (http://creativecommons.org/licenses/by-nc/3.0/).

;;; The remaining definitions are
;;; copyright (C) 2009, 2015 by John David Stone
;;; and are similarly released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.
