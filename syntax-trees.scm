#lang eopl

;;; Syntax trees for DYNAMIC-PROC

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

;;; created February 3, 2009
;;; last revised October 5, 2015

;;; This module defines a data type
;;; for abstract syntax trees of the PROC programming language,
;;; as described in section 3.2 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(require "../natural-numbers.scm")
(require "../list-of.scm")

;;; The grammar for PROC is as follows:

;;;       <program> ::= <expression>
;;;    <expression> ::= <numeral>
;;;                   | - ( <expression> , <expression> )
;;;                   | zero? ( <expression> )
;;;                   | if <expression> then <expression> else <expression>
;;;                   | <identifier>
;;;                   | let <identifier> = <expression> in <expression>
;;;                   | proc ([<identifier>{, <identifiers>}* {, (<identifier> <expression>)}*]) <expression>
;;;                   | ( <expression> [<expression> {, <expression>}*])

;;; The data type definitions exactly reflect this grammar.

(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype expression expression?
  (const-exp (datum exact-integer?))
  (diff-exp (minuend expression?)
            (subtrahend expression?))
  (zero?-exp (testee expression?))
  (if-exp (condition expression?)
          (consequent expression?)
          (alternative expression?))
  (var-exp (id symbol?))
  (let-exp (bound-var symbol?)
           (bound-value expression?)
           (body expression?))
  (proc-exp (parameters (list-of symbol?))
            (optional-parameters (list-of symbol-expression-pair?))
            (body expression?))
  (call-exp (operator expression?)
            (operands (list-of expression?))))

(define symbol-expression-pair?
 (lambda (something)
   (and (pair? something)
        (symbol? (car something))
        (expression? (cdr something)))))
            
(provide program program? a-program expression expression? const-exp diff-exp
         zero?-exp if-exp var-exp let-exp proc-exp call-exp symbol-expression-pair?)

;;; copyright (C) 2009, 2015 John David Stone

;;; This program is free software.
;;; You may redistribute it and/or modify it
;;; under the terms of the GNU General Public License
;;; as published by the Free Software Foundation --
;;; either version 3 of the License,
;;; or (at your option) any later version.
;;; A copy of the GNU General Public License
;;; is available on the World Wide Web at
;;;
;;;                http://www.gnu.org/licenses/gpl.html

;;; This program is distributed
;;; in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY --
;;; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
