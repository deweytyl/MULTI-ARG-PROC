#lang eopl

;;; Tokens in PROC

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; stone@cs.grinnell.edu

;;; created January 31, 2009
;;; last revised September 13, 2015

;;; This module defines a data type
;;; for lexical tokens of the PROC programming language,
;;; as described in section 3.2 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(require "../natural-numbers.scm")

;;; PROC has tokens of thirteen kinds:
;;; ``numbers'' (or, actually, numerals),
;;; identifiers, minus signs, open- and close-parentheses,
;;; commas, equals signs,
;;; and the keywords zero?, if, then, else, let, in, and proc.
;;; A single define-datatype declaration
;;; accommodates them all as variants.

(define-datatype token token?
  (numeral-token
    (value exact-integer?))
  (minus-sign)
  (open-parenthesis)
  (comma)
  (close-parenthesis)
  (zero?-token)
  (if-token)
  (then-token)
  (else-token)
  (identifier-token
    (id symbol?))
  (let-token)
  (equals-sign)
  (in-token)
  (proc-token))

(provide token token? numeral-token minus-sign open-parenthesis
         comma close-parenthesis zero?-token if-token then-token
         else-token identifier-token let-token equals-sign in-token
         proc-token)

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
