#lang eopl

;;; A scanner for the DYNAMIC-PROC language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; stone@cs.grinnell.edu

;;; Fiona Byrne & Tyler Dewey
;;; Certified killing it
;;; 14 September 2015

;;; created January 31, 2009
;;; last revised September 13, 2015

;;; This file provides a scanner
;;; for the PROC language
;;; developed by Daniel P. Friedman and Mitchell Wand
;;; in section 3.2 of their book
;;; _Essentials of programming languages_ (third edition).

(require "../character-sources.scm")
(require "tokens.scm")

;;; The scanner is implemented as a procedure
;;; that takes a character source as its argument
;;; and returns a ``token source'' object,
;;; which responds to the same three messages
;;; as a character source, but returns a token
;;; rather than a character
;;; in response to peek and get messages.

;;; When we first peek at a token,
;;; we'll have to get all of the characters that make it up
;;; from the character source.
;;; Since we can get those characters only once, however,
;;; we'll have to store the token
;;; so that we can still return it
;;; when the next peek or get message is received.
;;; The local variable named buffer
;;; holds the next available token,
;;; if we have peeked at it,
;;; or the symbol empty if we have not.

;; scanner : Character-source -> Token-source
(define scanner
  (lambda (character-source)
    (let ((buffer 'empty))
      (lambda (message)

        ;; When the token source that scanner returns
        ;; receives any message,
        ;; it first discards any whitespace
        ;; and any comments
        ;; that it finds in the given character source.
        ;; This ensures that the character source
        ;; is either "at end"
        ;; or positioned to yield a non-whitespace,
        ;; non-comment character.

        (skip-whitespace character-source)
        (case message

          ;; Then, if the message is at-end?,
          ;; the token source returns #t
          ;; if the buffer is empty
          ;; and the character source can supply
          ;; no more input.
          ;; Otherwise, another token may and should be available,
          ;; so the token source returns #f.

          ((at-end?) (and (eqv? buffer 'empty)
                          (character-source 'at-end?)))

          ;; If the message is peek,
          ;; the token source returns the token from the buffer,
          ;; filling it first if it starts out empty.

          ((peek) (when (eqv? buffer 'empty)
                        (set! buffer (get-token character-source)))
                  buffer)
 
         ;; If the message is get,
          ;; the token source returns the token from the buffer
          ;; if there is one
          ;; (emptying out the buffer along the way)
          ;; or the next token from the character source
          ;; if there is no token in the buffer.

           ((get) (if (eqv? buffer 'empty)
                     (get-token character-source)
                     (let ((result buffer))
                       (set! buffer 'empty)
                       result)))
          (else (report-bad-message-error message)))))))

;;; report-bad-message-error : Symbol -> (aborts the computation)
(define report-bad-message-error
  (lambda (non-message)
    (eopl:error 'token-source
                "The message ~s was not recognized.~%"
                non-message)))

;;; Comments in the PROC language
;;; begin with a percentage sign and
;;; end at the next following newline.

;; comment-starter : Char
(define comment-starter #\%)

;; comment-terminator : Char
(define comment-terminator #\newline)

;;; The discard-comment procedure is invoked
;;; when the beginning of a comment has been detected (and consumed).
;;; It consumes characters from the source
;;; up to and including a newline character
;;; (unless it reaches the end of the source first,
;;; in which case it consumes all of the remaining
;;; characters from the source).

;; discard-comment : Character-source -> ()
(define discard-comment
  (lambda (source)
    (let loop ()
      (unless (or (source 'at-end?)
                  (char=? (source 'get) comment-terminator))
        (loop)))))

;; The skip-whitespace procedure discards whitespace
;; and comments from the character source.
;; Its postcondition is that
;; either the character source
;; has no more available characters,
;; or the next available character
;; is a non-whitespace character
;; that is not part of a comment.

;; skip-whitespace : Character-source -> ()
(define skip-whitespace
  (lambda (source)
    (let loop ()
      (unless (source 'at-end?)
        (let ((next (source 'peek)))
          (cond ((char-whitespace? next)
                 (source 'get)
                 (loop))
                ((char=? next comment-starter)
                 (source 'get)
                 (discard-comment source)
                 (loop))))))))

;;; The get-token procedure consumes the text of one token
;;; from the character source
;;; and constructs and returns the appropriate token.
;;; It is a precondition of this procedure
;;; that the next available character can begin a token.

;; get-token : Character-source -> Token
(define get-token
  (lambda (source)
    (let ((next (source 'peek)))
      (cond ((char-numeric? next)
             (numeral-token (get-numeral-value source)))
            ((char-alphabetic? next)
             (let ((id (get-identifier source)))
               (case id
                 ((zero?) (zero?-token))
                 ((if) (if-token))
                 ((then) (then-token))
                 ((else) (else-token))
                 ((let) (let-token))
                 ((in) (in-token))
                 ((proc) (proc-token))
                 (else (identifier-token id)))))
            (else
             (source 'get)
             (case next
               ((#\-) (scan-after-minus source))
               ((#\() (open-parenthesis))
               ((#\,) (comma))
               ((#\)) (close-parenthesis))
               ((#\=) (equals-sign))
               (else
                (report-mislexical-error next))))))))

;;; report-mislexical-error : Char -> (aborts the computation)
(define report-mislexical-error
  (lambda (bad-character)
    (eopl:error 'scanner
                "A token may not begin with the ~c character.~%"
                bad-character)))

;;; As we collect the characters that make up a numeral,
;;; we'll need to compute its value.
;;; The digit-value procedure assists this process.
;;; It returns the natural number denoted by any given digit character
;;; (in ASCII; to make this work for Unicode would be much trickier).

;; digit-value : Char -> Int
(define digit-value
  (let ((zero-slot (char->integer #\0)))
    (lambda (digit)
      (- (char->integer digit) zero-slot))))

;;; The get-numeral-value procedure
;;; consumes an uninterrupted sequence of digit characters
;;; from the character source
;;; and computes and returns the value of the numeral
;;; that they compose.
;;; It is a precondition of this procedure
;;; that the next available character is a digit character.

;; get-numeral-value : Character-source -> Natural-number
(define get-numeral-value
  (lambda (source)
    (let loop ((value (digit-value (source 'get))))
      (if (or (source 'at-end?)
              (not (char-numeric? (source 'peek))))
          value
          (loop (+ (* value 10)
                   (digit-value (source 'get))))))))

;;; As we collect the characters that make up a keyword or an identifier,
;;; we'll need to know where to stop.
;;; The successor? predicate tests whether a given character
;;; can appear in a keyword or identifier
;;; (after the initial position, which must be a letter).
;;; The characters permitted here are letters, digits, and the question mark.

;; successor? : Char -> Bool
(define successor?
  (lambda (ch)
    (or (char-alphabetic? ch)
        (char-numeric? ch)
        (char=? ch #\?))))

;;; The get-identifier procedure consumes an uninterrupted sequence
;;; of letters, digits, and question marks
;;; from the character source,
;;; assembles them into a string,
;;; and constructs and returns the symbol
;;; named by that string.
;;; It is a precondition of this procedure
;;; that the next available character is a letter.

;; get-identifier : Character-source -> Sym
(define get-identifier
  (lambda (source)
    (let loop ((reversed-name (list (source 'get))))
      (if (or (source 'at-end?)
              (not (successor? (source 'peek))))
          (string->symbol (list->string (reverse reversed-name)))
          (loop (cons (source 'get) reversed-name))))))

;;; The scan-after-minus procedure determines whether
;;; a minus sign that has just been discovered (and consumed)
;;; is a subtraction operator
;;; or the beginning of a numeral,
;;; then consumes the rest of the token's text (if any)
;;; from the character source and returns the token.

;; scan-after-minus : Character-source -> Token
(define scan-after-minus
  (lambda (source)
     (if (source 'at-end?)
         (minus-sign)
         (let ((next (source 'peek)))
           (if (char-numeric? next)
               (numeral-token (- (get-numeral-value source)))
               (minus-sign))))))

(provide scanner)

;; ;;; Tests

;; (require "../test.scm")

;; ;; Tests for the digit-value procedure.

;; (test 0 (= (digit-value #\0) 0))
;; (test 1 (= (digit-value #\7) 7))

;; ;; Tests for the get-numeral-value procedure.

;; (test 2 (= (get-numeral-value (make-character-source "0")) 0))
;; (test 3 (= (get-numeral-value (make-character-source "365")) 365))
;; (test 4 (= (get-numeral-value (make-character-source "00001170")) 1170))
;; (test 5 (= (get-numeral-value (make-character-source "7foo")) 7))

;; ;; Tests for the successor? predicate.

;; (test 6 (eqv? (successor? #\j) #t))
;; (test 7 (eqv? (successor? #\3) #t))
;; (test 8 (eqv? (successor? #\?) #t))
;; (test 9 (eqv? (successor? #\-) #f))
;; (test 10 (eqv? (successor? #\space) #f))
;; (test 11 (eqv? (successor? #\newline) #f))

;; ;; Tests for the get-identifier procedure.

;; (test 12 (eqv? (get-identifier (make-character-source "a")) 'a))
;; (test 13 (eqv? (get-identifier (make-character-source "A")) 'A))
;; (test 14 (eqv? (get-identifier (make-character-source "a?")) 'a?))
;; (test 15 (eqv? (get-identifier (make-character-source "a3")) 'a3))
;; (test 16 (eqv? (get-identifier (make-character-source "a?3?4?")) 'a?3?4?))
;; (test 17 (eqv? (get-identifier (make-character-source "a(")) 'a))
;; (test 18 (eqv? (get-identifier (make-character-source "abcdefg "))
;;                'abcdefg))

;; ;; Tests for scan-after-minus.

;; (test 19 (equal? (scan-after-minus (make-character-source "(3, 7)"))
;;                  (minus-sign)))
;; (test 20 (equal? (scan-after-minus (make-character-source "268"))
;;                  (numeral-token -268)))
;; (test 21 (equal? (scan-after-minus (make-character-source "0bar"))
;;                  (numeral-token 0)))
;; (test 22 (equal? (scan-after-minus (make-character-source ""))
;;                  (minus-sign)))

;; ;; Tests for get-token.

;; (test 23 (equal? (get-token (make-character-source "0"))
;;                  (numeral-token 0)))
;; (test 24 (equal? (get-token (make-character-source "117"))
;;                  (numeral-token 117)))
;; (test 25 (equal? (get-token (make-character-source "alpha"))
;;                  (identifier-token 'alpha)))
;; (test 26 (equal? (get-token (make-character-source "zero"))
;;                  (identifier-token 'zero)))
;; (test 27 (equal? (get-token (make-character-source "zero?"))
;;                  (zero?-token)))
;; (test 28 (equal? (get-token (make-character-source "iffy"))
;;                  (identifier-token 'iffy)))
;; (test 29 (equal? (get-token (make-character-source "if "))
;;                  (if-token)))
;; (test 30 (equal? (get-token (make-character-source "if-"))
;;                  (if-token)))
;; (test 31 (equal? (get-token (make-character-source "then"))
;;                  (then-token)))
;; (test 32 (equal? (get-token (make-character-source "THEN"))
;;                  (identifier-token 'THEN)))
;; (test 33 (equal? (get-token (make-character-source "else"))
;;                  (else-token)))
;; (test 34 (equal? (get-token (make-character-source "Else"))
;;                  (identifier-token 'Else)))
;; (test 35 (equal? (get-token (make-character-source "let"))
;;                  (let-token)))
;; (test 36 (equal? (get-token (make-character-source "in"))
;;                  (in-token)))
;; (test 37 (equal? (get-token (make-character-source "-("))
;;                  (minus-sign)))
;; (test 38 (equal? (get-token (make-character-source "-4"))
;;                  (numeral-token -4)))
;; (test 39 (equal? (get-token (make-character-source "(foo"))
;;                  (open-parenthesis)))
;; (test 40 (equal? (get-token (make-character-source ","))
;;                  (comma)))
;; (test 41 (equal? (get-token (make-character-source ")"))
;;                  (close-parenthesis)))
;; (test 42 (equal? (get-token (make-character-source "="))
;;                  (equals-sign)))
;; (test 43 (equal? (get-token (make-character-source "proc"))
;;                  (proc-token)))

;; ;; Tests for the scanner.

;; ;; at-end? tests

;; (test 44 (eqv? ((scanner (make-character-source "")) 'at-end?) #t))
;; (test 45 (eqv? ((scanner (make-character-source "     \n\t\n \t "))
;;                'at-end?)
;;                #t))
;; (test 46 (eqv? ((scanner (make-character-source "foo")) 'at-end?) #f))
;; (test 47 (eqv? ((scanner (make-character-source "    foo  ")) 'at-end?) #f))

;; ;; peek tests

;; (test 48 (equal? ((scanner (make-character-source "362")) 'peek)
;;                  (numeral-token 362)))
;; (test 49 (equal? ((scanner (make-character-source "   IF   ")) 'peek)
;;                  (identifier-token 'IF)))

;; ;; get tests

;; (test 50 (equal? ((scanner (make-character-source "(foo bar))")) 'get)
;;                  (open-parenthesis)))
;; (test 51 (equal? ((scanner (make-character-source "  -3?")) 'get)
;;                  (numeral-token -3)))

;; ;; The following expressions should produce errors.

;; ;; ((scanner (make-character-source "")) 'get)
;; ;; ((scanner (make-character-source "")) 'peek)
;; ;; ((scanner (make-character-source "#")) 'get)
;; ;; ((scanner (make-character-source "% say no more\n")) 'get)
;; ;; ((scanner (make-character-source "%%%")) 'get)
;; ;; ((scanner (make-character-source "?")) 'get)
;; ;; ((scanner (make-character-source "+")) 'get)

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
