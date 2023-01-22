#lang racket

(require "core.rkt"
         "../dfa/image-builder.rkt"
         "../fa.rkt"
         "image-builder.rkt")

;"subset-construction.rkt"

(define M
  (dfa A (B) (A : 0 -> B)
             (B : 1 -> B)
             (B : 0 -> B)))


(define N
  (dfa C (D) (C : 0 -> C)
             (C : 1 -> D)
             (D : 0 -> C)
             (D : 1 -> D)))

(dfa->pict N)

; append list of the states
(define (product-states dfa1 dfa2)
  (append (dfa-states dfa1) (dfa-states dfa2)))

; generate a list with the initial states of dfa
(define (product-start dfa1 dfa2)
  (list (dfa-start dfa1) (dfa-start dfa2)))


;(define (dfa-product dfa1 dfa2)
;  (define D (pair-initial dfa1 dfa2)))

; generate the combination of all possible states using strings
(define (combination-states str)
  (define combination-raw
    (map list->string (combinations(string->list str))))
  (filter (lambda (s) (= (string-length s) 2)) combination-raw)
  )

; convert a list os symbols to string
(define (convert-symbol-string list-symbols)
  (apply string-append
         (map symbol->string list-symbols)))

(define (convert-string-symbol list-string)
  (map string->symbol list-string))

; generate a list with the combination of states from two dfa
(define (list-states dfa1 dfa2)
  (define list-symbol-states (product-states dfa1 dfa2))
  (define list-string-states
    (convert-symbol-string list-symbol-states))
  (define states-combination (combination-states list-string-states))
  (convert-string-symbol states-combination))

(list-states M N)