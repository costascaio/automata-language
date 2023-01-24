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


; convert a list of symbols to string
(define (convert-symbol-string list-symbols)
  (map symbol->string list-symbols))

; convert a list of string to symbol
(define (convert-string-symbol list-string)
  (map string->symbol list-string))

; generate the combination of all possible states using strings
(define (combination-states states1 states2)
  (define str-states1 (convert-symbol-string states1))
  (define str-states2 (convert-symbol-string states2))
  (apply append (map (lambda (a) 
    (map (lambda (b) 
      (string-append a b)) str-states1)) str-states2)))

; generate a list with the combination of states from two dfa
(define (list-states dfa1 dfa2)
  (define states-combination 
    (combination-states 
      (dfa-states dfa1) (dfa-states dfa2)))
  (convert-string-symbol states-combination))

(list-states M N)
;;; (dfa-delta M)

;;; (define (concat-states states1 states2)
  
;;;   )

; generate a list of all transitions that starts with a state
(define (find-state-transitions state list-delta)
    (filter (lambda (transition)
           (eq? state (car (car transition)))) list-delta))

