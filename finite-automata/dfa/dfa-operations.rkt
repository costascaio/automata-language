#lang racket

(require "core.rkt"
         "../dfa/image-builder.rkt"
         "../fa.rkt"
         "image-builder.rkt")

;"subset-construction.rkt"

(define M
  (dfa s1 (s2) (s1 : 0 -> s2)
             (s2 : 1 -> s2)
             (s2 : 0 -> s2)))


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

; convert a list of symbols to string
(define (convert-symbol-string list-symbols)
  (apply string-append
         (map symbol->string list-symbols)))

; convert a list of string to symbol
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
;;; (dfa-delta M)

;;; (define (concat-states states1 states2)
  
;;;   )

; generate a list of all transitions that starts with a state
(define (find-state-transitions state list-delta)
    (filter (lambda (transition)
           (eq? state (car (car transition)))) list-delta))

(find-state-transitions (dfa-start M) (dfa-delta M))
(find-state-transitions (dfa-start N) (dfa-delta N))

(define transition-start-dfa1 (find-state-transitions (dfa-start M) (dfa-delta M)))
(define transition-start-dfa2 (find-state-transitions (dfa-start N) (dfa-delta N)))

(list? transition-start-dfa1)

(cdr (car (first transition-start-dfa1)))

