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
  (apply append (map (lambda (s1) 
    (map (lambda (s2)
      (string-append s1 s2)) str-states2)) str-states1)))

; generate a list with the combination of states from two dfa
(define (list-states dfa1 dfa2)
  (define states-combination 
    (combination-states 
      (dfa-states dfa1) (dfa-states dfa2)))
  (convert-string-symbol states-combination))


; generate a list of all transitions that starts with a state
(define (find-state-transitions state list-delta)
    (filter (lambda (transition)
           (eq? state (car (car transition)))) list-delta))

; generate a list of all transitions that has a espected symbol
(define (find-symbol-transitions symb list-delta)
    (filter (lambda (transition)
           (eq? symb (cdr (car transition)))) list-delta))

(find-state-transitions (dfa-start M) (dfa-delta M))
(find-state-transitions (dfa-start N) (dfa-delta N))

; generate a product of n transitions from different fa considering two differnt states
(define (product-transitions list-delta1 list-delta2 state1 state2 symb)
  (define s1 (find-state-transitions state1 list-delta1))
  (define s2 (find-state-transitions state2 list-delta2))
  (define t1 (find-symbol-transitions symb s1))
  (define t2 (find-symbol-transitions symb s2))
  (apply append (map (lambda (s1)
    (map (lambda (s2)
      (product-transition s1 s2 symb)) t2)) t1)))

; generate a product of two transitions of a symbol
(define (product-transition transition1 transition2 symb)
  (define origin-state 
    (string->symbol (string-append 
      (symbol->string (car (car transition1)))
      (symbol->string (car (car transition2))))))
  (define destiny-state 
    (string->symbol (string-append 
      (symbol->string (cdr transition1))
      (symbol->string (cdr transition2)))))
  (cons (cons origin-state symb) destiny-state))

(product-transitions (dfa-delta M) (dfa-delta N) (dfa-start M) (dfa-start N) (first (dfa-sigma M)))