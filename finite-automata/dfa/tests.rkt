#lang racket


(require "core.rkt"
         "image-builder.rkt"
         "table-minimization.rkt"
         "../../utils/dot.rkt")


(define T
  (dfa A (B C) (A : 0 -> C)
       (A : 1 -> B)
       (B : 0 -> D)
       (B : 1 -> A)
       (C : 1 -> D)
       (C : 0 -> A)
       (D : 0 -> B)
       (D : 1 -> C)))

(dfa->pict T)
(dfa->pict (minimize T))
(dfa->pict (minimization T))
