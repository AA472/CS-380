#lang eopl

;Abdullah Aljandali
;CS 380 HW1
; 1/19/2019

(provide (all-defined-out))

;define pi
(define pi ( * 4 ( atan 1.0)))

;receives an angle in degrees and returns it in radians
(define (degrees->radians d)
  ( * pi (/ d 180 ) ) )

;recieves speed and time, and returns distance
(define (distance v t)
  (* v t))

;receives vertical velocity
;returns the time it takes for
;the object to reach the ground
(define (time-to-ground vy)
  ( / vy 4.9))

;returns the horizantal distance
;moved by a thrown ball
(define (ball-distance v theta)
  (let* ([theta1 (degrees->radians theta)]
        [vx (* v (cos theta1))]
        [vy (* v (sin theta1))]
        [time (time-to-ground vy)]
        )
  (distance time vx)))

         
;Problem 1.15
;makes a list of n x's
(define (duple n x)
  (cond ( [zero? n] '())
        ( #t (cons x (duple (- n 1) x)))))
;Problem 1.17
(define (down lst)
  (cond ( [null? lst] '())
        (#t (cons (cons (car lst) '()) (down (cdr lst)) )) ) )
;Problem 1.19
;changes the nth symbol of the list to x
(define (list-set lst n x)
  (cond ([null? lst] '())
        ([zero? n] (cons x (list-set (cdr lst) (- n 1) x)))
        ( #t (cons (car lst) (list-set (cdr lst) (- n 1) x)))))