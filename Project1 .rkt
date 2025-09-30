;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Project1 |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#lang racket
;; Reza Enayati
;; Racket Project 1 
; (rotate-left-1 x) 
(define (rotate-left-1 x)
  (cond
    ((empty? x) '())
    ((empty? (cdr x)) x)
    (else (append (cdr x ) (list (car x))))
    ))

; (rotate-left-n x n)
(define (rotate-left-n x n)
  (cond
    ((= n 0) x)
    (else (rotate-left-n (rotate-left-1 x) (- n 1)))
    ))

; (count items x)
(define (count-items x)
  (cond
    ((empty? x) 0)
    (else (+ 1 (count-items (cdr x))))
   ))

; (get-n x n)
(define (get-n x n)
  (cond
    ((empty? x) "Index Out of Range")
    ((= n 0) (car x))
    (else (get-n (cdr x) (- n 1)))
    ))

; (del-n x n)
(define (del-n x n)
  (cond
    ((empty? x) '())
    ((= n 0) (cdr x))
    (else (cons (car x) (del-n (cdr x) (- n 1))))))

; (rotate-right-1 x)
(define (rotate-right-1 x)
  (cond
    ((or (empty? x) (empty? (cdr x))) x)
    (else
     (cons (get-n x (- (count-items x) 1))
           (del-n x (- (count-items x) 1))))))

; (reverse-list x)
(define (reverse-list x)
  (cond
    ((or (empty? x) (empty? (cdr x))) x)
    (else (append (reverse-list (cdr x))
                  (list (car x))))))

; (cons-all a x)
(define (cons-all a x)
  (cond
    ((empty? x) '())
    (else (cons (cons a (car x))
                (cons-all a (cdr x)))))) 

;; perm
(define (perm x)
  (cond
    ((empty? x) '())                             
    ((empty? (cdr x)) (list x))                  
    ((empty? (cddr x)) (list x (reverse x)))     
    (else (ph-2 x (- (length x) 1)))))           

;; ph-1
(define (ph-1 x n)
  (cons-all (get-n x n) (perm (del-n x n))))

;; ph-2
(define (ph-2 x n)
  (cond
    ((< n 0) '())
    (else (append (ph-2 x (- n 1)) (ph-1 x n)))))

; -------- Test Cases -------
;(rotate-left-1 '())
;(rotate-left-1 '(a))
;(rotate-left-1 '(a b c))

;(rotate-left-n '(a b c) 0)
;(rotate-left-n '(a b c d e) 2)
;(rotate-left-n '(a b c d e) 5)

;(count-items '())
;(count-items '(a))
;(count-items '(a b c d e))

;(get-n '(a b c d e) 0)
;(get-n '(a b c d e) 4)
;(get-n '(a b c d e) 1)

;(del-n '(a b c d e) 0)
;(del-n '(a b c d e) 1)
;(del-n '(a b c d e) 2)
;(del-n '(a b c d e) 4)

;(rotate-right-1 '(a b c d e))
;(rotate-right-1 '(a))
;(rotate-right-1 '(a b))
;(rotate-right-1 '(a b c d e f g))

;(reverse-list '(a))
;(reverse-list '(a b))
;(reverse-list '(a b c d e))

;(cons-all 'a '((b c) (d e) (f g)))

;(perm '(a b))
;(perm '(a b c))
;(perm '(a b c d))