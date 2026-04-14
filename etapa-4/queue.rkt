#lang racket
(require racket/match)

(provide empty-queue)
(provide make-queue)
(provide queue-empty?)
(provide rotate)             ; pentru testare
(provide enqueue)
(provide dequeue)
(provide top)

(provide (struct-out queue)) ; pentru testare

;; În etapa 3 am implementat TDA-ul queue asigurând cost amortizat O(1)
;; atât pentru enqueue cât și pentru dequeue.
;; Am reprezentat coada ca pe o colecție de 2 stive:
;; - stiva left: pentru scoaterea de elemente la dequeue 
;; - stiva right: pentru adăugarea de elemente la enqueue 
;;
;; Singurul caz în care o operație nu era O(1) era dequeue când stiva left era goală.
;; Un asemenea dequeue era O(n), din cauza mutării tuturor elementelor din right în left.
;; Ne dorim să îmbunătățim costul operației dequeue pe cazul cel mai
;; defavorabil, de la O(n) la O(1).
;;
;; Soluția: păstrăm reprezentarea cu 2 stive, dar ne asigurăm că, la dequeue, 
;; stiva left nu este niciodată goală, menținând invariantul:
;;        size(left) ≥ size(right)
;; Oricând o operație duce la violarea invariantului, efectuăm o rotație:
;;        <left, right>   devine   <left ++ (reverse right), []>
;; Cât timp stivele sunt liste Racket, o rotație are complexitate O(n),
;; cauzată de append și de reverse. Există o reprezentare mai bună?
;;
;; Da! Vom reprezenta stiva left ca pe un flux.
;; Spre deosebire de append (notat aici ++) pe liste (O(n)),
;; append pe fluxuri este o operație incrementală:
;; - elementele din rezultat sunt furnizate unul câte unul, atunci când este nevoie
;; - ex: A = fluxul [1,2,3,4,5], reprezentat ca (stream-cons 1 <calcul-întârziat-rest>)
;;       B = un flux oarecare
;;       A ++ B va fi (stream-cons 1 <calcul-întârziat-append-între-restA-și-B>)
;;   (acest rezultat se obține în timp O(1))
;; Astfel rezolvăm complexitatea operației append din expresia
;;       "left ++ (reverse right)"
;;
;; Cum rezolvăm complexitatea operației reverse din aceeași expresie?
;; Cum append este deja o operație incrementală, ideea este să efectuăm câte 
;; un pas de reverse de fiecare dată când efectuăm un pas de append.
;; Acest truc termină ambele operații cam în același timp,
;; întrucât rotațiile se declanșează când right devine mai lungă decât left,
;; adică size(right) = size(left) + 1.
;; Amintiți-vă codul pentru append, respectiv reverse cu recursivitate pe coadă:
;; (define (append A B)                     (define (reverse L Acc)
;;   (if (null? A)                            (if (null? L)
;;       B                                        Acc
;;       (cons (car A) (append (cdr A) B))))      (reverse (cdr L) (cons (car L) Acc))))
;;
;; Implementăm o rotație conform axiomelor următoare
;; (observați fuziunea de append și reverse):
;; rotate([], [y], Acc)        = y : Acc                    
;; rotate((x:xs), (y:ys), Acc) = x : rotate(xs, ys, y : Acc)
;; Obs: 
;; - x : rotate(...) reprezintă un pas de append ( : înseamnă cons)
;; - y : Acc         reprezintă un pas de reverse


; Structura queue nu se modifică.
; Ceea ce se modifică este implementarea câmpului left
; - din listă, left devine flux
; - acest lucru nu este vizibil în definiția structurii queue,
;   ci în implementarea operațiilor tipului 
(define-struct queue (left right size-l size-r) #:transparent) 


; RESTRICȚII (maxim 50p)
;  - Implementați funcțiile conform specificației
;  - O funcție implementată diferit se depunctează în totalitate.


; TODO 1 (5p)
; Definiți valoarea care reprezintă o coadă goală.
(define empty-queue
  ;; left e flux gol
  ;; risght e lista vida
  (make-queue empty-stream null 0 0))


; TODO 2 (5p)
; Implementați o funcție care verifică dacă o coadă este goală.
(define (queue-empty? q)
  (if (and (= (queue-size-l q) 0) (= (queue-size-r q) 0))
      #t
      #f))


; TODO 3 (10p)
; Implementați funcția rotate, conform axiomelor de mai sus.
; Atenție: ce tip trebuie să aibă Acc?
;; Acc trebuie sa fie flux
(define (rotate left right Acc)
  ;; daca left e gol, right are un elem
  (if (stream-empty? left)
      ;; pun primul elem din right in fata acumulatorului
      (stream-cons (car right) Acc)
      ;; pastrez primul elem din left si aman apelul recursiv
      (stream-cons (stream-first left)
                   (rotate (stream-rest left) (cdr right) (stream-cons (car right) Acc)))))


; TODO 4 (10p)
; Implementați o funcție care adaugă un element la
; sfârșitul unei cozi. Întoarceți coada actualizată.
; Atenție: în urma adăugării, poate fi necesară o rotație!
(define (enqueue x q)
  (define current-left (queue-left q))
  (define current-right (queue-right q))
  (define current-size-l (queue-size-l q))
  (define current-size-r (queue-size-r q))

  (define new-right (cons x current-right))
  (define new-size-r (+ current-size-r 1))

  ;; daca dreapta e mai mare decat stanga, fac rotatie, altfel pastrez coada
  (if (< current-size-l new-size-r)
      (make-queue (rotate current-left new-right empty-stream) null (+ current-size-l new-size-r) 0)
      (make-queue current-left new-right current-size-l new-size-r)))


; TODO 5 (10p)
; Implementați o funcție care scoate primul element
; dintr-o coadă nevidă. Întoarceți coada actualizată.
; Obs: dequeue pe coada vidă este firesc să dea eroare.
; Atenție: în urma extragerii, poate fi necesară o rotație!
(define (dequeue q)
  (define current-left (queue-left q))
  (define current-right (queue-right q))
  (define current-size-l (queue-size-l q))
  (define current-size-r (queue-size-r q))
  
  (define new-left (stream-rest current-left))
  (define new-size-l (- current-size-l 1))

  (if (< new-size-l current-size-r)
      ;; rotesc
      (make-queue (rotate new-left current-right empty-stream) null (+ new-size-l current-size-r) 0)
      ;; pastrez'
      (make-queue new-left current-right new-size-l current-size-r)))


; TODO 6 (10p)
; Implementați o funcție care obține primul element
; dintr-o coadă nevidă. Întoarceți elementul.
; Obs: top pe coada vidă este firesc să dea eroare.
(define (top q)
  ;; iau primul elem din fluxul left si daca e gol, stream-first da eroare
  (stream-first (queue-left q)))
