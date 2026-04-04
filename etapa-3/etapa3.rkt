#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Este necesar să implementați întâi
;;          TDA-ul queue în fișierul queue.rkt.
;; Reveniți la acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Se modifică însă implementarea câmpului queue:
; - în loc de listă, acesta va fi o structură de tip queue
; - modificarea nu este vizibilă în definiția structurii,
;   ci în implementarea operațiilor tipului counter
(define-struct counter (index tt et queue) #:transparent)


; TODO 6 (20p)
; Actualizați funcțiile de mai jos conform cu 
; noua reprezentare a cozii de persoane.
; Elementele cozii rămân perechi (nume . nr_produse).
; RESTRICȚII (5p per abatere)
;  - Respectați "bariera de abstractizare", adică 
;    operați cu coada folosind exclusiv interfața:
;    - empty-queue
;    - queue-empty?
;    - enqueue
;    - dequeue
;    - top
; Obs: Doar câteva funcții necesită actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  ;; ca in etapa 2
  (map (lambda (C)
         (if (= (counter-index C) index)
             (f C)
             C))
       counters))

(define tt+
  ;; ca in etapa 2
  (lambda (minutes)
    (lambda (C)
      (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C)))))

(define et+
  ;; ca in etapa 2
  (lambda (minutes)
    (lambda (C)
      (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-queue C)))))

(define ((add-to-counter name items) C) ; testată de checker
  (make-counter (counter-index C) (+ (counter-tt C) items)
                ;; daca e goala coada, et creste, altfel ramane la fel
                (if (queue-empty? (counter-queue C))
                    (+ (counter-et C) items)
                    (counter-et C))
                (enqueue (cons name items) (counter-queue C))))                      ; nu modificați signatura!
    

(define abstract-min-function
  ;; ca in etapa 2
  (lambda (extract-value)
    (lambda (counters)
      (foldl
       (lambda (current-counter min-counter)
         (if (< (extract-value current-counter) (extract-value min-counter))
             current-counter
             min-counter))
       (car counters)
       (cdr counters)))))
(define min-tt
  ;; ca in etapa 2
  (lambda (counters)
    (define min-tt-counter ((abstract-min-function counter-tt) counters))
    (cons (counter-index min-tt-counter) (counter-tt min-tt-counter))))
(define min-et
  ;; ca in etapa 2
  (lambda (counters)
    (define min-et-counter ((abstract-min-function counter-et) counters))
    (cons (counter-index min-et-counter) (counter-et min-et-counter))))

(define (remove-first-from-counter C); testată de checker
  ;; scad din tt vechiul et, se elimina delay ul si produsele primului client
  (make-counter (counter-index C)
                (- (counter-tt C) (counter-et C))
                ;; pt noul et apelez dequeue ca sa vad coada fara primul elem si verific daca a ramas goala
                (if (queue-empty? (dequeue (counter-queue C)))
                    0
                    ;; scot clientul de pe primul loc si verific noul prim client cu top si ii iau nr de produse
                    (cdr (top (dequeue (counter-queue C)))))
                (dequeue (counter-queue C))))


; TODO 7 (10p)
; Implementați o funcție care calculează starea
; unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp
; nu iese nimeni din coadă, deci se modifică
; doar câmpurile tt și et.
; Este responsabilitatea utilizatorului să nu apeleze
; funcția cu minutes > et și coadă nevidă.
; La casele fără clienți, este responsabilitatea
; voastră să nu produceți timpi negativi.
(define ((pass-time-through-counter minutes) C)
  (make-counter (counter-index C)
                ;; din tt scad minutele trecute si am grija sa nu am nr negative
                (if (< (- (counter-tt C) minutes) 0)
                    0
                    (- (counter-tt C) minutes))

                ;; pt et fac ca la tt
                (if (< (- (counter-et C) minutes) 0)
                    0
                    (- (counter-et C) minutes))

                ;; coada e aceeasi cf cerintei
                (counter-queue C)))
  

; TODO 8 (60p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri:
;   3 moștenite din etapa 2:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   plus noutatea:
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
; Obs: Cererile (remove-first) din etapa 2 sunt înlocuite de un mecanism  
; mai sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul procesează cererile în ordine, astfel:
; - nicio modificare pentru cererile moștenite din etapa 2
; - când timpul prin sistem avansează cu <x> minute, starea caselor
;   se actualizează pentru a reflecta trecerea timpului;
;   ieșirile clienților din coadă se rețin în ordine cronologică.
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Sugestii:
; - gestionați cronologia folosind în mod repetat funcția min-et 
; - pentru a menține lista clienților plecați, definiți o funcție ajutătoare
; (cu un parametru în plus față de serve), pe care serve doar o apelează.
; RESTRICȚII (5p per abatere)
;  - Folosiți minim un let și un let* (care nu ar putea fi let). (2*5p)
;  - Respectați "bariera de abstractizare" oricând operați cu tipul queue.
(define (serve requests fast-counters slow-counters)
  ;; functie ajutataoare cu acumulator pt clientii plecati
  (define (process-requests req fast slow departed)
    (if (null? req)
        ;; daca nu mai sunt cereri, returnez (lista_plecati . case_finale)
        (cons departed (append fast slow))
        
        ;; prima cerere
        (let ((current-req (car req))
              (next-req (cdr req)))
          
          (cond
            ;; trecerea timpului
            [(number? current-req)
             ;; functie ajutatoare care consuma minutele pas cu pas pana la zero
             (define (pass-time mins-left current-fast current-slow current-departed)
               ;; busy-counters depinde de all-counters
               (let* ((all-counters (append current-fast current-slow))
                      ;; casele care au clienti
                      (busy-counters (filter (lambda (C) (not (queue-empty? (counter-queue C)))) all-counters)))
                 
                 (if (null? busy-counters)
                     ;; daca toate casele sunt goale, nu are cine sa iasa si scad timpul ramas din toate casele
                     (process-requests next-req
                                       ;; scad timpul pe fiecare casa din current-fast
                                       (map (pass-time-through-counter mins-left) current-fast)
                                       (map (pass-time-through-counter mins-left) current-slow)
                                       current-departed)
                     
                     ;; primul care iese are et minim
                     (let* ((min-pair (min-et busy-counters))
                            (min-index (car min-pair))
                            (min-time (cdr min-pair)))
                       
                       (if (> min-time mins-left)
                           ;; daca timpul pana iese primul e mai mare decat timpul ce trece, nu iese nimeni
                           (process-requests next-req (map (pass-time-through-counter mins-left) current-fast) (map (pass-time-through-counter mins-left) current-slow) current-departed)
                           
                           ;; daca clientul iese, scad timpul pana ajung la momentul plecarii lui
                           (let* ((fast-updated (map (pass-time-through-counter min-time) current-fast))
                                  (slow-updated (map (pass-time-through-counter min-time) current-slow))
                                  (all-updated (append fast-updated slow-updated))
                                  
                                  ;; caut casa cu indexul min-index
                                  (target-counter (car (filter (lambda (C) (= (counter-index C) min-index)) all-updated)))
                                  ;; iau numele de la prima pereche din coada
                                  (client-name (car (top (counter-queue target-counter)))))
                             
                             ;; apelez pentru restul de minute si scot clientul care a plecat
                             (pass-time (- mins-left min-time) (update remove-first-from-counter fast-updated min-index) (update remove-first-from-counter slow-updated min-index) (append current-departed (list (cons min-index client-name))))))))))
             
             (pass-time current-req fast slow departed)]

            ;; delay
            [(eq? (car current-req) 'delay)
             (let ((index (cadr current-req))
                   (mins (caddr current-req)))
               (process-requests next-req (update (lambda (C) ((et+ mins) ((tt+ mins) C))) fast index) (update (lambda (C) ((et+ mins) ((tt+ mins) C))) slow index) departed))]
            
            ;; ensure average
            [(eq? (car current-req) 'ensure)
             (let ((average (cadr current-req))
                   (all-counters (append fast slow)))
               (if (> (/ (apply + (map counter-tt all-counters)) (length all-counters)) average)
                   ;; daca media e depasita, adaug o casa slow
                   (process-requests req fast (append slow (list (empty-counter (+ 1 (apply max (map counter-index all-counters)))))) departed)
                   (process-requests next-req fast slow departed)))]
            
            ;; client nou
            [else
             (let ((name (car current-req))
                   (items (cadr current-req)))
               (if (<= items ITEMS)
                   (if (<= (cdr (min-tt fast)) (cdr (min-tt slow)))
                       (process-requests next-req (update (add-to-counter name items) fast (car (min-tt fast))) slow departed)
                       (process-requests next-req fast (update (add-to-counter name items) slow (car (min-tt slow))) departed))
                   (process-requests next-req fast (update (add-to-counter name items) slow (car (min-tt slow))) departed)))]))))
  
  (process-requests requests fast-counters slow-counters '()))
