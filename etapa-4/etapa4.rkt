#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

; TODO (0p)
; Aveți libertatea să vă structurați programul cum doriți
; (dar cu restricțiile de mai jos), astfel încât
; funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - va exista în continuare funcția (empty-counter index)
; - veți reprezenta cozile folosind noul TDA queue

;; am adaugat un camp in plus pentru simplitudine
(define-struct counter (index tt et queue is-open) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 empty-queue #t))

;; functiile ajutatoare din etapele trecute + adaptare la noua structura
(define (update f counters index)
  ;; ca sa parcurg fiecare casa din counters, o sa folosesc map
  (map (lambda (C)
         ;; daca indexul casei curente este bun, aplic functia f
         (if (= (counter-index C) index)
             (f C)
             C))
       counters))

(define tt+
  ;; tt+ primeste nr de minute
  (lambda (minutes)
    ;; intoarce o noua functie care asteapta o casa C
    (lambda (C)
      ;; construiesc o casa noua ca cea veche, dar cu tt crescut
      (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C) (counter-is-open C)))))

(define et+
  ;; aceeasi logica ca la tt+, dar acum adun minutele la et
  (lambda (minutes)
    ;; intoarce functia care asteapta o casa C
    (lambda (C)
      ;; construiesc casa noua cu et crescut
      (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-queue C) (counter-is-open C)))))

(define ((add-to-counter name items) C)
  ;; tt va creste mereu cu nr de produse al noului client
  ;; et va creste cu nr de produse doar daca coada casei era goala
  (make-counter (counter-index C) (+ (counter-tt C) items)
                (if (queue-empty? (counter-queue C))
                    ;; daca coada e goala, et creste
                    (+ (counter-et C) items)
                    ;; altfel nu se schimba
                    (counter-et C))
                ;; noua coada = adaug clientul nou cu enqueue
                (enqueue (cons name items) (counter-queue C))
                (counter-is-open C)))

(define abstract-min-function
  ;; functia abstracta primeste modul in care se va extrage valoarea (counter-tt sau counter-et)
  (lambda (extract-value)
    ;; returneaza o functie care primeste lista de case
    (lambda (counters)
      ;; ca sa gasesc minimul dintr o lista voi folosi foldl
      ;; compar fiecare elem cu minimul de pana atunci
      (foldl
       (lambda (current-counter min-counter)
         (if (< (extract-value current-counter) (extract-value min-counter))
             current-counter
             min-counter))
       ;; vloarea initiala pt foldl va fi prima casa din lista
       (car counters)
       ;; lista parcursa va fi restul caselor
       (cdr counters)))))

(define min-tt
  (lambda (counters)
    ;; casa cu tt min
    (define min-tt-counter ((abstract-min-function counter-tt) counters))
    ;; perechea (index . tt)
    (cons (counter-index min-tt-counter) (counter-tt min-tt-counter))))

(define min-et
  (lambda (counters)
    ;; casa cu et min
    (define min-et-counter ((abstract-min-function counter-et) counters))
    ;; perechea (index . et)
    (cons (counter-index min-et-counter) (counter-et min-et-counter))))

(define (remove-first-from-counter C)
  ;; scad din tt vechiul et, se elimina delay ul si produsele primului client
  (make-counter (counter-index C)
                (- (counter-tt C) (counter-et C))
                ;; pt noul et apelez dequeue ca sa vad coada fara primul elem si verific daca a ramas goala
                (if (queue-empty? (dequeue (counter-queue C)))
                    0
                    ;; scot clientul de pe primul loc si verific noul prim client cu top si ii iau nr de produse
                    (cdr (top (dequeue (counter-queue C)))))
                (dequeue (counter-queue C))
                (counter-is-open C)))

(define ((pass-time-through-counter minutes) C)
  (make-counter (counter-index C)
                ;; din tt scad minutele trecute si am grija sa nu am nr negative
                (if (< (- (counter-tt C) minutes) 0) 0 (- (counter-tt C) minutes))
                ;; pt et fac ca la tt
                (if (< (- (counter-et C) minutes) 0) 0 (- (counter-et C) minutes))
                ;; coada e aceeasi cf cerintei
                (counter-queue C)
                (counter-is-open C)))

(define (close-counter C)
  (make-counter (counter-index C) (counter-tt C) (counter-et C) (counter-queue C) #f))

(define (open-counter C)
  (make-counter (counter-index C) (counter-tt C) (counter-et C) (counter-queue C) #t))


; TODO 7 (70p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 6 tipuri de cereri:
;   4 moștenite din etapa 3:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă deschisă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al caselor deschise depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
;   plus 2 noi:
;   - (close <index>) - închide casa cu indexul <index> (casa există deja)
;   - (open <index>) - deschide casa cu indexul <index> (casa există deja)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa DESCHISĂ cu tt minim la care are voie;
;   se garantează că persoana poate fi distribuită la o casă
; - nicio modificare pentru situația când o casă suferă o întârziere
; - dacă tt-ul mediu pentru toate casele DESCHISE > <average>,
;   adaugă case slow până când media <= <average>
; - nicio modificare în modelarea trecerii timpului
; - o casă care se închide nu mai primește clienți noi și:
;   - primul client (dacă există) își continuă treaba la această casă
;   - restul clienților se redistribuie la celelalte case,
;     în ordinea în care erau așezați la coadă
; - o casă care se deschide redevine disponibilă pentru clienți
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista cozilor nevide în starea finală, sortată după indexul casei
;   - elementele listei au forma (index_casă . coadă) (coada este de tip queue)
(define (serve requests fast-counters slow-counters)
  (define (process-requests req fast slow departed)
    (if (null? req)
        ;; daca nu mai sunt cereri, returnez (lista_plecati . case_finale)
        (let* ((all-counters (append fast slow))
               ;; le retin doar pe cele care au clienti
               (busy-counters (filter (lambda (C) (not (queue-empty? (counter-queue C)))) all-counters))
               ;; fac casele pereche (index . coada)
               (final-queues (map (lambda (C) (cons (counter-index C) (counter-queue C))) busy-counters)))
          (cons departed (sort final-queues < #:key car)))

        ;; prima cerere in current si restul in next
        (let ((current-req (car req))
              (next-req (cdr req)))
          
          (cond
            ;; trecerea timpului
            [(number? current-req)
             (define (pass-time mins-left current-fast current-slow current-departed)
               (let* ((all-counters (append current-fast current-slow))
                      ;; aflu ce case au efectiv clienti ca sa stiu daca are cine sa iasa
                      (busy-counters (filter (lambda (C) (not (queue-empty? (counter-queue C)))) all-counters)))
                 
                 (if (null? busy-counters)
                     ;; daca toate casele sunt goale, nu are cine sa iasa, asa ca doar scad timpul din toate
                     (process-requests next-req
                                       (map (pass-time-through-counter mins-left) current-fast)
                                       (map (pass-time-through-counter mins-left) current-slow)
                                       current-departed)
                     
                     ;; primul care iese e cel cu et minim
                     (let* ((min-pair (min-et busy-counters))
                            (min-index (car min-pair))
                            (min-time (cdr min-pair)))
                       
                       (if (> min-time mins-left)
                           ;; daca pana iese clientul e mai mult decat trece timpul acum, nu iese nimeni
                           (process-requests next-req 
                                             (map (pass-time-through-counter mins-left) current-fast) 
                                             (map (pass-time-through-counter mins-left) current-slow) 
                                             current-departed)
                           
                           ;; scad timpul pana ajung la momentul plecarii clientului
                           (let* ((fast-updated (map (pass-time-through-counter min-time) current-fast))
                                  (slow-updated (map (pass-time-through-counter min-time) current-slow))
                                  (all-updated (append fast-updated slow-updated))
                                  
                                  ;; caut casa din care trebuie sa iasa cu min-index
                                  (target-counter (car (filter (lambda (C) (= (counter-index C) min-index)) all-updated)))
                                  ;; iau numele de la prima pereche din coada
                                  (client-name (car (top (counter-queue target-counter)))))

                             ;; reiau procesul pentru min ramase, dar fara clientul curent
                             (pass-time (- mins-left min-time) 
                                        (update remove-first-from-counter fast-updated min-index) 
                                        (update remove-first-from-counter slow-updated min-index) 
                                        ;; adaug noul plecat la finalul listei departed
                                        (append current-departed (list (cons min-index client-name))))))))))

             ;; executarea functiei
             (pass-time current-req fast slow departed)]

            ;; delay
            [(eq? (car current-req) 'delay)
             (let ((index (cadr current-req))
                   (mins (caddr current-req)))
               (process-requests next-req 
                                 (update (lambda (C) ((et+ mins) ((tt+ mins) C))) fast index) 
                                 (update (lambda (C) ((et+ mins) ((tt+ mins) C))) slow index) 
                                 departed))]
                                 
            ;; open
            [(eq? (car current-req) 'open)
             (let ((index (cadr current-req)))
               ;; aplic open-counter si seteaza campul is-open pe true
               (process-requests next-req (update open-counter fast index) (update open-counter slow index) departed))]
               
            ;; close
            [(eq? (car current-req) 'close)
             (let* ((index (cadr current-req))
                    (all-counters (append fast slow))
                    ;; caut casa care trebuie inchisa
                    (target-counter (car (filter (lambda (C) (= (counter-index C) index)) all-counters)))
                    (q (counter-queue target-counter)))

               ;; functie ajutatoare care scoate clientii din coada in liste de forma (nume produse)
               (define (extract-clients queue)
                 (if (queue-empty? queue)
                     null
                     (cons (list (car (top queue)) (cdr (top queue))) 
                           (extract-clients (dequeue queue)))))

               ;; functie ajutatoare care asaza la loc clientii ce trebuie scosi de la o casa ce se inchide
               ;; intoarce listele de case actualizate in pereche (fast . slow)
               (define (redistribute-clients clients curr-fast curr-slow)
                 (if (null? clients)
                     ;; cand terminam, returnam listele
                     (cons curr-fast curr-slow)
                     ;; altfel, il luam pe primul si il rutam
                     (let* ((name (car (car clients)))
                            (items (cadr (car clients)))
                            (open-fast (filter counter-is-open curr-fast))
                            (open-slow (filter counter-is-open curr-slow)))
                       ;; logica din client nou
                       (if (<= items ITEMS)
                           (if (null? open-fast)
                               (redistribute-clients (cdr clients) curr-fast (update (add-to-counter name items) curr-slow (car (min-tt open-slow))))
                               (if (null? open-slow)
                                   (redistribute-clients (cdr clients) (update (add-to-counter name items) curr-fast (car (min-tt open-fast))) curr-slow)
                                   (if (<= (cdr (min-tt open-fast)) (cdr (min-tt open-slow)))
                                       (redistribute-clients (cdr clients) (update (add-to-counter name items) curr-fast (car (min-tt open-fast))) curr-slow)
                                       (redistribute-clients (cdr clients) curr-fast (update (add-to-counter name items) curr-slow (car (min-tt open-slow)))))))
                           ;; doar la slow
                           (redistribute-clients (cdr clients) curr-fast (update (add-to-counter name items) curr-slow (car (min-tt open-slow))))))))

               (if (queue-empty? q)
                   ;; daca nu are clienti la coada, o inchid si merg mai departe
                   (process-requests next-req (update close-counter fast index) (update close-counter slow index) departed)
                   (if (queue-empty? (dequeue q))
                       ;; daca are doar un client, isi termina treaba si inchid casa
                       (process-requests next-req (update close-counter fast index) (update close-counter slow index) departed)

                       ;; daca sunt mai multi clienti, primul ramane, restul ii redistribui
                       (let* ((modified-counter (make-counter index (counter-et target-counter) (counter-et target-counter) (enqueue (top q) empty-queue) #f))
                              (extracted-reqs (extract-clients (dequeue q)))

                              ;; tai casa inchisa din liste ca sa nu vina un client tot aici
                              (fast-closed (update (lambda (C) modified-counter) fast index))
                              (slow-closed (update (lambda (C) modified-counter) slow index))
                              
                              ;; redistribui clientii ramasi
                              (new-lists (redistribute-clients extracted-reqs fast-closed slow-closed)))

                         ;; trecem la comenzile urmatoare, in car sunt cele fast si in cdr cele slow
                         (process-requests next-req 
                                           (car new-lists) 
                                           (cdr new-lists) 
                                           departed)))))]
        
            ;; ensure average
            [(eq? (car current-req) 'ensure)
             (let* ((average (cadr current-req))
                    ;; calculez media doar pe casele deschise
                    (open-counters (filter counter-is-open (append fast slow))))
               (if (null? open-counters)
                   (process-requests next-req fast slow departed)
                   (if (> (/ (apply + (map counter-tt open-counters)) (length open-counters)) average)
                       ;; daca media e depasita, adaug o casa slow
                       (process-requests req fast (append slow (list (empty-counter (+ 1 (apply max (map counter-index (append fast slow))))))) departed)
                       ;; altfel sar peste
                       (process-requests next-req fast slow departed))))]
            
            ;; client nou
            [else
             (let* ((name (car current-req))
                    (items (cadr current-req))
                    ;; clientii noi se asaza doar la casele deschise
                    (open-fast (filter counter-is-open fast))
                    (open-slow (filter counter-is-open slow)))
               (if (<= items ITEMS)
                   ;; are voie la ambele tipuri de case
                   (if (null? open-fast)
                       (process-requests next-req fast (update (add-to-counter name items) slow (car (min-tt open-slow))) departed)
                       (if (null? open-slow)
                           (process-requests next-req (update (add-to-counter name items) fast (car (min-tt open-fast))) slow departed)
                           ;; daca sunt optiuni si la fast si la slow, iau minimul
                           (if (<= (cdr (min-tt open-fast)) (cdr (min-tt open-slow)))
                               (process-requests next-req (update (add-to-counter name items) fast (car (min-tt open-fast))) slow departed)
                               (process-requests next-req fast (update (add-to-counter name items) slow (car (min-tt open-slow))) departed))))
                   ;; are voie doar la casele slow deschise
                   (process-requests next-req fast (update (add-to-counter name items) slow (car (min-tt open-slow))) departed)))]))))
  
  (process-requests requests fast-counters slow-counters null))