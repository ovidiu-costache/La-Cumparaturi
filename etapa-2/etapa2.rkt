#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul
;; până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client
;; + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat Ana cu 3 produse, apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute =>
;; et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO 1 (5p)
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 null))


; TODO 2 (15p)
; Implementați o funcție care aplică o transformare f
; casei cu un anumit index.
; f = funcție unară cu un parametru de tip casă,
; counters = listă de case,
; index = indexul casei care trebuie transformată
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index,
; întoarceți lista nemodificată.
(define (update f counters index)
  ;; ca sa parcurg fiecare casa din counters, o sa folosesc map
  (map
   ;; functia lambda
   (lambda (C)
     ;; daca indexul casei curente este bun, aplic functia f
     (if (= (counter-index C) index)
         (f C)
         C))
   ;; lista de case
   counters))


; TODO 3 (7.5p)
; Memento: tt+ crește tt-ul unei case cu un număr de minute.
; Obs: tt+ afectează doar câmpul tt, nu și câmpul et.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument
;   funcției update în cel mai facil mod
; Obs: Facil înseamnă că o aplicație parțială a funcției tt+ 
; va produce o funcție unară cu parametru de tip casă, fără
; să fie nevoie de funcții anonime sau funcții auxiliare.
; Scheletul nu menționează parametrii funcției tt+, întrucât
; trebuie să determinați voi înșivă cum este cel mai bine
; ca tt+ să își primească parametrii.
;
; Apoi implementați funcția checker-tt+, care apelează funcția
; tt+ pe o casă și un număr de minute.
; Funcția checker-tt își precizează clar parametrii și
; poate fi testată, acesta este singurul său rol.
; RESTRICȚII (5p)
;  - Implementați tt+ conform cerinței anterioare.
(define tt+
  ;; tt+ primeste nr de minute
  (lambda (minutes)
    ;; intiarce o noua functie care asteapta o casa C
    (lambda (C)
      ;; construiesc o casa noua ca cea veche, dar cu tt crescut
      (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C)))))

(define (checker-tt+ C minutes)
  ((tt+ minutes) C))


; TODO 4 (7.5p)
; Implementați o funcție care crește et-ul unei case
; cu un număr dat de minute.
; Obs: et+ afectează doar câmpul et, nu și câmpul tt.
; Păstrați formatul folosit pentru tt+.
; Apoi implementați funcția checker-et+ care apelează
; et+, pentru testare.
; RESTRICȚII (5p)
;  - Implementați et+ conform cerinței anterioare.
(define et+
  ;; aceeasi logica ca la tt+, dar acum adun minutele la et
  ;; et+ primeste nr de minute
  (lambda (minutes)
    ;; intoarce functia care asteapta o casa C
    (lambda (C)
      ;; construiesc casa noua cu et crescut
      (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-queue C)))))

(define (checker-et+ C minutes)
  ((et+ minutes) C))


; TODO 5 (10p)
; Memento: add-to-counter adaugă o persoană
; (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter din aceleași
; rațiuni pentru care ați actualizat funcția tt+.
; Atenție la cum se modifică tt și et!
; Apoi implementați funcția checker-add-to-counter
; care apelează add-to-counter, pentru testare.
; RESTRICȚII (5p)
;  - Implementați add-to-counter conform cerinței anterioare.
(define add-to-counter
  ;; tt va creste mereu cu nr de produse al noului client
  ;; et va creste cu nr de produse doar daca coasa casei era goala, daca mai era cineva la coada, et nu se schimba

  ;; functia primeste numele persoanei si nr de produse
  (lambda (name n-items)
    ;; intoarce o functie care asteapta casa
    (lambda (C)
      ;; trebuie creata casa care sa respecte conditiile pt tt si et
      (make-counter
       ;; indexul e acelasi
       (counter-index C)

       ;; noul tt creste mereu cu nr de produse
       (+ (counter-tt C) n-items)

       ;; noul et creste doar daca persoana adaugata e prima
       (if (null? (counter-queue C))
           ;; daca coada e goala, et creste
           (+ (counter-et C) n-items)
           (counter-et C))

       ;; noua coada = vechea coada + clientul nou
       (append (counter-queue C) (list (cons name n-items)))))))

(define (checker-add-to-counter C name n-items)
  ((add-to-counter name n-items) C))


; TODO 6 (15p)
; Întrucât vom folosi atât min-tt (implementat în etapa 1)
; cât și min-et (funcție nouă), definiți o funcție mai abstractă
; din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă nevidă de case și
; întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (la același et, este preferată casa cu indexul cel mai mic)
; Obs: în etapele 2-4, listele de case sunt sortate după index.
; RESTRICȚII (10p - 2*5p)
;  - min-tt și min-et vor fi aplicații parțiale ale funcției abstracte.
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

; folosind funcția de mai sus
(define min-tt
  (lambda (counters)
    ;; casa cu tt min
    (define min-tt-counter ((abstract-min-function counter-tt) counters))
    ;; perechea (index . tt)
    (cons (counter-index min-tt-counter) (counter-tt min-tt-counter))))

; folosind funcția de mai sus
(define min-et
  (lambda (counters)
    ;; casa cu et min
    (define min-et-counter((abstract-min-function counter-et) counters))
    ;; perechea
    (cons (counter-index min-et-counter) (counter-et min-et-counter))))


; TODO 7 (10p)
; Implementați o funcție care scoate prima persoană
; din coada unei case.
; Funcția presupune, fără să verifice, că există
; minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin
; modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva,
; înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  ;; extrag coada actuala a casei
  (define current-queue (counter-queue C))
  ;; extrag restul cozii dupa plecarea primului client
  (define remaining-queue (cdr current-queue))

  ;; noul tt este suma produselor clientilor ramasi
  (define new-tt
    (foldl
     (lambda (person acc)
       (+ acc (cdr person)))
     0 ; acumulatorul porneste de la 0
     remaining-queue))

  ;; noul et
  ;; daca nu a mai ramas nimeni la coada, et = 0
  ;; daca a mai ramas cineva, et = nr lui de produse
  (define new-et
    (if (null? remaining-queue)
        0
        ;; (car remaining-queue) este noul prim client
        ;; cdr este numarul de produse
        (cdr (car remaining-queue))))

  ;; constuiesc noua casa
  (make-counter (counter-index C) new-tt new-et remaining-queue))
    

; TODO 8 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a procesa liste de case)
; - requests conține 4 tipuri de cereri (două în plus față de etapa 1):
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care este
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (ca înainte, dar folosind fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc
;   (chiar dacă nu are clienți)
; - persoana cea mai avansată este prima persoană la casa cu et-ul minim
;   (dintre casele care au clienți)
;   (dacă nicio casă nu are clienți, ignoră cererea)
; - dacă tt-ul mediu pentru toate casele > <average>,
;   adaugă case slow până când media <= <average>
;   (puteți determina matematic de câte case noi este nevoie sau
;   să adăugați recursiv una câte una cât timp este necesar)
; Considerați casele indexate de la 1 și mereu sortate după index.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie adăugați întâi casa 16, apoi casa 17, etc.
; RESTRICȚII (25p - 5*5p)
;  - Folosiți minim două funcționale predefinite în Racket. (2*5p)
;  - Nu apelați checker-tt+, checker-et+, checker-add-to-counter,
;    ci doar tt+, et+, add-to-counter. (3*5p) 
(define (serve requests fast-counters slow-counters)
  (if (null? requests)
      ;; daca nu mai sunt cereri, lipesc listele
      (append fast-counters slow-counters)
      (cond
        ;; (delay index minutes)
        [(eq? (car (car requests)) 'delay)
         (define current (car requests))
         (define next-requests (cdr requests))
         (define idx (cadr current))
         (define mins (caddr current))
         ;; caut casa cu indexul idx in ambele liste si ii dau delay
         ;; update returneaza lista nemodificata daca nu gaseste indexul
         (serve next-requests 
                (update (lambda (C) ((et+ mins) ((tt+ mins) C))) fast-counters idx)
                (update (lambda (C) ((et+ mins) ((tt+ mins) C))) slow-counters idx))]

        ;; (remove-first)
        [(eq? (car (car requests)) 'remove-first)
         (define next-requests (cdr requests))
         ;; trebuie gasita casa cu et min dintre toate casele cu clinti
         (define all-counters (append fast-counters slow-counters))
         ;; filter pt a pastra doar casele cu coada nevida
         (define busy-counters (filter (lambda (C) (not (null? (counter-queue C)))) all-counters))
         
         (if (null? busy-counters)
             ;; daca nu e nimeni la nicio coada, ignor cererea
             (serve next-requests fast-counters slow-counters)
             ;; altfel, scot de la casa cu et min din ambele liste
             (serve next-requests 
                    (update remove-first-from-counter fast-counters (car (min-et busy-counters)))
                    (update remove-first-from-counter slow-counters (car (min-et busy-counters)))))]

        ;; (ensure average)
        [(eq? (car (car requests)) 'ensure)
         (define current (car requests))
         (define average (cadr current))
         (define all-counters (append fast-counters slow-counters))
         ;; calculez tt total al tuturor caselor cu apply si map
         (define total-tt (apply + (map counter-tt all-counters)))
         (define num-counters (length all-counters))
         
         (if (> (/ total-tt num-counters) average)
             ;; daca media e prea mare, deschid o casa slow noua cu indexul urmator
             ;; reapelez cu requests in loc de next-requests pt a reverifica media
             (serve requests 
                    fast-counters 
                    (append slow-counters (list (empty-counter (+ 1 (counter-index (last all-counters)))))))
             ;; daca media e ok, se trece la urmatoarea cerere
             (serve (cdr requests) fast-counters slow-counters))]

        ;; (name n-items), client nou
        [else
         (define current (car requests))
         (define next-requests (cdr requests))
         (define name (car current))
         (define n-items (cadr current))
         
         ;; daca are putine produse, poate merge la ambele tipuri de case
         (if (<= n-items ITEMS)
             ;; aleg casa cu tt min dintre cele doua liste
             ;; caasele rapide au indexurile mici
             (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                 (serve next-requests (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters)
                 (serve next-requests fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))
             ;; daca are mai multe produse, merge automat la slow
             (serve next-requests fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))])))