; Abagiu Ioan-Razvan
; 321CD

#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))


; identic etapa2
; intoarce casa cu index dat sau counters daca aceasta nu exista
; (car filter) pentru ca filter intoarce o lista
; si vrem doar primul element
(define (search-by-index counters index)
  (if (null? (filter (lambda (C)
                       (if (equal? (counter-index C) index)
                           #t
                           #f)) counters))
      counters
      (car (filter (lambda (C)
                     (if (equal? (counter-index C) index)
                         #t
                         #f)) counters))))


; identic etapa2
; intoarce lista initiala daca nu exista casa cu index dat
; sau aplica functia f pe casa gasita iar apoi traverseaza lista folosind
; foldl si retine in acumulator casele, daca este casa cu index dat o
; inlocuieste cu casa cea noua
(define (update f counters index)
  (if (list? (search-by-index counters index))
      counters
      (reverse (foldl (lambda(C acc)
                        (if (equal? (counter-index C) index)
                            (cons (f C) acc)
                            (cons C acc))) '() counters))))


; identic etapa2
; functie curry primind ca argument nr de minute
; apoi se reaplica pe o casa de marcat
(define (tt+ minutes)
  (lambda (C)
    (struct-copy counter C [tt (+ (counter-tt C) minutes)])))


; identic etapa2
; acelasi comportament ca functia tt+
(define (et+ minutes)
  (lambda (C)
    (struct-copy counter C [et (+ (counter-et C) minutes)])))


; folosind abstractizarile de TDA se adauga cu enqueue, restul la fel ca etapa2 pentru update la tt/et
(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
        (struct-copy counter ((et+ items)((tt+ items) C)) [queue (enqueue (cons name items) (counter-queue C))])
        (struct-copy counter ((tt+ items) C) [queue (enqueue (cons name items) (counter-queue C))]))))


; identic etapa2
; functie abstracta care intoarce casa cu f = counter-tt/counter-et minim din lista de counters
(define (min-helper f)
  (lambda (counters)
    (car (filter (lambda (C)
                   (if (equal? (f C) (apply min (foldl (lambda(C acc)
                                                         (cons (f C) acc)) '() counters)))
                       #t
                       #f)) counters))))


; identic etapa2
; folosind funcția de mai sus - pasam functia counter-tt pentru helper si facem perechea index - tt
(define (min-tt counters)
  (cons (counter-index ((min-helper counter-tt) counters)) (counter-tt ((min-helper counter-tt) counters))))


; identic etapa2
; folosind funcția de mai sus - pasam functia counter-et pentru helper si facem perechea index - et
(define (min-et counters)
  (cons (counter-index ((min-helper counter-et) counters)) (counter-et ((min-helper counter-et) counters))))



; identic etapa2
; calculeaza tt din persoanele din queue a unei case si intoarce noua casa cu tt actualizat
(define (sum-of-queue-tt C)
  (struct-copy counter C [tt (foldl (lambda (pair acc)
                                      (+ (cdr pair) acc)) 0 (append (queue-left (counter-queue C)) (queue-right (counter-queue C))))]))


; identic etapa2
; elimina prima persoana din queue si actualizeaza et cu cel al urmatoarei persoane
; daca nu exista, cu 0
; tt actualizeaza prin suma tuturor n-items din queue
(define (remove-first-from-counter C)    ; testata de checker
  (if (queue-empty? (counter-queue C))
      C
      (sum-of-queue-tt (struct-copy counter C [et
                                               (if (queue-empty? (dequeue (counter-queue C)))
                                                   0
                                                   (cdr (top (dequeue (counter-queue C)))))
                                               ][queue (dequeue (counter-queue C))]))))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
      [(and (< (- (counter-tt C) minutes) 0) (< (- (counter-et C) minutes) 0)) (struct-copy counter C [tt 0] [et 0])] ; ambele dau sub 0 cand minutes > et
      [(and (>= (- (counter-tt C) minutes) 0) (< (- (counter-et C) minutes) 0)) (struct-copy counter C [tt (- (counter-tt C) minutes)] [et 0])] ; et da sub 0 cand minutes > et si minutes < tt
      [(and (>= (- (counter-tt C) minutes) 0) (>= (- (counter-et C) minutes) 0)) (struct-copy counter C [tt (- (counter-tt C) minutes)] [et (- (counter-et C) minutes)])]))) ; ambele peste 0


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.


; functie care este apelata de checker by default
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))


; functie apelata de functia serve care contine in plus parametrul de acumulator
(define (serve-helper requests fast-counters slow-counters acc)
  (if (null? requests)
      (cons acc (append fast-counters slow-counters))
      (match (car requests)
        [(list 'ensure average) ; update pe slow-counters
         (serve-helper (cdr requests) fast-counters (add-slow-counter fast-counters slow-counters average) acc)
         ]
        [(list name n-items)
         (if (<= n-items ITEMS) ; si fast si slow
             (if (list? (search-by-index fast-counters (car (min-tt (append fast-counters slow-counters)))))
                 ; este in slow-counters
                 (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) acc)
                 ; este in fast-counters
                 (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters acc))
             (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) acc)) ; doar slow
         ]
        [(list 'delay index minutes)
         (if (list? (search-by-index fast-counters index))
             ; este in slow-counters
             (serve-helper (cdr requests) fast-counters (update (et+ minutes) (update (tt+ minutes) slow-counters index) index) acc)
             ; este in fast-counters
             (serve-helper (cdr requests) (update (et+ minutes) (update (tt+ minutes) fast-counters index) index) slow-counters acc))    
         ]
        [minutes
         (serve-helper (cdr requests)
                       (extract-fast-counters (cdr (for-loop '() (append fast-counters slow-counters) 1 minutes)) (counter-index (car (reverse fast-counters))))
                       (extract-slow-counters (cdr (for-loop '() (append fast-counters slow-counters) 1 minutes)) (counter-index (car slow-counters)))
                       
                       (append acc (filter (lambda (element) ; scoate acumulatorii auxiliari nuli
                                             (if (null? element)
                                                 #f
                                                 #t))
                                           (car (for-loop '() (append fast-counters slow-counters) 1 minutes)))))])))


; functie ce returneaza media tt pe toate casele
(define (average-tt fast-counters slow-counters)
  (/ (foldl (lambda (C acc)
              (+ (counter-tt C) acc)) 0 (append fast-counters slow-counters)) (counter-index (car (reverse slow-counters)))))


; functie ce adauga slow-counter pana cand conditia mediei e indeplinita
(define (add-slow-counter fast-counters slow-counters average)
  (if (> (average-tt fast-counters slow-counters) average)
      ; adaugat
      (add-slow-counter fast-counters (append slow-counters (list (empty-counter (add1 (counter-index (car (reverse slow-counters))))))) average)
      ; ignorat
      slow-counters
      ))


; functie care extrage acumulatorul total
(define (get-acc from-result)
  (foldl (lambda (element acc)
           (append acc (list (car element)))) '() from-result))


; functie care extrage toate casele
(define (get-counters from-result)
  (foldl (lambda (element acc)
           (append acc (list (cdr element)))) '() from-result))


; functie care extrage casele fast dintr-o lista de case
(define (extract-fast-counters list last-idx)
  (filter (lambda (C)
            (if (<= (counter-index C) last-idx)
                #t
                #f
                )) list))


; functie care extrage casele slow dintr-o lista de case
(define (extract-slow-counters list first-idx)
  (filter (lambda (C)
            (if (>= (counter-index C) first-idx)
                #t
                #f)) list))


; functie ce simuleaza un for loop() si intoarce un acumulator
; folosita pentru trecerea timpului treptat pe la case
(define (for-loop acc counters index stop-index)
  
  (if (equal? index (add1 stop-index))
      (cons acc counters) ; acumulator final si casele
      
      (for-loop (append acc (get-acc (pass-one-minute counters)))
                (get-counters (pass-one-minute counters))
                (add1 index)
                stop-index)))

; functie ce simuleaza trecerea a 1 minut pe la fiecare casa
(define (pass-one-minute counters)
  (foldl (lambda (C acc1)
             
           (if (equal? (counter-et C) 0) ; are deja et = 0?
               ; da, return intre acumulator si C
               (append acc1 (list (cons '() C)))
               ; nu
               (if (> (- (counter-et C) 1) 0) ; et - 1 > 0?
                   ; da, return cu et = et - 1, tt = tt - 1
                   (append acc1 (list (cons '() (struct-copy counter C [tt (- (counter-tt C) 1)] [et (- (counter-et C) 1)]))))
                   ; nu, deci et - 1 <= 0
                   ; avem empty-queue?
                   (if (queue-empty? (counter-queue C))
                       ; da, return la C cu tt si et actualizat
                       (append acc1 (list (cons '() (struct-copy counter C [tt (- (counter-tt C) 1)] [et 0])))) ; tt = tt - et, et = 0
                       ; nu
                       (if (> (queue-size (counter-queue C)) 1) ; avem queue-size > 1?
                           ; da
                           (append acc1 (list (cons (cons (counter-index C) (car (top (counter-queue C)))) (struct-copy counter C [tt (- (counter-tt C) 1)]
                                                                                                                        [et (cdr (top (dequeue (counter-queue C))))]
                                                                                                                        [queue (dequeue (counter-queue C))]))))
                           ; nu
                           (append acc1 (list (cons (cons (counter-index C) (car (top (counter-queue C)))) (struct-copy counter C [tt (- (counter-tt C) 1)]
                                                                                                                        [et 0]
                                                                                                                        [queue (dequeue (counter-queue C))]))))))))) '() counters))
