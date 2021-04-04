#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index tt et queue state) #:transparent)
; state
; 1 - casa deschisa
; 0 - casa inchisa


(define (empty-counter index)
  (make-counter index 0 0 empty-queue 1))
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei


; functie care este apelata de checker by default
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))


; functie apelata de functia serve care contine in plus parametrul de acumulator
(define (serve-helper requests fast-counters slow-counters acc)
  (if (null? requests)
      (cons acc (reverse (get-queues (append fast-counters slow-counters))))
      (match (car requests)
        [(list 'close index) ; seteaza casa cu index dat pe state - 0 -> casa inchisa
         (serve-helper (cdr requests) (extract-fast-counters (update (lambda (C) (struct-copy counter C [state 0])) (append fast-counters slow-counters) index) (counter-index (car (reverse fast-counters))))
                       (extract-slow-counters (update (lambda (C) (struct-copy counter C [state 0])) (append fast-counters slow-counters) index) (counter-index (car slow-counters)))
                       acc)
         ]
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


; functie ce returneaza media tt pe toate casele deschise
(define (average-tt fast-counters slow-counters)
  (/ (foldl (lambda (C acc)
              (+ (counter-tt C) acc)) 0 (filter (lambda (C)
                                                  (if (equal? (counter-state C) 1)
                                                      #t
                                                      #f)) (append fast-counters slow-counters))) (length (filter (lambda (C)
                                                                                                                    (if (equal? (counter-state C) 1)
                                                                                                                        #t
                                                                                                                        #f)) (append fast-counters slow-counters)))))


; functie ce adauga slow-counter pana cand conditia mediei e indeplinita
(define (add-slow-counter fast-counters slow-counters average)
  (if (> (average-tt fast-counters slow-counters) average)
      ; adaugat
      (add-slow-counter fast-counters (append slow-counters (list (empty-counter (add1 (counter-index (car (reverse slow-counters))))))) average)
      ; ignorat
      slow-counters))


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
; folosind funcția de mai sus - pasam functia counter-tt pentru helper si facem perechea index - tt doar pentru casele deschise
(define (min-tt counters)
  (cons (counter-index ((min-helper counter-tt) (filter (lambda (C)
                                                          (if (equal? (counter-state C) 1)
                                                              #t
                                                              #f)) counters))) (counter-tt ((min-helper counter-tt)
                                                                                            (filter (lambda (C)
                                                                                                      (if (equal? (counter-state C) 1)
                                                                                                          #t
                                                                                                          #f
                                                                                                          )) counters)))))


; identic etapa2
; folosind funcția de mai sus - pasam functia counter-et pentru helper si facem perechea index - et
(define (min-et counters)
  (cons (counter-index ((min-helper counter-et) counters)) (counter-et ((min-helper counter-et) counters))))



; identic etapa2
; calculeaza tt din persoanele din queue a unei case si intoarce noua casa cu tt actualizat
(define (sum-of-queue-tt C)
  (struct-copy counter C [tt (foldl (lambda (pair acc)
                                      (+ (cdr pair) acc)) 0 (append (queue-left (counter-queue C)) (queue-right (counter-queue C))))]))


; functie ce returneaza o lista cu perechi de index-casa si coada-casa pentru casele deschise
(define (get-queues counters)
  (foldl (lambda (C acc)
           (append (list (cons (counter-index C) (counter-queue C))) acc)) '() (filter (lambda (C)
                                                                                         (if (queue-empty? (counter-queue C))
                                                                                             #f
                                                                                             #t))
                                                                                       counters)))
