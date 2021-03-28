; Abagiu Ioan-Razvan
; 321CD PP TEMA1 - etapa2

#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.


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


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.


; functie curry primind ca argument nr de minute
; apoi se reaplica pe o casa de marcat
(define (tt+ minutes)
  (lambda (C)
    (struct-copy counter C [tt (+ (counter-tt C) minutes)])))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.


; acelasi comportament ca functia tt+
(define (et+ minutes)
  (lambda (C)
    (struct-copy counter C [et (+ (counter-et C) minutes)])))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.


; adauga perechea persoana cu numele name si n-items la coada casei C
; daca casa are coada goala => se modifica et cu n-items si tt cu n-items
; altfel doar tt cu n-items
(define (add-to-counter name n-items)
  (lambda (C)
    (if (null? (counter-queue C))
        (struct-copy counter ((et+ n-items)((tt+ n-items) C)) [queue (append (counter-queue C) (list (cons name n-items)))])
        (struct-copy counter ((tt+ n-items) C) [queue (append (counter-queue C) (list (cons name n-items)))]))))       


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)


; functie abstracta care intoarce casa cu f = counter-tt/counter-et minim din lista de counters
(define (min-helper f)
  (lambda (counters)
    (car (filter (lambda (C)
                   (if (equal? (f C) (apply min (foldl (lambda(C acc)
                                                         (cons (f C) acc)) '() counters)))
                       #t
                       #f)) counters))))


; folosind funcția de mai sus - pasam functia counter-tt pentru helper si facem perechea index - tt
(define (min-tt counters)
  (cons (counter-index ((min-helper counter-tt) counters)) (counter-tt ((min-helper counter-tt) counters))))


; folosind funcția de mai sus - pasam functia counter-et pentru helper si facem perechea index - et
(define (min-et counters)
  (cons (counter-index ((min-helper counter-et) counters)) (counter-et ((min-helper counter-et) counters))))


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.


; calculeaza tt din persoanele din queue a unei case si intoarce noua casa cu tt actualizat
(define (sum-of-queue-tt C)
  (struct-copy counter C [tt (foldl (lambda (pair acc)
                                      (+ (cdr pair) acc)) 0 (counter-queue C))]))


; elimina prima persoana din queue si actualizeaza et cu cel al urmatoarei persoane
; daca nu exista, cu 0
; tt actualizeaza prin suma tuturor n-items din queue
(define (remove-first-from-counter C)
  (if (null? (counter-queue C))
      C
      (sum-of-queue-tt (struct-copy counter C [et
                                               (if (null? (remove (first (counter-queue C)) (counter-queue C)))
                                                   0
                                                   (cdr (second (counter-queue C))))
                                               ][queue (remove (first (counter-queue C)) (counter-queue C))]))))
    

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)
(define (serve requests fast-counters slow-counters)

  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average) ; update pe slow-counters
         (serve (cdr requests) fast-counters (add-slow-counter fast-counters slow-counters average))
         ]
        [(list name n-items)
         (if (<= n-items ITEMS) ; si fast si slow
             (if (list? (search-by-index fast-counters (car (min-tt (append fast-counters slow-counters)))))
                 ; este in slow-counters
                 (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))
                 ; este in fast-counters
                 (serve (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters))
             (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))) ; doar slow
         ]
        [(list 'delay index minutes)
         (if (list? (search-by-index fast-counters index))
             ; este in slow-counters
             (serve (cdr requests) fast-counters (update (et+ minutes) (update (tt+ minutes) slow-counters index) index))
             ; este in fast-counters
             (serve (cdr requests) (update (et+ minutes) (update (tt+ minutes) fast-counters index) index) slow-counters))    
         ]
        [(list 'remove-first)
         (if (null? (remove-empty-counters (append fast-counters slow-counters)))
             ; ignoram requestul
             (serve (cdr requests) fast-counters slow-counters)
             ; verific daca e slow/fast
             (if (list? (search-by-index fast-counters (car (min-et (remove-empty-counters (append fast-counters slow-counters))))))
                 ; este in slow-counters
                 (serve (cdr requests) fast-counters (update (lambda (C)
                                                               (remove-first-from-counter (search-by-index slow-counters (car (min-et (remove-empty-counters slow-counters))))))
                                                             slow-counters 
                                                             (car (min-et (remove-empty-counters slow-counters))))) ; index de cautat si aplicat lambda
                 ; este in fast-counters
                 (serve (cdr requests) (update (lambda (C)
                                                 (remove-first-from-counter (search-by-index fast-counters (car (min-et (remove-empty-counters fast-counters))))))
                                               fast-counters 
                                               (car (min-et (remove-empty-counters fast-counters)))) slow-counters))) ; index de cautat si aplicat lambda
         ])))


; helper care scoate casele ce nu au clienti din lista
(define (remove-empty-counters counters)
  (filter (lambda (C)
            (if (null? (counter-queue C))
                #f
                #t)
            ) counters))


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
