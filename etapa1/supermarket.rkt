; Abagiu Razvan
; 321CD

#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)

; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip

; Folosesc constructorul default la care adaug index dat si tt = 0
(define (empty-counter index)
  (make-counter index 0 '()))

; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.

; Caut in casa C tt-ul curent pe care apoi il incrementez cu 'minutes' si creez o structura noua de counter cu aceste date
(define (tt+ C minutes)
  (struct-copy counter C [tt (+ (match C
                                  [(counter index tt queue)
                                   tt]) minutes)]))

; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic

; Functie helper ce returneaza o lista cu tt-urile tuturor caselor de marcat
(define (get-tt counters)
  (if (null? counters)
      '()
      (cons (counter-tt (car counters)) (get-tt (cdr counters)))))

; Daca tt al casei curente este egal cu minimul (folosind functia min din Racket) din lista returnata de
; get-tt atunci returneaza perechea ceruta intre index si tt
; altfel, cauta in continuare
; apply -> pune o lista ca argumente in functia ceruta
(define (min-tt counters)
  (if (null? counters)
      '()
      (if (equal? (counter-tt (car counters)) (apply min (get-tt counters)))
          (cons  (counter-index (car counters)) (counter-tt (car counters)))
          (min-tt (cdr counters)))))
  
; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.

; Folosind append adaug persoana la finalul cozii, iar apoi folosind functia tt+
; copiez o noua structura avand tt-ul modificat corect si queue-ul anterior
                      ; stanga - update la tt                ; dreapta - update la queue
(define (add-to-counter C name n-items)
  (struct-copy counter (tt+ C n-items) [queue (append (counter-queue C) (list (cons name n-items)))]))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește

; Functie helper care cauta intr-o lista de case casa cu index-ul dat si o returneaza
(define (find-by-index counters index)
  (if (null? counters)
      '()
      (if (= (counter-index (car counters)) index)
          (car counters)
          (find-by-index (cdr counters) index))))

; Functie helper care primeste o casa updatata, sterge versiunea veche din lista, apoi o adauga pe cea noua la inceput
; si dupa le sorteaza dupa index pentru a mentine ordinea
(define (update-counter new-C counters)
  (sort (append (remove (find-by-index counters (counter-index new-C)) counters) (list new-C)) <= #:key counter-index))

; Folosind functiile de mai sus, se cauta casa la care se face request si se updateaza lista - ca un acumulator
(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes) (apply serve (cdr requests) (update-counter (tt+ (find-by-index (list C1 C2 C3 C4) index) minutes) (list C1 C2 C3 C4)))]
        [(list name n-items)
         (if (<= n-items ITEMS)
             (apply serve (cdr requests) (update-counter (add-to-counter (find-by-index (list C1 C2 C3 C4) (car (min-tt (list C1 C2 C3 C4)))) name n-items) (list C1 C2 C3 C4)))
             (apply serve (cdr requests) (append (list C1) (update-counter (add-to-counter (find-by-index (list C2 C3 C4) (car (min-tt (list C2 C3 C4)))) name n-items) (list C2 C3 C4)))))])))
