#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un magazin.
;; C1 acceptă doar clienți care au cumpărat maxim ITEMS produse
;; (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întârzieri (delay).
;; La un moment dat, la fiecare casă există
;; 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă
;; timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei +
;; întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO 1 (10p)
; Implementați o funcție care întoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat
; o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))


; TODO 2 (10p)
; Implementați o funcție care crește tt-ul unei case
; cu un număr dat de minute.
(define (tt+ C minutes)

  (make-counter
   (counter-index C)
   (+ (counter-tt C) minutes)
   (counter-queue C))

  )


; TODO 3 (20p)
; Implementați o funcție care primește o listă nevidă 
; de case și întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt,
; este preferată casa cu indexul cel mai mic
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe coadă.

(define (min-tt counters)
  (find-min-tt (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))) 2 (counter-index (car counters))))

(define (find-min-tt counters min-pair index real_index)
  
  (if (null? counters)
      min-pair

          (if ( < (counter-tt (car counters)) (cdr min-pair))
              (find-min-tt (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))) (+ 1 index) (counter-index (car counters)))

              (if (and (= (counter-tt (car counters)) (cdr min-pair)) (< (counter-index (car counters)) real_index))
                  (find-min-tt (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))) (+ 1 index) (counter-index (car counters)))
                  (find-min-tt (cdr counters) min-pair (+ 1 index) (counter-index (car counters)))
                  )
              )
      )
)


; TODO 4 (20p)
; Implementați aceeași funcționalitate de mai sus,
; cu recursivitate pe stivă.
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe stivă.
(define (min-tt-stack counters)

  (if (null? (cdr counters))

      (cons (counter-index (car counters)) (counter-tt (car counters)))

      (if (< (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))

          (cons (counter-index (car counters)) (counter-tt (car counters)))

          (if ( and (= (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))
                    (< (counter-index (car counters)) (car (min-tt-stack (cdr counters)))) )
              
              (cons (counter-index (car counters)) (counter-tt (car counters)))

              (min-tt-stack (cdr counters))

              )
          )
      )
  )

#|
(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(min-tt-stack (list (tt+ C1 1) (tt+ C2 32) C4))
|#

; TODO 5 (10p)
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei,
; n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii
; (name . n-items) la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)

   (make-counter
    (counter-index C)
    ( + (counter-tt C)  n-items)
    (append (counter-queue C) (list (cons name n-items)))

    )
)


; TODO 6 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - așază persoana <name> la coadă la o casă
; - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (conform logicii implementate de min-tt)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)
  
  ; Puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele
  ;   requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; Puteți să vă definiți funcții ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți testa fiecare funcție imediat ce ați implementat-o
  ; Nu este obligatoriu să definiți funcții ajutătoare.

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes)

         (cond
           (
                (= (counter-index C1) index)
                (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4)
           )
           
           (
                (= (counter-index C2) index)
                (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4)
           )
           
           (
                (= (counter-index C3) index)
                (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4)
           )
           
           (
                (= (counter-index C4) index)
                (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes))
           ) 
         )

         ]
        
        [(list name n-items)

         (if(> n-items ITEMS)

            ( let ((min (car (min-tt (list C2 C3 C4)))))
               (cond
                 (
                  (= min (counter-index C2))
                  (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4)
                  )

                 (
                  (= min (counter-index C3))
                  (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4)
                  )

                 (
                  (= min (counter-index C4))
                  (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))
                  )
              
                 ))

            ( let ((min (car (min-tt (list C1 C2 C3 C4)))))
               (cond
                 (
                  (= min (counter-index C1))
                  (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4)
                  )

                 (
                  (= min (counter-index C2))
                  (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4)
                  )

                 (
                  (= min (counter-index C3))
                  (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4)
                  )

                 (
                  (= min (counter-index C4))
                  (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))
                  )
              
                 ))
            )
         ]

        )))
