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
  (make-counter index 0 0 '()))


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

  (map (lambda (c) (if (= (counter-index c) index) (f c) c)) counters)

  )


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
  (lambda C (lambda minutes
              (make-counter
               (counter-index (car C))
               (+ (counter-tt (car C)) (car minutes))
               (counter-et (car C))
               (counter-queue (car C)))
              )
    )
  )

(define (checker-tt+ C minutes)
  ((tt+ C) minutes)

  )

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
  (lambda C (lambda minutes
              (make-counter
               (counter-index (car C))
               (counter-tt (car C))
               (+ (counter-et (car C)) (car minutes))
               (counter-queue (car C))) 
              )
    )
  )

(define (checker-et+ C minutes)
  ((et+ C) minutes)

  )

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
  (lambda C (lambda name (lambda n-items
                           (make-counter
                            (counter-index (car C))
                            (+ (counter-tt (car C)) (car n-items))
                            (if (null? (counter-queue (car C)))
                                (+ (counter-tt (car C)) (car n-items))
                                (counter-et (car C)))
                            (append (counter-queue (car C)) (list (cons (car name) (car n-items))))
                            )
                           )
              )
    )
  )

(define (checker-add-to-counter C name n-items)
  ( ( (add-to-counter C) name) n-items)

  )

(define (min-time et n-items)
  (if (= et 0)
      
      n-items
      
      et
      
      )
  )

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

(define (find-min-abstract f)

  (lambda (counters) (f counters))
  
  )

(define (min-tt-stack counters)

  (if(null? (cdr counters))

     (cons (counter-index (car counters)) (counter-tt (car counters)))

     (if(< (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))

        (cons (counter-index (car counters)) (counter-tt (car counters)))

        (if (and (= (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))
                 (< (counter-index (car counters)) (car (min-tt-stack (cdr counters)))))

            (cons (counter-index (car counters)) (counter-tt (car counters)))

            (min-tt-stack (cdr counters))

           )
        )
     )
)

(define (min-et-stack counters)
  
  (if (null? (cdr counters))
      
      (if (null? (counter-queue (car counters)))
          
          (cons (counter-index (car counters)) 0)
          (cons (counter-index (car counters)) (counter-et (car counters))))
      
      (if (null? (counter-queue (car counters)))
          
          (min-et-stack (cdr counters))
          
          (if (= (cdr (min-et-stack (cdr counters))) 0)
              
              (cons (counter-index (car counters)) (counter-et (car counters)))
              
              (if (< (counter-et (car counters)) (cdr (min-et-stack (cdr counters))))
                  
                  (cons (counter-index (car counters)) (counter-et (car counters)))
                  
                  (if (and (= (counter-et (car counters)) (cdr (min-et-stack (cdr counters))))
                           (< (counter-index (car counters)) (car (min-et-stack (cdr counters)))))
                      
                      (cons (counter-index (car counters)) (counter-et (car counters)))
                      (min-et-stack (cdr counters))
                      )
                  )
              )
          )
      )
  )

(define min-tt (find-min-abstract min-tt-stack))
(define min-et (find-min-abstract min-et-stack))

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

  (make-counter
               (counter-index C)
               (- (counter-tt C) (counter-et C))
               (min-time 0 (if (null? (cdr (counter-queue C))) 0 (cdr (cadr (counter-queue C)))))
               (cdr (counter-queue C))
               )
  )

#|
(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 '((remus . 6) (vivi . 4))))

(min-et (list (make-counter 2 113 100 '((ana . 100) (dan . 4) (gigi . 9)))))
(min-time 0 (if (null? (cdr (counter-queue C))) 0 (cdr (cadr (counter-queue C)))))
(counter-queue (make-counter 2 113 100 '((ana . 100) (dan . 4) (gigi . 9))))

 ;(remove-first-from-counter (counter 3 2 2 '((mara . 2))))

|#

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
      
      (append fast-counters slow-counters)
      
      (match (car requests)

        ((list 'delay index minutes)
         (serve
          (cdr requests)
          (update (lambda (c) ((et+ ((tt+ c) minutes)) minutes)) fast-counters index)
          (update (lambda (c) ((et+ ((tt+ c) minutes)) minutes)) slow-counters index))
         )

        
        ((list 'remove-first)
           (serve
            (cdr requests)
            (map (lambda (c)
                   (define idx (car (common-min min-et fast-counters slow-counters)))
                   (if (null? (counter-queue c))
                       c
                       (if (= (counter-index c) idx)
                           (remove-first-from-counter c)
                           c)
                       )
                   )
                 fast-counters)
            (map (lambda (c)
                   (define idx (car (common-min min-et fast-counters slow-counters))) 
                   (if (null? (counter-queue c))
                       c
                       (if (= (counter-index c) idx)
                           (remove-first-from-counter c)
                           c)
                       ))
                 slow-counters)
            )
         )

        ((list 'ensure average)

         (serve (cdr requests) fast-counters (ensure-helper average fast-counters slow-counters))
         
         )        
         
         ((list name n-items)
          (if (and (<= n-items ITEMS)
                   (or (null? slow-counters)
                       (<= (cdr (min-tt fast-counters))
                           (cdr (min-tt slow-counters)))))
              
              (serve (cdr requests)
                     (update (lambda (c) (((add-to-counter c) name) n-items))
                             fast-counters
                             (car (min-tt fast-counters)))
                     slow-counters)
              
              (serve (cdr requests)
                     fast-counters
                     (update (lambda (c) (((add-to-counter c) name) n-items))
                             slow-counters
                             (car (min-tt slow-counters))))
              )
          )
        
       )
  ))

(define (common-min f fast slow)
  (cond
    ((and (null? fast) (null? slow)) (cons -1 0))
    ((null? fast) (f slow))
    ((null? slow) (f fast))
    ((and (= (cdr (f fast)) 0) (= (cdr (f slow)) 0)) (cons -1 0))
    ((= (cdr (f fast)) 0) (f slow))
    ((= (cdr (f slow)) 0) (f fast))
    ((< (cdr (f fast)) (cdr (f slow))) (f fast))
    ((> (cdr (f fast)) (cdr (f slow))) (f slow))
    ((< (car (f fast)) (car (f slow))) (f fast))
    (else (f slow))))

(define (ensure-helper average fast-counters slow-counters)
  (if (<= (/ (+ (get-tt-sum fast-counters 0) (get-tt-sum slow-counters 0))
             (+ (length fast-counters) (length slow-counters)))
          average)
      
      slow-counters
      
      (ensure-helper average fast-counters
                     (append slow-counters
                             (list (empty-counter (+ (counter-index (last slow-counters)) 1)))
                     )
      )
   )
)

(define (get-tt-sum list result)

  (foldl (lambda (x acc) (+ acc (counter-tt x))) 0 list)

)
