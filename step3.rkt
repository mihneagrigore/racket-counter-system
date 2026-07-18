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
  (make-counter index 0 0 empty-queue)
  )

(define (update f counters index)

  (map (lambda (c)
         (if (= (counter-index c) index)
             (f c)
             c)
         )
       counters)
  )

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

(define et-
  (lambda C (lambda minutes
              (make-counter
               (counter-index (car C))
               (counter-tt (car C))
               (- (counter-et (car C)) (car minutes))
               (counter-queue (car C))) 
              )
    )
  )

(define tt-
  (lambda C (lambda minutes
              (make-counter
               (counter-index (car C))
               (- (counter-tt (car C)) (car minutes))
               (counter-et (car C))
               (counter-queue (car C)))
              )
    )
  )

(define ((add-to-counter name items) C)

  (make-counter
   (counter-index C)
   (+ (counter-tt C) items)
   (if (queue-empty? (counter-queue C))
       (+ (counter-tt C) items)
       (counter-et C))
   (enqueue (cons name items) (counter-queue C) )
   )  
)
                    

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
      
      (if (queue-empty? (counter-queue (car counters)))
          
          (cons (counter-index (car counters)) 0)
          (cons (counter-index (car counters)) (counter-et (car counters))))
      
      (if (queue-empty? (counter-queue (car counters)))
          
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

(define (remove-first-from-counter C)   ; testată de checker
  (make-counter
   (counter-index C)
   (- (counter-tt C) (counter-et C))
   (min-time 0 (if (queue-empty? ( dequeue (counter-queue C))) 0 (cdr (top ( dequeue (counter-queue C))))))
   (dequeue (counter-queue C))
   )
  )

(define (min-time et n-items)
  (if (= et 0)
      
      n-items
      et
      
      )
  )

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

  (if (and (= (counter-et C) 0) (queue-empty? (counter-queue C)))

       C
       (if (>= minutes (counter-tt C))
      
           (( tt- ((et- C) (counter-et C))) (counter-et C))
           (( tt- ((et- C) minutes)) minutes)
 
           )
       )
  )

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
  (serve-helper requests fast-counters slow-counters '()))

(define (serve-helper requests fast-counters slow-counters departed)
  (if (null? requests)
      (cons departed (append fast-counters slow-counters))
      
      (match (car requests)
        
        ((list 'delay index minutes)
         (let ((upd (lambda (c) ((et+ ((tt+ c) minutes)) minutes))))
           (serve-helper
            (cdr requests)
            (update upd fast-counters index)
            (update upd slow-counters index)
            departed)
           )
         )

        ((list 'ensure average)
         (serve-helper (cdr requests) fast-counters
                       (ensure-helper average fast-counters slow-counters)
                       departed)
         )

        ((list name n-items)
         (if (and (<= n-items ITEMS)
                  (or (null? slow-counters)
                      (<= (cdr (min-tt fast-counters))
                          (cdr (min-tt slow-counters)))))
             (serve-helper (cdr requests)
                           (update (lambda (c) ((add-to-counter name n-items) c))
                                   fast-counters
                                   (car (min-tt fast-counters)))
                           slow-counters departed)
             (serve-helper (cdr requests)
                           fast-counters
                           (update (lambda (c) ((add-to-counter name n-items) c))
                                   slow-counters
                                   (car (min-tt slow-counters)))
                           departed)
             )
         )

        (minutes
         (let* ((all-counters (append fast-counters slow-counters))
                (all-result (pass-time-all all-counters minutes '()))
                (new-all (car all-result))
                (new-departed (cdr all-result))
                (n-fast (length fast-counters))
                (new-fast (take new-all n-fast))
                (new-slow (drop new-all n-fast)))
           (serve-helper (cdr requests)
                         new-fast
                         new-slow
                         (append departed new-departed))))
        )
      )
  )

(define (pass-time-all counters minutes departed)
  (if (= minutes 0)
      
      (cons counters departed)
      
      (let* ((non-empty (filter (lambda (c) 
                                  (not (queue-empty? (counter-queue c)))) 
                                counters))
             (min-et-val (if (null? non-empty)
                             minutes  
                             (cdr (min-et counters)))))
        
        (if (or (null? non-empty) (> min-et-val minutes))
            
            (cons (map (lambda (c) 
                         ((pass-time-through-counter minutes) c)) 
                       counters)
                  departed)
            
            (let* ((step (if (= min-et-val 0) 0 min-et-val))
                   (advance (map (lambda (c) 
                                    ((pass-time-through-counter step) c)) 
                                  counters))
                   (exiting (filter (lambda (c)
                                      (and (= (counter-et c) 0)
                                           (not (queue-empty? (counter-queue c)))))
                                    advance))
                   (new-departed (append departed
                                     (map (lambda (c)
                                            (cons (counter-index c)
                                                  (car (top (counter-queue c)))))
                                          exiting)))
                   (removed (map (lambda (c)
                                    (if (and (= (counter-et c) 0)
                                             (not (queue-empty? (counter-queue c))))
                                        (remove-first-from-counter c)
                                        c))
                                  advance)))
              (pass-time-all removed (- minutes step) new-departed))))
      )
  )

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
