#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index open tt et queue) #:transparent)

; TODO (0p)
; Aveți libertatea să vă structurați programul cum doriți
; (dar cu restricțiile de mai jos), astfel încât
; funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - va exista în continuare funcția (empty-counter index)
; - veți reprezenta cozile folosind noul TDA queue
(define (empty-counter index)
  (make-counter index #t 0 0 empty-queue))
  
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
  (serve-helper requests fast-counters slow-counters '()))

(define (serve-helper requests fast-counters slow-counters departed)
  (if (null? requests)
      (let* ((all-counters (append fast-counters slow-counters))
             (non-empty (filter (lambda (c) 
                                  (not (queue-empty? (counter-queue c)))) 
                                all-counters))
             (sorted (sort non-empty (lambda (a b) 
                                       (< (counter-index a) (counter-index b)))))
             (result (map (lambda (c) 
                            (cons (counter-index c) (counter-queue c))) 
                          sorted)))
        (cons departed result))
      
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

        ((list 'open index)
         (serve-helper
          (cdr requests)
          (update (lambda (c) (make-counter (counter-index c)
                                            #t
                                            (counter-tt c)
                                            (counter-et c)
                                            (counter-queue c)))
                  fast-counters
                  index)
          (update (lambda (c) (make-counter (counter-index c)
                                            #t
                                            (counter-tt c)
                                            (counter-et c)
                                            (counter-queue c)))
                  slow-counters
                  index)
          departed))

        ((list 'close index)
         (let* ((all-counters (append fast-counters slow-counters))
                (target (car (filter (lambda (c) (= (counter-index c) index)) all-counters)))
                (q (counter-queue target))
                (all-clients (if (queue-empty? q)
                                 '()
                                 (letrec ((loop (lambda (qq acc)
                                                  (if (queue-empty? qq)
                                                      (reverse acc)
                                                      (loop (dequeue qq)
                                                            (cons (top qq) acc))))))
                                   (loop q '()))
                                 )
                             )
                (remaining (if (null? all-clients) '() (cdr all-clients)))
                (new-queue (if (null? all-clients)
                               empty-queue
                               (enqueue (car all-clients) empty-queue)))
                (new-fast (update (lambda (c)
                                    (make-counter (counter-index c)
                                                  #f
                                                  (counter-tt target)
                                                  (counter-et target)
                                                  new-queue))
                                  fast-counters index))
                (new-slow (update (lambda (c)
                                    (make-counter (counter-index c)
                                                  #f
                                                  (counter-tt target)
                                                  (counter-et target)
                                                  new-queue))
                                  slow-counters index)))
           (serve-helper
            (append (map (lambda (client) (list (car client) (cdr client))) remaining)
                    (cdr requests))
            new-fast new-slow departed)
           )
         )
        

        ((list name n-items)
         (let ((open-fast (filter counter-open fast-counters))
                (open-slow (filter counter-open slow-counters)))
           
           (if (and (<= n-items ITEMS)
                    (not (null? open-fast))
                    (or (null? open-slow)
                        (<= (cdr (min-tt open-fast))
                            (cdr (min-tt open-slow)))))
               (serve-helper (cdr requests)
                             (update (lambda (c) ((add-to-counter name n-items) c))
                                     fast-counters
                                     (car (min-tt open-fast)))
                             slow-counters departed)
               (serve-helper (cdr requests)
                             fast-counters
                             (update (lambda (c) ((add-to-counter name n-items) c))
                                     slow-counters
                                     (car (min-tt open-slow)))
                             departed))))

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
                         (append departed new-departed))
           )
         )
      )
  ))

(define tt+
  (lambda C (lambda minutes
              (make-counter
               (counter-index (car C))
               (counter-open (car C))
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
               (counter-open (car C))
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
               (counter-open (car C))
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
               (counter-open (car C))
               (- (counter-tt (car C)) (car minutes))
               (counter-et (car C))
               (counter-queue (car C)))
              )
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

(define (min-tt counters)
  (if (null? counters)
      (cons -1 +inf.0)
      (min-tt-stack counters)))

(define (min-et counters)
  (if (null? counters)
      (cons -1 +inf.0)
      (min-et-stack counters)))

(define (update f counters index)

  (map (lambda (c)
         (if (= (counter-index c) index)
             (f c)
             c)
         )
       counters)
  )

(define ((add-to-counter name items) C)

  (make-counter
   (counter-index C)
   (counter-open C)
   (+ (counter-tt C) items)
   (if (queue-empty? (counter-queue C))
       (+ (counter-tt C) items)
       (counter-et C))
   (enqueue (cons name items) (counter-queue C) )
   )  
)

(define (get-tt-sum list result)

  (foldl (lambda (x acc) (+ acc (counter-tt x))) 0 list)

)

(define (ensure-helper average fast-counters slow-counters)
  (let ((open-all (filter counter-open (append fast-counters slow-counters))))
    (if (<= (/ (get-tt-sum open-all 0) (length open-all))
            average)
        slow-counters
        (ensure-helper average fast-counters
                       (append slow-counters
                               (list (empty-counter 
                                      (+ (counter-index (last slow-counters)) 1)))))
        )
    )
  )

(define (min-time et n-items)
  (if (= et 0)
      
      n-items
      et
      
      )
  )

(define (remove-first-from-counter C)
  (make-counter
   (counter-index C)
   (counter-open C)
   (- (counter-tt C) (counter-et C))
   (min-time 0 (if (queue-empty? ( dequeue (counter-queue C))) 0 (cdr (top ( dequeue (counter-queue C))))))
   (dequeue (counter-queue C))
   )
  )

(define ((pass-time-through-counter minutes) C)

  (if (and (= (counter-et C) 0) (queue-empty? (counter-queue C)))

       C
       (if (>= minutes (counter-tt C))
      
           (( tt- ((et- C) (counter-et C))) (counter-et C))
           (( tt- ((et- C) minutes)) minutes)
 
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
              (pass-time-all removed (- minutes step) new-departed))
            )
        )
      )
  )

