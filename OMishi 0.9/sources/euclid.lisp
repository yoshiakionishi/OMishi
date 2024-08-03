(in-package :om)
;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;===============================================
; OMishi Functions: Euclid
; As of July 24 2024
; - euclid-rhythm-binary
; - euclid-distance
; - euclid-distance-3d
;===============================================


(om::defmethod! euclid-rhythm-binary ((intA number) (intB number) (intC number))
 :initvals '(8 5 -2)
  :indoc '("Total Pulse-Space (n)" "Total Amount of Pulses (k)" "Rotation Values (r)")
  :icon 2345312
  :doc "Euclid Rhythm Binary (Euclidean Rhythm Generator)

(Yoshiaki Onishi, July 2, 2024; based on my Javascript code for Max (July 2020))

This function generates a series of 1 and 0 with given parameters, following the principle of the Euclidean Algorithm[1].

This function takes three values:

- First inlet takes the Total Pulse-Space (n)
- Second inlet takes the Total Amount of Pulses (k) (NB: the value of non-pulses (m) (i.e. rests) are calculated within the code, as m = n-k.)
- Third inlet takes the Rotation Values (r)
    + = shifts the starting point clockwise
    - = shifts the starting point counterclockwise)
     When the rotation value equals or exceeds (n), it loops back to 
     the value within the (n). (e.g. When n = 5 and r = 7, the adjusted r
     is 2.)

It outputs:
- EUCLID(k,n) (with appropriate rotation-value adjustment)
in a list of 1 (pulses) and 0 (non-pulses)
(NB: Bjorklund's stopping rule is applied. (see Toussaint, page 2 (or 3 for extended version))


--------
[1] Toussaint, Godfried. *The Euclidean Algorithm Generates Traditional Musical Rhythms.* Paper presented at the BRIDGES: Mathematical Connections in Art, Music and Science, Banff, Alberta, Canada, 2005.
"
	
; Ported from the original Javascript by Yoshiaki Onishi
; July 1 2024 


; === INPUT FUNCTION ===

(setq   totaltime intA
        beats intB
        rotavalue intC

)
; === SAFETY MECHANISM ===

(if (< totaltime 0) 
    ;(and 
    ; (print "Total time value was negative. I turned it positive.")
    (setq totaltime (abs totaltime))
    ;)
)

(if (< beats 0) 
    ;(and 
    ;    (print "Beat value was negative. I turned it positive.")
        (setq beats (abs beats))
    ;)
)

(setq   nonbeats (- totaltime beats))

; === PRIMARY DEFINITIONS ===

(cond 
    ((< nonbeats 0)
        ;(and 
            (setq   beats (create-list totaltime 1)
                item1 beats
                item2 (list )
                item2length (length item2)
                item3 (list 'n)
                item3length (length item3)
            )
            ;(print "Nonbeats is less than 0. Creating the all-beat situation based on totaltime value...") 
        ;)
    ) ; This prevents nonbeat value from going under 0 and freeze.

    ((= nonbeats 0)
        (setq   beats (create-list beats 1)
                item1 beats
                item2 (list )
                item2length (length item2)
                item3 (list 'n)
                item3length (length item3)
        )
    ;=    (print (list "Beats occupy the entire beat space" (list item1 item2 item3 item1length item2length item3length)))
    )

    ((= nonbeats totaltime)
        (setq nonbeats (create-list nonbeats 0)
                item1 nonbeats
                item2 (list )
                item2length (length item2)
                item3 (list 'n)
                item3length (length item3); (0 0 0 0 0 0 0 .... )
        )
    ;    (print (list "Rests occupy the entire beat space" (list item1 item2 item3 item1length item2length item3length)))

    )

    ((and (> nonbeats 0) (not (= nonbeats totaltime)))
     (and   (setq beats (create-list beats 1) nonbeats (create-list nonbeats 0) ) 
            (cond   
                (
                    (=  (length beats) (length nonbeats))
                        (setq   item1length (length beats) ; (1 1 1) (0 0 0)
                                    item1 (loop for x from 1 to item1length collect (car beats))
                                    item2length (length nonbeats)                                        
                                    item2 (loop for x from 1 to item2length collect (car nonbeats))
                                    item3 (list 'n)
                                    item3length (length item3)
                            )
;                            (print (list "Beats and nonbeats have the same amount" (list item1 item2 item3 item1length item2length item3length)))
                )
                (
                    (< (length beats) (length nonbeats)) ; (1 1 1) (0 0 0 0 0)
                            (setq   item1length (length beats) ; (1 1 1 1 1) (0 0 0)
                                    item1 (loop for x from 1 to item1length collect (car beats))
                                    item2length (length nonbeats)                                        
                                    item2 (loop for x from 1 to item2length collect (car nonbeats))
                                    item3length (- (length nonbeats) (length beats))
                                    item3 (loop for x from 1 to item3length collect (car nonbeats)) 
                                    item2length item1length
                                    item2 (loop for x from 1 to item2length collect (car nonbeats)); (1 1 1) (0 0 0) (1 1)
                            )
 ;                           (print (list "Nonbeats are more than Beats" (list item1 item2 item3 item1length item2length item3length)))
                )
                (
                    (> (length beats) (length nonbeats)) 
                            (setq   item1length (length beats) ; (1 1 1 1 1) (0 0 0)
                                    item1 (loop for x from 1 to item1length collect (car beats))
                                    item2length (length nonbeats)                                        
                                    item2 (loop for x from 1 to item2length collect (car nonbeats))
                                    item3length (- (length beats) (length nonbeats))
                                    item3 (loop for x from 1 to item3length collect (car beats)) 
                                    item1length item2length
                                    item1 (loop for x from 1 to item1length collect (car beats)); (1 1 1) (0 0 0) (1 1)
                            )
;                            (print (list "Beats are more than Nonbeats"(list item1 item2 item3 item1length item2length item3length)))
                )
            )
     ))
)

; === PROCESSING 1 of 2 ===

(loop until (not (typep (car item3) 'number))
    if (and (= item1length item2length) (>= item3length item1length)) ;(1 1 1 1 1) (0 0 0 0 0) (1 1)
            do (loop until (< item3length item1length)
                do  ;(and    
                    (setq  item1 (mat-trans (list item1 item2)) ; (1 0 1 0 1 0 1 0 1 0) (0 0 0 0 0) (1 1)
                                item1length (length item1)
                                item2 item3
                                item2length (length item1)
                                item3length (- (length item3) item2length)
                                item2 (loop for x from 1 to item2length collect (car item3))
                                item3 (loop for x from 1 to item3length collect (car item3))
                        )
;                            (print (list "first process" (list item1 item2 item3 item1length item2length item3length))))
                )
    if (< item3length item1length) ;(1 1 1 1 1) (0 0 0 0 0) (1 1)
        do ;(and 
                (setq   item1 (mat-trans (list item1 item2)) ; (1 0 1 0 1 0 1 0 1 0) (0 0 0 0 0) (1 1)
                        item1length (length item1)
                        item2 item3 ;(1 0 1 0 1 0 1 0 1 0) (1 1) (1 1)
                        item2length (length item2)
                        item3 (list 'n)
                        
                        item3length (length item3) 
                )
;                    (print (list "second process" (list item1 item2 item3 item1length item2length item3length))))
)
; === PROCESSING 2 of 2 ===

(loop until (< item2length 3)
                do  (setq   item3length (- item1length item2length)
                            item3 (loop for x from 1 to item3length collect (car item1)) ; this serves as a placeholder, to be moved ultimately to item2
                            item1length (length item2)
                            item1 (loop for x from 1 to item1length collect (car item1))
                            item1 (mat-trans (list item1 item2))
                            item1length (length item1)
                            item2 item3
                            item2length (length item2)
                            item3 (list 'n)
                            item3length 0
                    )
;                    (print (list "still working on it..." (list item1 item2 item3 item1length item2length item3length)))
                            
                            collect item1
                            
)
; === FINAL PRESENTATION ===
   
(rotate (remove nil (flat (mat-trans (list item1 item2)))) rotavalue)

)

        

;===============================================


(om::defmethod! euclid-distance ((list1 list) (mode symbol)) 
 :initvals '('((0 0) (4 3) (1 -1)) 'xyxy ) 
  :indoc '("list of coordinates" "format")
  :menuins '((1 (("((x1 y1) (x2 y2)...)" 'xyxy ) ("((x1 x2 x3...)(y1 y2 y3...))" 'xxyy ))))
  :icon 5678645
  :doc "euclid-distance

(Yoshiaki Onishi, July 16, 2024)

This function computes the Euclidean distance between two points in 2D Euclidean Space.

It can accept lists of points in two different formats:

((x1 y1) (x2 y2) (x3 y3)...) or ((x1 x2 x3...)(y1 y2 y3...)). 

When there are more than two points, this function computes each of the Euclidean distances between two successive points. 

For Euclidean distance between two points in 3D Euclidean Space, use euclid-distance-3d.

"

(if (eq mode 'xyxy)
    (setq process 0)
    (setq process 1)
)

(if (eq process 0) 
        (loop for n from 1 to (- (length list1) 1)
            collect (sqrt 
                        (+  (expt (abs (- (first (nth n list1)) (first (nth (- n 1) list1)))) 2)
                            (expt (abs (- (second (nth n list1)) (second (nth (- n 1) list1)))) 2)
                        )
                    )
        )
        (loop for n from 1 to (- (length (first list1)) 1)
            collect  (sqrt 
                        (+  (expt (abs (- (nth n (first list1)) (nth (- n 1) (first list1)))) 2)
                            (expt (abs (- (nth n (second list1)) (nth (- n 1) (second list1)))) 2)
                        )
                    )
        )    


)

)
;===============================================


(om::defmethod! euclid-distance-3d ((list1 list) (mode symbol)) 
 :initvals '('((0 0) (4 3) (1 -1)) 'xyzxyzxyz ) 
  :indoc '("list of coordinates" "format")
  :menuins '((1 (("((x1 y1 z1) (x2 y2 z2)...)" 'xyzxyzxyz ) ("((x1 x2...)(y1 y2...)(z1 z2...))" 'xxxyyyzzz ))))
  :icon 5678645
  :doc "euclid-distance-3d

(Yoshiaki Onishi, July 15, 2024)

This function computes the Euclidean distance between two points in 3D Euclidean Space.

It can accept lists of points in two different formats:

((x1 y1 z1) (x2 y2 z2)...) or ((x1 x2...)(y1 y2...)(z1 z2...)). 

When there are more than two points, this function computes each of the Euclidean distances between two successive points. 

For Euclidean distance between two points in 2D Euclidean Space, use euclid-distance-2d.

"

(if (eq mode 'xyzxyzxyz)
    (setq process 0)
    (setq process 1)
)

(if (eq process 0) 
        (loop for n from 1 to (- (length list1) 1)
            collect (sqrt 
                        (+  (expt (abs (- (first (nth n list1)) (first (nth (- n 1) list1)))) 2)
                            (expt (abs (- (second (nth n list1)) (second (nth (- n 1) list1)))) 2)
                            (expt (abs (- (third (nth n list1)) (third (nth (- n 1) list1)))) 2)
                        )
                    )
        )
        (loop for n from 1 to (- (length (first list1)) 1)
            collect  (sqrt 
                        (+  (expt (abs (- (nth n (first list1)) (nth (- n 1) (first list1)))) 2)
                            (expt (abs (- (nth n (second list1)) (nth (- n 1) (second list1)))) 2)
                            (expt (abs (- (nth n (third list1)) (nth (- n 1) (third list1)))) 2)

                        )
                    )
        )    


)

)

