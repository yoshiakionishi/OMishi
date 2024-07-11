(in-package :om)
;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;===============================================
; OMishi Functions: Number Generators
; As of June 30 2024
; - srn
; - zigzag-arithm-ser
; As of July 2 2024
; - euclid-rhythm-binary
; As of July 6 2024
; - dejong
; - dejong-svensson
; - lorenz

;===============================================


(om::defmethod! srn ((firstCondition number) (secondCondition number) (thirdCondition number))
 :initvals '(20 1 5)
  :indoc '("How many random numbers to generate?" "Minimum value" "Maximum value")
  :icon 2345312
  :doc "SRN: Stefan's Random Numbers

(Yoshiaki Onishi, June 30, 2024)

SRN generates a series of random numbers with one constraint: once a number appears, it cannot reappear for at least 2 iterations. That is:

BAD: 3 3 9 8 5 . . . = 3 is immediately followed by another 3. 
BAD: 3 7 3 8 9 . . . = 3 reappears with one non-3 number in between.
GOOD: 3 7 9 3 1 . .  . = Between the two 3s there are two numbers. 

This is a function proposed by Stefan Beyer in the spring of 2024. I built the original code on Javascript for Max, then I rewrote it in Common Lisp to include in the OMishi Library."

(setq   a firstCondition 
        b secondCondition 
        c thirdCondition
)
(setq srn-list
    (loop for a from (1- (+ a 2)) downto 0
        collect (+ b (random (- (+ c 1) b)))
    )

)

(setq srn-list-two
    (loop for x from 2 to (- (length srn-list) 1)
        do (loop until (not 
                            (or (= (nth x srn-list) (nth (- x 2) srn-list))
                                (= (nth x srn-list) (nth (- x 1) srn-list))
                            )
                        )
            do (setf (nth x srn-list) (+ b (random (- (+ c 1) b)))))
    collect (nth x srn-list)

    )
)

)

;===============================================


(om::defmethod! zigzag-arithm-ser ((startnumber number) (listofstep list) (stepvalue-within-list number) (stepvalue-per-list number))
 :initvals '(0 '(2 3 4 3 2) 2 3)
  :indoc '("starting number of the arithmetic series" "list of length(s) of sub-series" "step amount within sub-series" "step amount of one sub-series to the next")
  :icon 2345312
  :doc "Zigzag Arithmetic Series 

(Yoshiaki Onishi, June 30, 2024)

Building on the OM function 'arithm-ser,' This function creates an arithmetic series of an arithmetic series. 

- First inlet defines the number with which the arithmetic series begins.
- Second inlet, taking a list of number, defines the length of each arithmetic sub-series.
- Third inlet defines the amount of step within the sub-series, much like the third inlet of 'arithm-ser' function.
- Fourth inlet defines the amount of step from the first number of one sub-series to that of the next."
;   For those interested, here was the code in place prior to Version 0.6. This did not allow negative integers/zero on the last two inlets because of the nature of arithm-ser function. 
;   (loop   for x in listofstep 
;           for startnum from 0
;       collect (arithm-ser (+ startnumber (* startnum stepvalue-per-list)) 
;                      (+ (+ startnumber (* startnum stepvalue-per-list)) (* stepvalue-within-list (- x 1))) 
;                            stepvalue-within-list)
;
;   )
  (if   (= stepvalue-within-list 0)
        (loop for x in listofstep
        collect (loop repeat x collect startnumber)
        )
  
  (loop for x in listofstep ; 2 of (2 3 4 3 2)
        for startnum from startnumber by stepvalue-per-list      
        collect 
                (loop for n from 0 to (- x 1) 
                    collect (+ startnum (* n stepvalue-within-list))
                )
  )
  )
  
)

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

Click on the function and push t to see an example patch.

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
(om::defmethod! dejong ((a number) (b number) (c number) (d number) (iter number) (initskip number))
 :initvals '(1.641 1.902 0.316 1.525 100 0)
  :indoc '("value a" "value b" "value c" "value d" "number of iterations" "how many to skip initially")
  :icon 2345312
  :doc "Peter de Jong Attractor

(Yoshiaki Onishi, July 6, 2024)

dejong follows the principle of the Peter de Jong Attractor. Its equation is:

xn+1 = sin(a yn) - cos(b xn)
yn+1 = sin(c xn) - cos(d yn)

First four inlets accept values a through d. 
Fifth inlet accepts a value of iterations.
Sixth inlet accepts a value of initial skip. When set to x>0, the function will have skipped x times before outputting the value.

The function outputs a list with two sublists: ( (list of x values) (list of y values) )

Click on the function and push t to see an example patch.

For details, see: https://paulbourke.net/fractals/peterdejong/
"
(setq   x   0
        y   0
        newx    0
        newy    0
        )

; the following is performed only when the initial skip value is more than 0
(when (> initskip 0)
(loop for i from 1 to initskip
    do  (setq   newx   (-  (sin (* a y))    (cos (* b x)))
                newy   (-  (sin (* c x))    (cos (* d y)))
                x newx
                y newy
        )
)
)

(loop for i from 1 to iter
    do  (setq   newx   (-  (sin (* a y))    (cos (* b x)))
                newy   (-  (sin (* c x))    (cos (* d y)))
                x newx
                y newy
        )
    collect newx into x-list
    collect newy into y-list
    finally (return (list x-list y-list))
)
)

;===============================================
(om::defmethod! dejong-svensson ((a number) (b number) (c number) (d number) (iter number) (initskip number))
 :initvals '(1.40 1.56 1.40 -6.56 100 0)
  :indoc '("value a" "value b" "value c" "value d" "number of iterations" "how many to skip initially")
  :icon 2345312
  :doc "Peter de Jong Attractor: Johnny Svensson Variation

(Yoshiaki Onishi, July 6, 2024)

dejong-svensson follows the principle of the alternate version (contributed by Johnny Svensson) of the Peter de Jong Attractor. Its equation is:

xn+1 = d sin(a xn) - sin(b yn)
yn+1 = c cos(a xn) + cos(b yn)

First four inlets accept values a through d. 
Fifth inlet accepts a value of iterations.
Sixth inlet accepts a value of initial skip. When set to x>0, the function will have skipped x times before outputting the value.

The function outputs a list with two sublists: ( (list of x values) (list of y values) )

Click on the function and push t to see an example patch.

For details, see: https://paulbourke.net/fractals/peterdejong/
"
(setq   x   0
        y   0
        newx    0
        newy    0
        )

; the following is performed only when the initial skip value is more than 0
(when (> initskip 0)
(loop for i from 1 to initskip
    do  (setq   newx   (-  (* d (sin (* a x)))    (sin (* b y)))
                newy   (+  (* c (cos (* a x)))    (cos (* b y)))
                x newx
                y newy
        )
)
)

(loop for i from 1 to iter
    do  (setq   newx   (-  (* d (sin (* a x)))    (sin (* b y)))
                newy   (+  (* c (cos (* a x)))    (cos (* b y)))
                x newx
                y newy
        )
    collect newx into x-list
    collect newy into y-list
    finally (return (list x-list y-list))
)
)


;===============================================
(om::defmethod! lorenz ((a number) (b number) (c number) (x number) (y number) (z number) (h number) (iter number) (initskip number))
 :initvals '(10 28 2.6666668 0.1 0 0 0.01 1000 0)
  :indoc '("value a" "value b" "value c" "initial value x" "initial value y" "initial value z" "step size" "number of iterations" "how many to skip initially")
  :icon 2345312
  :doc "Lorenz Attractor

(Yoshiaki Onishi, July 6, 2024)

lorenz follows the principle of the Lorenz Attractor in three dimensions. Its equation is:

dx / dt = a (y - x)
dy / dt = x (b - z) - y
dz / dt = xy - cz

Expressed otherwise:

xn+1 = x + h * a * (y - x);
yn+1 = y + h * (x * (b - z) - y)
zn+1 = z + h * (x * y - c * z)


First three inlets accept values a through c.
Inlets 4~6 accept initial values x, y, and z. At least one of the three values must be nonzero. 
Inlet 7 accepts step size h. Normally the value is between 0.1 and 0.001.
Inlet 8 accepts a value of iterations.
Inlet 9 accepts a value of initial skip. When set to x>0, the function will have skipped x times before outputting the value.
The function outputs a list with three sublists: ( (list of x values) (list of y values) (list of z values))

Click on the function and push t to see an example patch.

For details, see: https://paulbourke.net/fractals/lorenz/
"
(setq   
        newx    0
        newy    0
        newz    0
        )

; the following is performed only when the initial skip value is more than 0
(when (> initskip 0)
(loop for i from 1 to initskip
    do  (setq   newx    (+ x (* h (* a (- y x))))
                newy    (+ y (* h (- (* x (- b z)) y)))
                newz    (+ z (* h (- (* x y) (* c z))))
                x newx
                y newy
                z newz
        )
)
)

(loop for i from 1 to iter
    do  (setq   newx    (+ x (* h (* a (- y x))))
                newy    (+ y (* h (- (* x (- b z)) y)))
                newz    (+ z (* h (- (* x y) (* c z))))
                x newx
                y newy
                z newz
        )
    collect newx into x-list
    collect newy into y-list
    collect newz into z-list
    finally (return (list x-list y-list z-list))
)
)