(in-package :om)
;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;===============================================
; OMishi Functions: Number Generators
; As of August 3 2024
; - collatz
; As of July 24 2024
; - euclid-rhythm-binary (MOVED TO 'EUCLID')
; - dejong (MOVED TO 'ATTRACTORS-SELF-SIMILARITY')
; - dejong-svensson (MOVED TO 'ATTRACTORS-SELF-SIMILARITY')
; - lorenz (MOVED TO 'ATTRACTORS-SELF-SIMILARITY')
; - mandelbrot-imager (MOVED TO 'ATTRACTORS-SELF-SIMILARITY')
; - mandelbrot-calc (MOVED TO 'ATTRACTORS-SELF-SIMILARITY')
; As of June 30 2024
; - srn
; - zigzag-arithm-ser
; As of July 2 2024
; - euclid-rhythm-binary
; As of July 6 2024
; - dejong
; - dejong-svensson
; - lorenz
; As of July 16 2024
; - mandelbrot-imager
; - mandelbrot-calc

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
(om::defmethod! collatz ((startnumber number))
 :initvals '(9)
  :indoc '("starting number of the Collatz Conjecture")
  :icon 2345312
  :doc "Collatz

(Yoshiaki Onishi, August 3, 2024)

This function generates a series of numbers according to the rules set forth by the Collatz Conjecture:

- Starting with an arbitrary positive integer:
- - - If the number is even, the number is divided by 2.
- - - If the number is odd, the number is tripled, then 1 is added.

The function stops when the number arrives at 1. 

Example: 

(collatz 9) => (9 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
"
(if (and (numberp startnumber) (> startnumber 1))
    (progn  (setq x startnumber) 
            (loop 
                until (= x 1)
            if (oddp x)
                collect x into path
                and do (setq x (+ (* x 3) 1))
            else 
                collect x into path
                and do (setq x (/ x 2))
            finally (return (append path (cons 1 nil)))
            )
    )
    (print "input must be a positive integer.")
)
  
)
