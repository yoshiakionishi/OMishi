(in-package :om)
;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;===============================================
; OMishi Functions: Number Operations
; As of June 30 2024
; - keep-within-value

(om::defmethod! keep-within-value ((value number) (upperboundary number))
 :initvals '(5 4)
  :indoc '("number to be evaluated" "upper boundary (nonzero and must be positive; negative value is turned positive)")
  :icon 2345312
  :doc "Keep Within Value

(Yoshiaki Onishi, June 30, 2024)

This function acts somewhere between 'mod' and 'rem' functions. 

First inlet accepts a number to be evaluated against the number put in the second inlet as the upper boundary.
Second inlet accepts a number that serves as an upper boundary number. This number must be non-zero and positive; a negatve value will be turned positive. 

Examples: 

First inlet 5; second inlet 3:

0 1 2 3 4 5 6 7 8 9 10...
0 1 2 0 1 2 0 1 2 0 1 ...

Out: 2

First inlet -5; second inlet 3

-5 -4 -3 -2 -1 0 1 2 3 4
 1  2  0  1  2 0 1 2 0 1...

Out: 1
"

(setf upperboundary (abs upperboundary))
(if (= upperboundary 0) (setf upperboundary 1))


(cond 
    ((and (>= value 0) (<= value (- upperboundary 1)))
    value)
    
    ((> value (- upperboundary 1)) 
    (loop while (> value (- upperboundary 1))
    do (setf value (- value upperboundary))
    collect value
    finally (return value)
    ))    

    ((< value 0)
    (loop while (< value 0)
    do (setf value (+ value upperboundary))
    collect value 
    finally (return value)
    ))

)
)


        
