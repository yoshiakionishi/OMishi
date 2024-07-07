(in-package :om)
;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;===============================================
; OMishi Functions: List Operations
; As of June 30 2024
; - fraction-maker
; - 0to-1
; - -1to0
; - find-duplicate-number
; - num->10
; - 10->num

;===============================================

(om::defmethod! fraction-maker ((num list) (den list))
 :initvals '('(2 3 4 3 6) '(3 5 8 2 3))
  :indoc '("list of numerators" "list of denominators")
  :icon 4567534
  :doc "Fraction Maker 

(Yoshiaki Onishi, June 30, 2024)

Using the two lists, one containing numerators and another containing denominators, this function combines them to create fractions.

First inlet takes a list of numerators.
Second inlet takes a list of denominators."
  (loop for int1 in num
	for int2 in den
      collect (rationalize (/ int1 int2))
  )

)
;===============================================


(om::defmethod! 0to-1 ((list1 list))
 :initvals '('(1 0 1 0 0 1))
  :indoc '("list")
  :icon 5678645
  :doc "0 to -1 

(Yoshiaki Onishi, June 30, 2024)

All 0s in the given list turn -1. I built this function for the purpose of rhythm tree, where a negative value is regarded as a rest. "
	(substitute -1 0 list1)


)
;===============================================


(om::defmethod! -1to0 ((list1 list))
 :initvals '('(1 0 1 0 0 1))
  :indoc '("list")
  :icon 5678645
  :doc "-1 to 0 

(Yoshiaki Onishi, June 30, 2024)

All -1s in the given list turn 0. I built this function for the purpose of using the rhythm tree data for my '1d-cellular-automata' function. "
 (substitute 0 -1 list1)


)

;===============================================



(om::defmethod! find-duplicate-number ((list1 list) (num1 number))
 :initvals '('(1 3 5 4 1 3 2 1) 3 )
  :indoc '("list of numbers" "number to search")
  :icon 5678645
  :doc "Find Duplicate 

(Yoshiaki Onishi, June 30, 2024)

This function looks for a number within a given list, then gives the result in a list of 'nth' address.

Example: (find-duplicate-number '(1 3 5 4 1 3 2 1) 3) => (1 5)"
(setq x 0)
(loop for i in list1
	if (equal i num1) 
	collect x
	and do (setq x (+ x 1))
	else do (setq x (+ x 1)))
)



;===============================================

(om::defmethod! 10->num ((list1 list))
 :initvals '('(0 0 1 0 0 1 0 1)) 
  :indoc '("list of numbers to turn into 1 and 0")
  :icon 5678645
  :doc "10->num 

(Yoshiaki Onishi, July 2, 2024)

This function turns a list of binary numbers into a list of intervals. Somewhat similar to x->dx function. However, the intervals are computed by the distance of a 1 from another. 

For example, (1 0 0 0 0) becomes: 5

If a list begins with a 0 or a series of 0s, the amount of 0s are expressed in a number in a Level-2 list.

For example, a list (0 0 0 0 1 0 0 1 0) becomes: ((4) 3 2)."

(setq onecounter    (loop for x from 0 to (- (length list1) 1)
                        if (= (nth x list1) 1)
                        collect x
                    )
)
(if (not (= (nth 0 onecounter) 0))
    (and    (setq   newonecounter (list (nth 0 onecounter))
                    onecounter (flat (list onecounter (length list1)))
                    newonecounter (cons newonecounter (x->dx onecounter))
            )
            (car (list newonecounter))
    )


    (and    (setq   onecounter (flat (list onecounter (+ (length list1) (nth 0 onecounter)))))
            (x->dx onecounter)
    ) 
)
)

;===============================================

(om::defmethod! num->10 ((list1 list))
 :initvals '('((3) 5 4 1 3) )
  :indoc '("list of numbers to turn into 1 and 0")
  :icon 5678645
  :doc "num->10 

(Yoshiaki Onishi, July 2, 2024)

This function turns a list of numbers into a list of binary numbers, where the amount of the original list is expressed by 1, followed by 0 in the amount of *(original amount - 1)*.

For example, 5 turns into (1 0 0 0 0)

A number in a Level-2 list is turned into a series of 0. For example, a list ((4) 3 2) becomes: (0 0 0 0 1 0 0 1 0)."

(flat (loop for x in list1

    if (typep x 'list)
        collect (loop for i from 1 to (car x) collect 0)
    if (and (not (typep x 'list)) (< x 0))
        collect (cons 1 (loop for i from 1 to (- (abs x) 1) collect 0))
    if (and (not (typep x 'list)) (= x 0))
        collect nil
    if (and (not (typep x 'list)) (> x 0))
        collect (cons 1 (loop for i from 1 to (- x 1) collect 0))
))
)
