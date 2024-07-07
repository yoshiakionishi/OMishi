(in-package :om)
;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;===============================================
; OMishi Functions: List Operations
; As of July 6 2024
; - chord-rotate (new!)
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

;===============================================

(om::defmethod! chord-rotate ((list1 list) (approx number) (mode symbol) (octeq number))
 :initvals '('(6000 6400 6700 7100) 2 'keepbassnote 0 )
  :indoc '("chord (list of midicents)" "1/x tone approximation" "keep/take out the bass pedal note" "midicent multiple")
  ; The optional menu (1 of 2) for whether or not to keep the persistent "pedal" at the end of the process
  :menuins '((2 (("keep the bass note" 'keepbassnote ) ("take out the bass note" 'takeoutbassnote))))
  :icon 5678645
  :doc "Chord Rotate 

(Yoshiaki Onishi, July 7, 2024)

Input 1: list of midicents (a chord)
Input 2: tone division (2 = halfstep; 4 = 1/4 step; 8 = 1/8 step)
Input 3: Two modes: Keep or take out the bass note from the resultant chords in the chord-seq.
Input 4: Midicent multiple value for the virtual pitch. By default, 0, an octave multiple (1200) of the lowest pitch just above the highest pitch of the chord is applied. You can change this value to other midicent values. For example, 1000 would find a virtual pitch closest to the highest pitch of the original chord at minor 7th starting from the lowest pitch.

chord-rotate takes a chord, analyzes its interval structure (including the interval between the highest pitch plus a *virtual* pitch, an octave multiple of the lowest pitch just above the highest pitch of the input chord), derives all possible rotations of the interval structure, then applies them upon the lowest pitch of the original chord, deriving rotated chords. 

You have the option to remove the lowest note (akin to pedal note) from the resultant chord-seq.

It handles smaller tone divisions than a half step, as well, but you need to specify which tone division you are using via Inlet 2.

This function was formerly named *perm-interval-2*, which created all the possible inversions of the original chord then transposed them down so that the lowest pitch of each chord is the same as that of the original chord. Though the procedure may be different, the result was identical. For chord-rotate, two additional functions were added. (1) Option to remove the lowest pitch which functions like a pedal tone. (2) Option to set the *virtual pitch* differently, where the multiple of midicent can be set to something other than an octave (1200).

Click on the function and push t to see an example patch.

For details, especially on *perm-interval-2*, please refer to: Onishi, Yoshiaki. *Between Imagination and Realization: Composers and Metaphysical Spaces.* D.M.A. Doctoral Dissertation, Columbia University, 2015. http://dx.doi.org/10.7916/D8CJ8CCG. Pages 167-169.
"


; The optional menu (1 of 2) for whether or not to keep the persistent "pedal" at the end of the process
(if (eq mode 'keepbassnote)
  (setq bnmode 0)
  (setq bnmode 1)
)

; The optional menu (2 of 2) to determine the multiple of midicents at which the "virtual soprano voice" (i.e. copy of the bass note) is applied. If set to 0, it is at an octave (1200 midic).
(if (eq octeq 0)
    (setq   basemultiple 1200
            multiplier 0)
    (setq   basemultiple octeq
            multiplier 0)
)

; Process 1. Input list is cleaned up with approx-m, as well as sorting from low to high
(setq   list1 (sort (approx-m list1 approx) #'<)
        testbassnote    (first list1)           ; this "testbassnote" will fluctuate according to the "basemultiple" notes.
        testhighnote    (car (last list1))
        originaltestbassnote testbassnote
        basemultiplelist (loop for i in (arithm-ser 1 1000 1) collect (* basemultiple i))

)

(if (numberp (position  (- testhighnote testbassnote) basemultiplelist)) 
; if it's the chord fits exactly the base multiple (ROUTINE BEGIN)
(progn
(setq   newlist  list1)

(setq   difflist (x->dx newlist)
        difflistrotate  (loop for i from 0 to (- (length difflist) 1) 
                        collect (rotate difflist i)
                        )
        rotatedchords   (loop for i in difflistrotate
                        collect (dx->x (nth 0 newlist) i)
                        )
)

(if     (eq bnmode 1)
        (setq rotatedchords (loop for i in rotatedchords
                collect (cdr i))
        )
        (setq rotatedchords rotatedchords)
))
; if it's the chord fits exactly the base multiple(ROUTINE END)
; if it does not (ROUTINE BEGIN)
(progn
        (loop   while (< basemultiple (- testhighnote testbassnote))
        do  (setq testbassnote (+ originaltestbassnote (* basemultiple multiplier))
                    multiplier (+ multiplier 1))  
)

(if     (= multiplier 0) 
        (setq multiplier 1)
)
(setq   newhighpitch (+ originaltestbassnote (* basemultiple multiplier)))

; Add the reference note at the top 


(setq   newlist   (flat (cons list1 (list newhighpitch)))
)

(if (= (car (last newlist)) (nth (- (length newlist) 2) newlist)) ;if the last two items are identical...
    (setq newlist (butlast newlist)) ; remove the last item (i.e. duplicate)
    (setq newlist newlist)      ; otherwise, keep the list intact
)

(setq   difflist (x->dx newlist)
        difflistrotate  (loop for i from 0 to (- (length difflist) 1) 
                        collect (rotate difflist i)
                        )
        rotatedchords   (loop for i in difflistrotate
                        collect (butlast (dx->x (nth 0 newlist) i))
                        )
)

(if     (eq bnmode 1)
        (setq rotatedchords (loop for i in rotatedchords
                collect (cdr i))
        )
        (setq rotatedchords rotatedchords)
))
; if it does not (ROUTINE END)
)
)