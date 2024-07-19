(in-package :om)
;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;===============================================
; OMishi Functions: List Operations
; As of July 19 2024
; - listchomp (added once again, while it is in a beta version, with improved functionality)
; As of July 18 2024
; - DELETED: listchomp (I am rewriting a new code for it)
; As of July 8 2024
; - listchomp
; - bifurcate1
; - bifurcate2
; - converge1
; - converge2
; - zigzag-chordseqtrace
; As of July 7 2024
; - chord-rotate
; As of June 30 2024
; - fraction-maker
; - 0to-1
; - -1to0
; - find-duplicate-number
; - num->10
; - 10->num
; As of July 16 2024
; - euclid-distance
; - euclid-distance-3d

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


;===============================================

(om::defmethod! bifurcate1 ((list1 list))
 :initvals '('(1 2 3 4 5 6 7 8 9 10) )
  :indoc '("list")
  :icon 5678645
  :doc "Bifurcate1 

(Yoshiaki Onishi, July 8, 2024)

Bifurcate1 takes a list and reorders its content by bifurcating it from the center.

Example 1: (bifurcate1 '(1 2 3 4 5 6 7 8 9 10)) ==> (5 6 4 7 3 8 2 9 1 10)
Example 2: (bifurcate1 '(1 2 3 4 5 6 7 8 9)) ==> (5 6 4 7 3 8 2 9 1)
"
(if (evenp (length list1))
    (setq   listoperation       (group-list list1 (list (/ (length list1) 2)(/ (length list1) 2)) 'linear)
            listoperationfirst  (loop for i in (reverse (first listoperation))
                                    collect i
                                )
            listoperation       (list listoperationfirst (second listoperation))
            newlist             (loop   for a in (first listoperation)
                                        for b in (second listoperation)
                                    nconcing (list a b)
                                )
    )
    
    (setq   listoperation       (group-list list1 (list (/ (- (length list1) 1) 2) 1 (/ (- (length list1) 1) 2)) 'linear)
            listoperationfirst  (loop for i in (reverse (first listoperation))
                                    collect i
                                )
            listoperationsecond  (second listoperation)
            newlist             (nconc listoperationsecond 
                                    (loop   for a in (third listoperation)
                                        for b in listoperationfirst
;                                       collect (cons a (cons b nil))
                                        nconcing (list a b)
                                    )
                                )

    )

    
)
)

;===============================================

(om::defmethod! bifurcate2 ((list1 list))
 :initvals '('(1 2 3 4 5 6 7 8 9 10) )
  :indoc '("list")
  :icon 5678645
  :doc "Bifurcate2 

(Yoshiaki Onishi, July 8, 2024)

Bifurcate2 takes a list and reorders its content by bifurcating it from the center but in the manner inverse to *bifurcate1*.

Example 1: (bifurcate2 '(1 2 3 4 5 6 7 8 9 10)) ==> (6 5 7 4 8 3 9 2 10 1)
Example 2: (bifurcate2 '(1 2 3 4 5 6 7 8 9)) ==> (5 4 6 3 7 2 8 1 9)
"
(if (evenp (length list1))
    (setq   listoperation       (group-list list1 (list (/ (length list1) 2)(/ (length list1) 2)) 'linear)
            listoperationfirst  (loop for i in (reverse (first listoperation))
                                    collect i
                                )
            listoperation       (list (second listoperation) listoperationfirst)
            newlist             (loop   for a in (first listoperation)
                                        for b in (second listoperation)
                                    nconcing (list a b)
                                )
    )
    
    (setq   listoperation       (group-list list1 (list (/ (- (length list1) 1) 2) 1 (/ (- (length list1) 1) 2)) 'linear)
            listoperationfirst  (loop for i in (reverse (first listoperation))
                                    collect i
                                )
            listoperationsecond  (second listoperation)
            newlist             (nconc listoperationsecond 
                                    (loop   for a in listoperationfirst 
                                        for b in (third listoperation)
;                                       collect (cons a (cons b nil))
                                        nconcing (list a b)
                                    )
                                )

    )

    
)


)

;===============================================

(om::defmethod! converge1 ((list1 list))
 :initvals '('(1 2 3 4 5 6 7 8 9 10) )
  :indoc '("list")
  :icon 5678645
  :doc "Converge1 

(Yoshiaki Onishi, July 8, 2024)

Converge1 takes a list and reorders its content, starting from its extremities (the first then the last item of the list) then moving toward the center.

Example 1: (converge1 '(1 2 3 4 5 6 7 8 9 10)) ==> (1 10 2 9 3 8 4 7 5 6)
Example 2: (converge1 '(1 2 3 4 5 6 7 8 9)) ==>  (1 9 2 8 3 7 4 6 5)
"
(if (evenp (length list1))
    (setq   listoperation       (group-list list1 (list (/ (length list1) 2)(/ (length list1) 2)) 'linear)
            listoperationlatter (loop for i in (reverse (second listoperation))
                                    collect i
                                )
            listoperation       (list (first listoperation) listoperationlatter)
            newlist             (loop   for a in (first listoperation)
                                        for b in (second listoperation)
                                    nconcing (list a b)
                                )
    )
    
    (setq   listoperation       (group-list list1 (list 1 (/ (- (length list1) 1) 2)(/ (- (length list1) 1) 2)) 'linear)
            listoperationthird  (loop for i in (reverse (third listoperation))
                                    collect i
                                )
            listoperationfirst  (first listoperation)
            newlist             (nconc listoperationfirst 
                                    (loop   for a in listoperationthird
                                        for b in (second listoperation)
;                                       collect (cons a (cons b nil))
                                        nconcing (list a b)
                                    )
                                )

    )

    
)

)

;===============================================

(om::defmethod! converge2 ((list1 list))
 :initvals '('(1 2 3 4 5 6 7 8 9 10) )
  :indoc '("list")
  :icon 5678645
  :doc "Converge2 

(Yoshiaki Onishi, July 8, 2024)

Converge2 takes a list and reorders its content, starting from its extremities (the last then the first item of the list) then moving toward the center.

Example 1: (converge2 '(1 2 3 4 5 6 7 8 9 10)) ==> (10 1 9 2 8 3 7 4 6 5)
Example 2: (converge2 '(1 2 3 4 5 6 7 8 9)) ==>  (9 1 8 2 7 3 6 4 5)
"
(if (evenp (length list1))
    (setq   listoperation       (group-list list1 (list (/ (length list1) 2)(/ (length list1) 2)) 'linear)
            listoperationlatter (loop for i in (reverse (second listoperation))
                                    collect i
                                )
            listoperation       (list (first listoperation) listoperationlatter)
            newlist             (loop   for a in (second listoperation)
                                        for b in (first listoperation)
                                    nconcing (list a b)
                                )
    )
    
    (setq   listoperation       (group-list list1 (list (/ (- (length list1) 1) 2)(/ (- (length list1) 1) 2) 1) 'linear)
            listoperationsecond (loop for i in (reverse (second listoperation))
                                    collect i
                                )
            listoperationthird  (third listoperation)
            newlist             (nconc listoperationthird
                                    (loop   for a in (first listoperation)
                                            for b in listoperationsecond
;                                       collect (cons a (cons b nil))
                                        nconcing (list a b)
                                    )
                                )

    )

    
)


)

;===============================================

(om::defmethod! zigzag-chordseqtrace ( (chordseq list) (list2 list) (num1 number) (num2 number) (num3 number) &optional (contouroption nil))
 :initvals '('((6000 6400 6700 7000 7400 7800) (6100 6500 6800 7100 7500 7900) (6200 6600 6900 7200 7600 8000) (6300 6700 7000 7300 7700 8100)) '(3 4) 0 2 1 nil)
  :indoc '("chord-seq list of midicents" "zigzag-arithm-ser Inlet 1: sublist length(s)" "zigzag-arithm-ser Inlet 2: starting pitch in nth value" "zigzag-arithm-ser Inlet 3: skip within sublist" "zigzag-arithm-ser Inlet 4: skip from one sublist to another" "further process of each sublist")
  :icon 5678645
  :doc "Zigzag Chord-seq Trace 

(Yoshiaki Onishi, July 8, 2024)

Specifically designed for use with a chord-seq object, this function accomplishes the following:

1. Each chord within the chord-seq object is sorted pitchwise, from the lowest to the highest pitch. 
2. Inlet 1 accepts list of midicents from the chord-seq object. After each sublist is reordered from lowest to highest midicents, the chord-seq list is flattened, resembling upward arpeggios from one chord to another.
3a. Inlets 2 (list), 3~5 (number) are analogous to the *zigzag-arithm-ser* function of the OMishi Library. The parameters determine how the series of notes ought to be traversed. It does use *zigzag-arithm-ser* function, with produces the *nth* values for the flattened chord-seq list. 
3b. Inlet 6: independent from but concurrent to Inlet 2, you can specify the specific *movement* that each group of the pitch-traversing movement undergoes. Aside from nil, you have to specify it with a list.
- - - nil or (o) : sublist is read ordinarily, from beginning to end.
- - - (r): sublist is retrograded.
- - - (b1): sublist is bifurcated clockwise from its center.
- - - (b2): sublist is bifurcated counterclockwise from its center.
- - - (c1): sublist is converged from the beginning, clockwise, toward its center.
- - - (c2): sublist is converged from the end, counterclockwise, toward its center.
- - - You may combine any of these items, e.g. (o c1 r), which are read cyclically.
4. The numbers that exceed the length of the flattened chord-seq list are converted, in such a way that: (exceeding number) mod (length of the flattened chord-seq list)
5. The list generated by the preceding process will be used to read the flattened chord-seq object.

Please refer to the example patch, accessible by clicking the function and pushing t.
"
(setq   prelimsortedchordseq (loop for i in chordseq collect (sort i #'<))
        prelimsortedchordseq (flat (loop for i in prelimsortedchordseq collect i))
        prelimsortedchordseqlength (length prelimsortedchordseq)
        arithmserlength (length list2)
        longarithmserlist (flat (loop repeat (om-round (/ prelimsortedchordseqlength arithmserlength)) collect list2))
        arithms (zigzag-arithm-ser num1 longarithmserlist num2 num3)

        contouroption2  (if (or (eq contouroption nil) (eq contouroption 'o))
                            (loop repeat (length longarithmserlist) collect 'o)
                            (subseq 
                                (flat 
                                    (loop repeat (ceiling (/ (length arithms) (length contouroption)))
                                        collect contouroption)
                                ) 
                            0 (length arithms)
                            )
                        )
        arithms  (flat
                        (loop for a in arithms
                                for b in contouroption2
                            if (eq b 'o)
                                collect a
                            if (eq b 'r)
                                collect (reverse a)
                            if (eq b 'b1)
                                collect (bifurcate1 a)
                            if (eq b 'b2)
                                collect (bifurcate2 a)
                            if (eq b 'c1)
                                collect (converge1 a)
                            if (eq b 'c2)
                                collect (converge2 a)                            
                        )
                    )
        arithms         (loop for a in arithms
                            collect (keep-within-value a prelimsortedchordseqlength)
                        )

        finalresult     (loop for a in arithms
                            collect (nth a prelimsortedchordseq)
                        )
)

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

;===============================================


(om::defmethod! listchomp ((num1 t) (num2 t) (num3 t) (list1 list)) 
 :initvals '(0 5 nil '(1 2 3 4 5 6 7 8 9 10) ) 
  :indoc '("starting index (0-based) or 'nil'" "stopping index (0-based), 'end', 'last' or 'nil'" "step or 'nil'" "list")
  :icon 5678645
  :doc "listchomp

(Yoshiaki Onishi, July 19, 2024)

NB: This function is still being tested for accuracy.

Inlet 1: starting index (also accepts: nil)
Inlet 2: stopping index (also accepts: end, last or nil)
Inlet 3: step (also accepts: nil)
Inlet 4: list

This function behaves nearly the same as the array slicing functionality of NumPy Package of Python, where starting index (0-based), stopping index, and step are given in order to slice an array.

(Reference: https://numpy.org/doc/stable/user/basics.indexing.html)

For example, for the following Python code:
>>> x = NumPy.array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
>>> x[1:7:2]
array([1, 3, 5])

You can obtain the same result using *listchomp* as follows:
> (setq x '(0 1 2 3 4 5 6 7 8 9))
> (listchomp 1 7 2 x)
=> (1 3 5)

Some syntacical differences between listchomp and NumPy array slicing (x is a list of '(0 1 2 3 4 5 6 7 8 9)):

1) Use nil for the absence of an index. For example:

>>> x[1]
(listchomp 1 nil nil x)
=> 1

>>> x[-3]
(listchomp -3 nil nil x)
=> 7

>>> x[:5]
(listchomp nil 5 nil x)
=> (0 1 2 3 4)

>>> x[5::-2]
(listchomp 5 nil -2 x)
=> (5 3 1)


2) The use/non-use of the initial colon in NumPy array slicing is differentiated in listchomp as:

>>> x[4]
(listchomp 4 nil nil x)
=> 4

>>> x[4:]
(listchomp 4 end nil x) OR
(listchomp 4 last nil x)
=> (4 5 6 7 8 9)

3) When the stopping index is lower than the starting index, listchomp interprets it as reversing the list. When no value is declared in *step*, it is understood as -1. Thus:

>>> x[6:2]
[] 

But:
(listchomp 6 2 nil x)
=> (6 5 4 3)

Equivalent in NumPy array slicing as:
>>> x[6:2:-1]



"
(cond   (
            (and (not (eq num1 nil))(eq num2 nil))  ; equivalent to array[n1::-n3]
            (cond   (   (or (eq num3 nil) (= num3 0)) ; equivalent to array[n1::]
                        (nth (keep-within-value num1 (length list1)) list1)
                    )
                    (   (< num3 0) ; equivalent to array[n1::-n3]
                        (setq   newnum1 (keep-within-value num1 (length list1))
                        )
                        (loop for n from newnum1 downto 0 by (abs num3)
                        collect (nth n list1)
                        )

                    )
                    (   (> num3 0) ; equivalent to array[n1::+n3]
                        (setq   newnum1 (keep-within-value num1 (length list1))
                        )
                        (loop for n from newnum1 to (- (length list1) 1) by (abs num3)
                        collect (nth n list1)
                        )

                    )

            )
        )
        (
            (and (not (eq num1 nil))(or (eq num2 'last)(eq num2 'end)))  ; equivalent to array[n1::-n3]
            (cond   (   (or (eq num3 nil) (>= num3 0)) ; equivalent to array[n1::+n3]
                        (setq   newnum1 (keep-within-value num1 (length list1))
                        )
                        (loop for n from newnum1 to (- (length list1) 1) by (if (eq num3 nil) 1 num3)
                        collect (nth n list1)
                        )

                    )
                    (   (< num3 0) ; equivalent to array[n1::-n3]
                        (setq   newnum1 (keep-within-value num1 (length list1))
                        )
                        (loop for n from newnum1 downto 0 by (abs num3)
                        collect (nth n list1)
                        )
                    )
            )
        )
        (
            (and (not (eq num1 nil))(not (eq num2 nil))) ; equivalent to array[n1:n2:]
            (setq   newnum1     (cond ((< num1 (* (length list1) -1)) 0 )
                                  ((and (>= num1 (* (length list1) -1)) (< num1 (length list1))) (keep-within-value num1 (length list1)))
                                  ((>= num1 (length list1)) (- (length list1) 1))
                                )
                    newnum2     (cond ((< num2 (* (length list1) -1)) 0 )
                                  ((and (>= num2 (* (length list1) -1)) (< num2 (length list1))) (keep-within-value num2 (length list1)))
                                  ((>= num2 (length list1)) (length list1))
                                )
            )
            (cond   (   (< newnum1 newnum2) 
                        (if (or (eq num3 nil)(>= num3 0))                        
                        (loop for n from newnum1 to (- newnum2 1) by (if (or (eq num3 nil)(eq num3 0)) 1 num3)
                        collect (nth n list1)
                        ))
                    )
                    (   (> newnum1 newnum2)
                        (if (or (eq num3 nil)(<= num3 0))    
                        (loop for n from newnum1 downto (+ newnum2 1) by (if (or (eq num3 nil)(eq num3 0)) 1 (abs num3))
                        collect (nth n list1)
                        ))
                    )
            )
        )
        (
            (and (eq num1 nil)(not (eq num2 nil))) ; equivalent to array[:n2:]
            (setq   newnum1    0
                    newnum2    (cond ((< num2 (* (length list1) -1)) 0)
                                  ((and (>= num2 (* (length list1) -1)) (< num2 (length list1))) (keep-within-value num2 (length list1)))
                                  ((>= num2 (length list1)) (length list1))
                            )
            )
            (cond   (   (and (< newnum1 newnum2) (eq num3 nil)) 
                        (loop for n from newnum1 to (- newnum2 1) by 1
                        collect (nth n list1)
                        )
                    )
                    (   (and (< newnum1 newnum2) (>= num3 0)) 
                        (loop for n from newnum1 to (- newnum2 1) by (if (eq num3 0) 1 num3)
                        collect (nth n list1)
                        )
                    )
                    (   (and (< newnum1 newnum2) (< num3 0)) 
                        (loop for n from (- (length list1) 1) downto (+ newnum2 1) by (abs num3)
                        collect (nth n list1)
                        )
                    )
                    (   (> newnum1 newnum2)
                        (if (< num3 0)    
                        (loop for n from (- (length list1) 1) downto newnum2 by (if (or (eq num3 nil)(eq num3 0)) 1 num3)
                        collect (nth n list1)
                        ))
                    )
            )
       

        )
       (
            (and (eq num1 nil)(eq num2 nil))
            (cond   (   (or (eq num3 0)(eq num3 nil))
                        (loop for n from 0 to (- (length list1) 1)
                        collect (nth n list1)
                        )
                    )

                    (   (< num3 0)
                        (loop for n from (- (length list1) 1) downto 0 by (abs num3)
                        collect (nth n list1)
                        )
                    )
                    (   (> num3 0)
                        (loop for n from 0 to (- (length list1) 1) by num3
                        collect (nth n list1)
                        )
                    )
            )
            
        )


)

)
