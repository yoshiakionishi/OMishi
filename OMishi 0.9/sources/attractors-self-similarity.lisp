(in-package :om)
;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;===============================================
; OMishi Functions: Attractors and Self-Similarity Operations
; As of July 29 2024
; - clifford
; As of July 24 2024
;  - - - The followings have been moved from 'number generators'
; - dejong
; - dejong-svensson
; - lorenz
; - mandelbrot-imager
; - mandelbrot-calc
; As of June 30 2024
; - 1D-CELLULAR-AUTOMATA

;===============================================
(om::defmethod! dejong ((a number) (b number) (c number) (d number) (iter number) (initskip number))
:initvals '(1.641 1.902 0.316 1.525 100 0)
  :indoc '("value a" "value b" "value c" "value d" "number of iterations" "how many to skip initially")
  :icon 2345312
  :numouts 2
  :doc "Peter de Jong Attractor

(Yoshiaki Onishi, July 6, 2024; revised July 22, 2024)

dejong follows the principle of the Peter de Jong Attractor. Its equation is:

xn+1 = sin(a yn) - cos(b xn)
yn+1 = sin(c xn) - cos(d yn)

First four inlets accept values a through d. 
Fifth inlet accepts a value of iterations.
Sixth inlet accepts a value of initial skip. When set to x>0, the function will have skipped x times before outputting the value.

The function outputs a list of x values on the first outlet, then a list of y values on the second outlet.

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

(setq finallist (loop for i from 1 to iter
    do  (setq   newx   (-  (sin (* a y))    (cos (* b x)))
                newy   (-  (sin (* c x))    (cos (* d y)))
                x newx
                y newy
        )
    collect newx into x-list
    collect newy into y-list
    finally (return (list x-list y-list))
))
(values (first finallist) (second finallist))

)


;===============================================
(om::defmethod! dejong-svensson ((a number) (b number) (c number) (d number) (iter number) (initskip number))
 :initvals '(1.40 1.56 1.40 -6.56 100 0)
  :indoc '("value a" "value b" "value c" "value d" "number of iterations" "how many to skip initially")
  :icon 2345312
  :numouts 2
  :doc "Peter de Jong Attractor: Johnny Svensson Variation

(Yoshiaki Onishi, July 6, 2024)

dejong-svensson follows the principle of the alternate version (contributed by Johnny Svensson) of the Peter de Jong Attractor. Its equation is:

xn+1 = d sin(a xn) - sin(b yn)
yn+1 = c cos(a xn) + cos(b yn)

First four inlets accept values a through d. 
Fifth inlet accepts a value of iterations.
Sixth inlet accepts a value of initial skip. When set to x>0, the function will have skipped x times before outputting the value.

The function outputs a list of x values on the first outlet, then a list of y values on the second outlet.

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

(setq finallist (loop for i from 1 to iter
    do  (setq   newx   (-  (* d (sin (* a x)))    (sin (* b y)))
                newy   (+  (* c (cos (* a x)))    (cos (* b y)))
                x newx
                y newy
        )
    collect newx into x-list
    collect newy into y-list
    finally (return (list x-list y-list))
))
(values (first finallist) (second finallist))
)


;===============================================
(om::defmethod! lorenz ((a number) (b number) (c number) (x number) (y number) (z number) (h number) (iter number) (initskip number))
 :initvals '(10 28 2.6666668 0.1 0 0 0.01 1000 0)
  :indoc '("value a" "value b" "value c" "initial value x" "initial value y" "initial value z" "step size" "number of iterations" "how many to skip initially")
  :icon 2345312
  :numouts 3
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
The function has three outputs: (list of x values); (list of y values), and; (list of z values).

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

(setq finallist (loop for i from 1 to iter
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
))
(values (first finallist) (second finallist) (third finallist))
)


;===============================================

(om::defmethod! mandelbrot-imager ((realnumberrange list) (imagnumberrange list) (iterationtestvalue number)) 
 :initvals '('(-1.75 0.5 0.01) '(-1 1 0.01) 30) 
  :indoc '("real number range (min max increment)" "imaginary number range (min max increment)" "iteration")
  :icon 2345312
  :numouts 2
  :doc "mandelbrot-imager

(Yoshiaki Onishi, July 16, 2024)

This function evaluates whether or not the set of complex numbers Z(n) that starts with the fixed value C (range for real and imaginary numbers must be specified via the first two inlets) that are iterated by the Mandelbrot equation

Z(n+1) = Z(n)^2 + C

are bounded. If bounded, the value C is returned.

First two inlets specify the range of real and imaginary numbers for a series of complex numbers to be evaluated. You may specify the range as follows:

(minimum maximum increment)

Once evaluated, depending on the range of the value C, it will take a while to calculate the result. Please be patient!

Best used with BPC object, this function is useful in identifying the value C for use in generating Mandelbrot set by using the function, mandelbrot-calc. 

See the example patch by selecting the function and pushing t."

(setq   zvalue (complex 0 0))
(loop for (r i) in (loop for i from (first imagnumberrange) to (second imagnumberrange) by (third imagnumberrange)
                        nconcing      (loop for r from (first realnumberrange) to (second realnumberrange) by (third realnumberrange)
                                        if (eq (loop initially (setq zvalue (complex 0 0)) for y from 1 to iterationtestvalue
                                                do (setq zvalue (+ (expt zvalue 2) (complex r i)))
                                                until (or (> (abs (realpart zvalue)) 2)(> (abs (imagpart zvalue)) 2))
                                                        if      (= y iterationtestvalue)
                                                                do (return t)

                                        ) t)
                                collect (list r i)
                                )
                        )
collect r into x-list
collect i into y-list
finally (return (values x-list y-list))
)
)
;===============================================


(om::defmethod! mandelbrot-calc ((realhalf number) (imaghalf number) (iterationtestvalue number) (mode symbol)) 
 :initvals '(-0.01 0 30 'ascomplexnumber ) 
  :indoc '("real" "imag" "iteration" "output as complex nums or real imag nums separated")
  :menuins '((3 (("output as (#C(r1 i1) #C(r2 i2)...)" 'ascomplexnumber ) ("output as ((r1 r2 r3...)(i1 i2 i3...))" 'realandimag ))))
  :icon 2345312
  :doc "mandelbrot-calc

(Yoshiaki Onishi, July 16, 2024)

This function outputs a set of values Z(n), based on the fixed value, complex number C (Inlets 1 and 2 that accept real and imaginary numbers respectively) iterated (n times, as specified in Inlet 3) using the Mandelbrot equation

Z(n+1) = Z(n)^2 + C

If Z(n) exceeds 2, the iteration stops at that point. You will also see in the OM Listener that the set is not part of Mandelbrot set. 

Inlet 4 provides the option of output being a list of Complex Numbers, or a list of two sublists, where real and imaginary numbers are separated.




"
(setq subsequentzvalue 0
        realness 0
        imagness 0)
(if (eq mode 'ascomplexnumber)
    (setq output 0)
    (setq output 1)
)

(if (eq output 0) 
    (progn 
        (setq subsequentzvalue (loop initially (setq zvalue (complex 0 0)) for y from 1 to iterationtestvalue
                        do (setq zvalue (+ (expt zvalue 2) (complex realhalf imaghalf)))
                        until (or (> (abs (realpart zvalue)) 2)(> (abs (imagpart zvalue)) 2))
                        collect zvalue
                               )
        )
        (if (eq (length subsequentzvalue) iterationtestvalue)
            (progn  (print "set remained bounded ==> likely part of Mandelbrot set")
                subsequentzvalue
            )
            (progn (print "set did not remain bounded ==> not part of Mandelbrot set")
                subsequentzvalue
            )
        )
    )

    (progn
        (setq subsequentzvalue  (loop initially (setq zvalue (complex 0 0)) for y from 1 to iterationtestvalue
                                    do (setq zvalue (+ (expt zvalue 2) (complex realhalf imaghalf)))
                                    until (or (> (abs (realpart zvalue)) 2)(> (abs (imagpart zvalue)) 2))
                                    collect (realpart zvalue) into realness
                                    collect (imagpart zvalue) into imagness
                                    finally (return (list realness imagness))
                                )
        )
        (if (eq (length (first subsequentzvalue)) iterationtestvalue)
            (progn  (print "set remained bounded ==> likely part of Mandelbrot set")
                subsequentzvalue
            )
            (progn 
                (print "set did not remain bounded ==> not part of Mandelbrot set" )
                subsequentzvalue
            )
        )
    )
)

)



;===============================================
(om::defmethod! clifford ((a number) (b number) (c number) (d number) (iter number) (initskip number))
 :initvals '(-1.7 1.8 -1.9 -0.4 200 0)
  :indoc '("value a" "value b" "value c" "value d" "number of iterations" "how many to skip initially")
  :icon 2345312
  :numouts 2
  :doc "Clifford Attractors

(Yoshiaki Onishi, July 29, 2024)

clifford follows the principle of Clifford Attractors, which is similar to the Peter de Jong Attractors. Its equation is:

xn+1 = sin(a yn) + c cos(a xn)
yn+1 = sin(b xn) + d cos(b yn)

First four inlets accept values a and d. 
Fifth inlet accepts a value of iterations.
Sixth inlet accepts a value of initial skip. When set to x>0, the function will have skipped x times before outputting the value.

The function outputs a list of x values on the first outlet, then a list of y values on the second outlet.

Click on the function and push t to see an example patch.

For details, see: https://paulbourke.net/fractals/clifford/
"
(setq   x   0
        y   0
        newx    0
        newy    0
        )

; the following is performed only when the initial skip value is more than 0
(when (> initskip 0)
(loop for i from 1 to initskip
    do  (setq   newx   (+  (sin (* a y))    (* c (cos (* a x))))
                newy   (+  (sin (* b x))    (* d (cos (* b y))))
                x newx
                y newy
        )
)
)

(setq finallist (loop for i from 1 to iter
    do  (setq   newx   (+  (sin (* a y))    (* c (cos (* a x))))
                newy   (+  (sin (* b x))    (* d (cos (* b y))))
                x newx
                y newy
        )
    collect newx into x-list
    collect newy into y-list
    finally (return (list x-list y-list))
))
(values (first finallist) (second finallist))
)

;===============================================

(om::defmethod! 1D-CELLULAR-AUTOMATA ((orig-list list) (iteration number))
 :initvals '( '(0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 ) 50)
  :indoc '("axiom as a list (initial list). It must be a list of 0 and 1." "How many iterations?")
  :icon 2345312
  :doc "1D-CELLULAR-AUTOMATA 
  
(Yoshiaki Onishi, October 27, 2020)

This function simulates 1d cellular automaton with a set of production rules.
First inlet takes the axiom (initial list). It must be a list of 0 and 1.
Second inlet takes an atomic number of how many times it will be repeated."
	
  (setq
    iii (list 1 1 1)
    iio (list 1 1 0)
    ioi (list 1 0 1)
    ioo (list 1 0 0)
    oii (list 0 1 1)
    oio (list 0 1 0)
    ooi (list 0 0 1)
    ooo (list 0 0 0)
    evallist orig-list
    newlist evallist
    )

    (defun circular-nth-flat (number0 list0)
      (setq list1 (append list0 list0) reverse-list0 (reverse list1))

        (if (< number0 0) ;if the nth value is a negative value
          (nth (- (abs number0) 1) reverse-list0)
          ;nth value turns into abs. value - 1 and applied to reversed list
          (nth number0 list1)
          ;else returns the nth value
        )
    )

    (defun evaluation-process (list3)
      (loop for y from 0 to (- (length list3) 1)
      do (setq evaluating-list
        (list (circular-nth-flat (- y 1) list3)
        (circular-nth-flat y list3)
          (circular-nth-flat (+ y 1) list3)))
          if (or  (equal evaluating-list iii)
                  (equal evaluating-list ioi)
                  (equal evaluating-list oio)
                  (equal evaluating-list ooo))
        collect 0
          else if (or  (equal evaluating-list iio)
                  (equal evaluating-list ioo)
                  (equal evaluating-list oii)
                  (equal evaluating-list ooi))
          collect 1
      )
    )


  (print newlist)
  (cons newlist
    (loop for x from 0 to (- iteration 1)
    do (setq evallist (evaluation-process evallist))
    do (print evallist)
    collect evallist)
  )

)


        
