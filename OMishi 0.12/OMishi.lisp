;===============================================
;   OMishi
;   Common Lisp Functions by Yoshiaki Onishi
;   (c) 2024 by Yoshiaki Onishi.
;
;   Version 0.1: June 30 2024 (initial beta release before going official and releasing Version 1)
;   Version 0.2: July 2 2024 (added euclidean-rhythm-binary)
;   Version 0.3: July 6 2024 (added dejong, dejong-svensson, lorenz)
;   Version 0.4: July 7 2024 (added chord-rotate)
;   Version 0.5: July 8 2024 (added listchomp, etc.)
;   Version 0.6: July 16 2024 (added mandelbrot-imager, etc.)
;   Version 0.6.1: July 18 2024 (deleted listchomp; I am building a new code for it)
;   Version 0.7: (added beta version of listchomp)
;   Version 0.8: July 24-26 (revisions on some functions)
;   Version 0.8.1: July 26 (revised search-number-index)
;   Version 0.9: ...
;   Version 0.10: August 3 2024 (added collatz)
;   Version 0.11: August 5 2024 (added four functions in List Operations)
;   Version 0.12: February 16 2025 (added one function (zigzag-chordseqtrace-v2) in List Operations)
;===============================================
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;===============================================


(in-package :om)


;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc 'om::compile&load 
      (list
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "attractors-self-similarity" :type "lisp")
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "euclid" :type "lisp")
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "number-operations" :type "lisp")
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "list-operations" :type "lisp")
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "number-generators" :type "lisp")

       ))



(om::fill-library 
 '(
   ("attractors and self-similarity operations" Nil Nil (dejong dejong-svensson clifford lorenz mandelbrot-imager mandelbrot-calc 1D-CELLULAR-AUTOMATA ) Nil)
   ("euclid" Nil Nil (euclid-rhythm-binary euclid-distance euclid-distance-3d) Nil)
   ("number operations" Nil Nil (keep-within-value ) Nil)
   ("list operations" Nil Nil (bifurcate1 bifurcate2 converge1 converge2 bifurcate1-repeat bifurcate2-repeat converge1-repeat converge2-repeat fraction-maker 0to-1 -1to0 search-number-index 10->num num->10 chord-rotate zigzag-chordseqtrace zigzag-chordseqtrace-v2 listchomp) Nil)
   ("number generators" Nil Nil (srn zigzag-arithm-ser collatz ) Nil)
   ))


