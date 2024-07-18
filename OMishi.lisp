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
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "number-operations" :type "lisp")
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "list-operations" :type "lisp")
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "number-generators" :type "lisp")
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "self-similarity" :type "lisp")

       ))



(om::fill-library 
 '(
   ("number operations" Nil Nil (keep-within-value ) Nil)
   ("list operations" Nil Nil (bifurcate1 bifurcate2 converge1 converge2 euclid-distance euclid-distance-3d fraction-maker 0to-1 -1to0 find-duplicate-number 10->num num->10 chord-rotate zigzag-chordseqtrace) Nil)
   ("number generators" Nil Nil (srn zigzag-arithm-ser euclid-rhythm-binary dejong dejong-svensson lorenz mandelbrot-imager mandelbrot-calc) Nil)
   ("self-similarity" Nil Nil (1D-CELLULAR-AUTOMATA ) Nil)
   ))


