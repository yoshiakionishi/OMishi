# OMishi
=======================================================================

    OMishi
    A library of Common Lisp functions
    by Yoshiaki Onishi [[https://www.yoshionishi.com]]

    Version 0.8
=======================================================================

    This program is free software. For information on usage 
    and redistribution, see the "LICENSE" file in this distribution.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

=======================================================================

    OMishi houses functions that I have written codes for in Common
    Lisp language, either natively or translating them from OpenMusic
    patches or Javascript codes. 
    
    Parallel to my work as a composer, OMishi is a work in progress; 
    more will be added as the need arises.

=======================================================================

    Contents

    OMishi
    - Attractors and Self-Similarity
    - - - dejong 
    - - - dejong-svensson 
    - - - lorenz 
    - - - mandelbrot-imager 
    - - - mandelbrot-calc 
    - - - 1D-CELLULAR-AUTOMATA
    - Euclid
    - - - euclid-rhythm-binary 
    - - - euclid-distance 
    - - - euclid-distance-3d
    - Number Operations
    - - - keep-within-value
    - List Operations
    - - - bifurcate1 
    - - - bifurcate2 
    - - - converge1 
    - - - converge2 
    - - - fraction-maker 
    - - - 0to-1 
    - - - -1to0 
    - - - search-number-index 
    - - - 10->num 
    - - - num->10 
    - - - chord-rotate 
    - - - zigzag-chordseqtrace 
    - - - listchomp
    - Number Generators
    - - - srn 
    - - - zigzag-arithm-ser 

=======================================================================

    Version Info

    Version 0.x = Pre-release Version (June 30 2024~)

    Version 0.8: July 24 2024
    Reorganized the functions
    Revised:
        - dejong
        - dejong-svensson
        - lorenz

    Version 0.7: July 19 2024
    Added:
        - listchomp (beta version but improved functionality)

    Version 0.6.1: July 18 2024
    Deleted:
        - listchomp (I am building a new code)

    Version 0.6: July 16 2024
    New Functions:
        List Operations:
            - euclid-distance
            - euclid-distance-3d
        Number Generators:
            - mandelbrot-imager
            - mandelbrot-calc
    Corrected:
        List Operations:
            - bifurcate1
            - bifurcate2
            - converge1
            - converge2
                (For these functions I removed an extra level of list 
                from the default inlet values. The default values 
                should work now.)

    Version 0.5.1: July 9 2024
    Corrected:
        Number Generators
            - zigzag-arithm-ser

    Version 0.5: July 8 2024
    New Functions:
        List Operations:
            - listchomp
            - bifurcate1
            - bifurcate2
            - converge1
            - converge2
            - zigzag-chordseqtrace


    Version 0.4: July 7 2024
    New Function:
        List Operations
            - chord-rotate

    Version 0.3: July 6 2024
    New Functions:
        Number Generators
            - dejong
            - dejong-svensson
            - lorenz


    Version 0.2: July 2 2024
    New Functions:
        Number Generators
            - euclid-rhythm-binary
        List Operations
            - num->10
            - 10->num


    Version 0.1: June 30 2024 
    New Functions:
        List Operations
            - fraction-maker
            - 0to-1
            - -1to0
            - find-duplicate-number

        Number Operations
            - keep-within-value
        
        Number Generators
            - srn
            - zigzag-arithm-ser

        Self-Similarity
            - 1D-CELLULAR-AUTOMATA
