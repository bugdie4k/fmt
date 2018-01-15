**NOTE:** True for SBCL on Linux

## dbp.1 ([documentation](../#dbp-and-dbp-reset-counter)

FORM:
``` common-lisp
(fmt:dbp :p> "PREFIX STR>>"
         :m> "MESSAGE GOES HERE. newline->" :nl
             "MESSAGE ON THE SECOND LINE. delimiter->" :d-
             "another delimiter->" :d~=-*)
```

PRINTS TO `*STANDARD-OUTPUT*`:

```
┌ 0   PREFIX STR>> MESSAGE GOES HERE. newline->
│ 0   PREFIX STR>> MESSAGE ON THE SECOND LINE. delimiter->
│ 0   PREFIX STR>> ------------------------------------------------------------
│ 0   PREFIX STR>> another delimiter->
└ 0   PREFIX STR>> ~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*
```

RETURNS:
```
NIL
```

## dbp.2


FORM
``` common-lisp
(fmt:dbp :?s s :m> "ONELINER. RETURNS THE RESULT OF A FORM AFTER THE :r= KEYWORD" :r= (* 3 (+ 1 2)))
```

PRINTS TO `s` STREAM:
```
• 1   ONELINER. RETURNS THE RESULT OF A FORM AFTER THE :r= KEYWORD
```

RETURNS:
```
9
```

## dbp.3

FORM
```common-lisp
(let ((i 1))
  (fmt:dbp p> "prefix" i
           m> "PREFIX IS EVALUATED ONCE AND FOR ALL LINES" nl
              "OTHER ARGS EVALUATE FROM LEFT TO RIGHT" nl
              (incf i) nl
              "NOT PRINTED, JUST RETURNED -->" r= (incf i) nl
              "PRINTED AND RETURNED -->" pr= (incf i) nl
              "RETURNED VALUES ARE RETURNED WITH 'VALUES' IN ORDER FROM RIGHT TO LEFT"))
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
┌ 2   prefix 1 PREFIX IS EVALUATED ONCE AND FOR ALL LINES
│ 2   prefix 1 OTHER ARGS EVALUATE FROM LEFT TO RIGHT
│ 2   prefix 1 2
│ 2   prefix 1 NOT PRINTED, JUST RETURNED -->
│ 2   prefix 1 PRINTED AND RETURNED --> 4
└ 2   prefix 1 RETURNED VALUES ARE RETURNED WITH 'VALUES' IN ORDER FROM RIGHT TO LEFT
```
RETURNS:
```
4
3
```

## dbp.4

FORM
``` common-lisp
(fmt:dbp "BACKSLASHING KEYWORDS WITH :l -->" :l :p> :l :m> :l :?no-end-newline :l :r= 123 :l :l)
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
• 3  BACKSLASHING KEYWORDS WITH :l --> :P> :M> :?NO-END-NEWLINE :R= 123 :L
```

RETURNS:
```
NIL
```

## dbp.5

FORM
``` common-lisp
(progn
  (fmt:dbp-reset-counter)
  (fmt:dbp "DEMONSTRATING COUNTER RESETTING AND SOME OPTIONS")  
  (fmt:dbp :A "A")
  (fmt:dbp ?fletter "A" :A "A")
  (fmt:dbp ?dw 20 :d- :d=)
  (fmt:dbp ?no-counter :d- :d=)
  (fmt:dbp ?no-clip :d- :d=)
  (fmt:dbp ?dw 20 ?no-counter ?no-clip :d- :d=))
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
• 0   DEMONSTRATING COUNTER RESETTING AND SOME OPTIONS
• 1   :A A
• 2   A A
┌ 3   --------------------
└ 3   ====================
┌ ------------------------------------------------------------
└ ============================================================
5   ------------------------------------------------------------
5   ============================================================
--------------------
====================
```

RETURNS:
```
NIL
```

## dbp.6

FORM
``` common-lisp
(fmt:dbp ?rsc ?dw 10
         d - :m-1 r= :r-1 :m-2 nl 
         m> :m-3 :m-4 nl
         p> :p-1 r= :r-2 :p-2 
         m> :m-5 :m-6 nl 
         p> :p-3 :p-4 "/" 
         m> d -)
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
┌ 0   :P-1 :P-2 :P-3 :P-4 / ----------
│ 0   :P-1 :P-2 :P-3 :P-4 / :M-1 :M-2
│ 0   :P-1 :P-2 :P-3 :P-4 / :M-3 :M-4
│ 0   :P-1 :P-2 :P-3 :P-4 / :M-5 :M-6
└ 0   :P-1 :P-2 :P-3 :P-4 / ----------
```

RETURNS:
```
:R-2
:R-1
```


## fmt.1

FORM
``` common-lisp
(fmt:fmt ":A :: ~ :: :S" "str" "str")
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
str : ~ : "str"
```

## fmt.2

FORM
``` common-lisp
(fmt:fmt :s s ":@{:A:^,:}" 1 2 3 4))
```

PRINTS TO `s` STREAM:
```
1,2,3,4
```

## fmt.3

FORM
``` common-lisp
(fmt:fmt :s nil "=>:4A=>:4@A" :ABC :DEF)
```

RETURNS:
```
"=>ABC => DEF"
```

## fmt4l.1

FORM
``` common-lisp
(fmt:fmt4l ":A--" 1 2 3 4)
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
1--2--3--4--
```

## fmt4l.2

FORM
``` common-lisp
(fmt:fmt4l :d "==" ":A" 1 2 3 4)
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
1==2==3==4
```

## fmt4l.3

FORM
``` common-lisp
(fmt:fmt4l :d+ "==" ":A" 1 2 3 4)
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
1==2==3==4==
```

## fmts.1

FORM
``` common-lisp
(fmt:fmts (":A " "string1")
          (":S" "string2"))
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
string1 "string2"
```

## fmts.2

FORM
``` common-lisp
(fmt:fmts :d ":%<br>:%"
          (":A" "string1")
          (":S" "string2"))
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
string1
<br>
"string2"
```

## echo.1

FORM
``` common-lisp
(fmt:echo 1 2 "DON'T CARE, PRINT" :anything (list (+ 41 1) (make-instance 'class)))
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
1 2 "DON'T CARE, PRINT" :ANYTHING (42 #<CLASS COMMON-LISP:NIL {100570B163}>)
```

## echo.2

FORM
``` common-lisp
(progn 
  (fmt:echo :-nl 1 2)
  (fmt:echo 3 4))
```

PRINTS TO `*STANDARD-OUTPUT*`:
```
1 2 3 4
```
