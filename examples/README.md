**NOTE:** True for SBCL on Linux

## dbp.1

**FORM:**
``` common-lisp
(fmt:dbp :p> "PREFIX STR>>"
         :m> "MESSAGE GOES HERE. newline->" :nl
             "MESSAGE ON THE SECOND LINE. delimiter->" :d-
             "another delimiter->" :d~=-*)
```

**PRINTS TO `*STANDARD-OUTPUT*`:**

```
┌ 0   PREFIX STR>> MESSAGE GOES HERE. newline->
│ 0   PREFIX STR>> MESSAGE ON THE SECOND LINE. delimiter->
│ 0   PREFIX STR>> ------------------------------------------------------------
│ 0   PREFIX STR>> another delimiter->
└ 0   PREFIX STR>> ~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*
```

**RETURNS:**
```
NIL
```

<hr/>

## dbp.2


**FORM:**
``` common-lisp
(fmt:dbp :?s s :m> "ONELINER. RETURNS THE RESULT OF A FORM AFTER THE :r= KEYWORD" :r= (* 3 (+ 1 2)))
```

**PRINTS TO `s` STREAM:**
```
• 1   ONELINER. RETURNS THE RESULT OF A FORM AFTER THE :r= KEYWORD
```

**RETURNS:**
```
9
```

<hr/>

## dbp.3

**FORM:**
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

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
┌ 2   prefix 1 PREFIX IS EVALUATED ONCE AND FOR ALL LINES
│ 2   prefix 1 OTHER ARGS EVALUATE FROM LEFT TO RIGHT
│ 2   prefix 1 2
│ 2   prefix 1 NOT PRINTED, JUST RETURNED -->
│ 2   prefix 1 PRINTED AND RETURNED --> 4
└ 2   prefix 1 RETURNED VALUES ARE RETURNED WITH 'VALUES' IN ORDER FROM RIGHT TO LEFT
```
**RETURNS:**
```
4
3
```

<hr/>

## dbp.4

**FORM:**
``` common-lisp
(fmt:dbp "BACKSLASHING KEYWORDS WITH :l -->" :l :p> :l :m> :l :?no-end-newline :l :r= 123 :l :l)
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
• 3  BACKSLASHING KEYWORDS WITH :l --> :P> :M> :?NO-END-NEWLINE :R= 123 :L
```

**RETURNS**
```
NIL
```

<hr/>

## dbp.5

**FORM:**
``` common-lisp
(progn
  (fmt:dbp-reset-counter)
  (fmt:dbp "DEMONSTRATING COUNTER RESETTING AND SOME OPTIONS")  
  (fmt:dbp :A "A")
  (fmt:dbp ?fletter "A" :A "A")
  (fmt:dbp ?dw 20 d- d=)
  (fmt:dbp ?no-counter d- d=)
  (fmt:dbp ?no-clip d- d=))
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
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
```

**RETURNS**
```
NIL
```

<hr/>

## fmt.1

**FORM:**
``` common-lisp
(fmt:fmt ":A :: ~ :: :S" "str" "str")
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
str : ~ : "str"
```

<hr/>

## fmt.2

**FORM:**
``` common-lisp
(fmt:fmt :s s ":@{:A:^,:}" 1 2 3 4))
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
1,2,3,4
```

<hr/>

## fmt4l.1

**FORM:**
``` common-lisp
(fmt:fmt4l ":A--" 1 2 3 4)
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
1--2--3--4--
```

<hr/>

## fmt4l.2

**FORM:**
``` common-lisp
(fmt:fmt4l :d "==" ":A" 1 2 3 4)
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
1==2==3==4
```

<hr/>

## fmt4l.3

**FORM:**
``` common-lisp
(fmt:fmt4l :d+ "==" ":A" 1 2 3 4)
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
1==2==3==4==
```

<hr/>

## fmts.1

**FORM:**
``` common-lisp
(fmt:fmts (":A " "string1")
          (":S" "string2"))
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
string1 "string2"
```

<hr/>

## fmts.2

**FORM:**
``` common-lisp
(fmt:fmts :d ":%<br>:%"
          (":A" "string1")
          (":S" "string2"))
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
string1
<br>
"string2"
```

<hr/>

## echo.1

**FORM:**
``` common-lisp
(fmt:echo 1 2 "DON'T CARE, PRINT" :anything (list (+ 41 1) (make-instance 'class)))
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
1 2 "DON'T CARE, PRINT" :ANYTHING (42 #<CLASS COMMON-LISP:NIL {100570B163}>)
```

<hr/>

## echo.2

**FORM:**
``` common-lisp
(progn 
  (fmt:echo :-nl 1 2)
  (fmt:echo 3 4))
```

**PRINTS TO `*STANDARD-OUTPUT*`:**
```
1 2 3 4
```

<hr/>
