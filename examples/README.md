NOTE: True for SBCL on Linux

## dbp.1

```
FORM:
(fmt:dbp :p> "PREFIX STR>>"
         :m> "MESSAGE GOES HERE. newline->" :nl
             "MESSAGE ON THE SECOND LINE. delimiter->" :d-
             "another delimiter->" :d~=-*)

PRINTS TO *STANDARD-OUTPUT*:
┌ 0   PREFIX STR>> MESSAGE GOES HERE. newline->
│ 0   PREFIX STR>> MESSAGE ON THE SECOND LINE. delimiter->
│ 0   PREFIX STR>> ------------------------------------------------------------
│ 0   PREFIX STR>> another delimiter->
└ 0   PREFIX STR>> ~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*~=-*

RETURNS:
NIL
```

## dbp.2

```
FORM:
(fmt:dbp :?s s :m> "ONELINER. RETURNS THE RESULT OF A FORM AFTER THE :r= KEYWORD" :r= (* 3 (+ 1 2)))

PRINTS TO s STREAM:
• 1   ONELINER. RETURNS THE RESULT OF A FORM AFTER THE :r= KEYWORD

RETURNS:
9
```

## dbp.3

```
FORM:
(let ((i 1))
  (fmt:dbp p> "prefix" i
           m> "PREFIX IS EVALUATED ONCE AND FOR ALL LINES" nl
              "OTHER ARGS EVALUATE FROM LEFT TO RIGHT" nl
              (incf i) nl
              "NOT PRINTED, JUST RETURNED -->" r= (incf i) nl
              "PRINTED AND RETURNED -->" pr= (incf i) nl
              "RETURNED VALUES ARE RETURNED WITH 'VALUES' IN ORDER FROM RIGHT TO LEFT"))

PRINTS TO *STANDARD-OUTPUT*:
┌ 2   prefix 1 PREFIX IS EVALUATED ONCE AND FOR ALL LINES
│ 2   prefix 1 OTHER ARGS EVALUATE FROM LEFT TO RIGHT
│ 2   prefix 1 2
│ 2   prefix 1 NOT PRINTED, JUST RETURNED -->
│ 2   prefix 1 PRINTED AND RETURNED --> 4
└ 2   prefix 1 RETURNED VALUES ARE RETURNED WITH 'VALUES' IN ORDER FROM RIGHT TO LEFT

RETURNS:
4
3
```

## fmt.1

```
FORM:
(fmt:fmt ":A :: ~ :: :S" "str" "str")

PRINTS TO *STANDARD-OUTPUT*:
str : ~ : "str"
```

## fmt.2

```
FORM:
(fmt:fmt :s s ":@{:A:^,:}" 1 2 3 4))

PRINTS TO s STREAM:
1,2,3,4
```

## fmt4l.1

```
FORM:
(fmt:fmt4l ":A--" 1 2 3 4)

PRINTS TO *STANDARD-OUTPUT*:
1--2--3--4--
```

## fmt4l.2

```
FORM:
(fmt:fmt4l :d "==" ":A" 1 2 3 4)

PRINTS TO *STANDARD-OUTPUT*:
1==2==3==4
```

## fmt4l.3

```
FORM:
(fmt:fmt4l :d+ "==" ":A" 1 2 3 4)

PRINTS TO *STANDARD-OUTPUT*:
1==2==3==4==
```

## fmts.1

```
FORM:
(fmt:fmts (":A " "string1")
          (":S" "string2"))

PRINTS TO *STANDARD-OUTPUT*:
string1 "string2"
```

## fmts.2

```
FORM:
(fmt:fmts :d ":%<br>:%"
          (":A" "string1")
          (":S" "string2"))

PRINTS TO *STANDARD-OUTPUT*:
string1
<br>
"string2"
```

## echo.1

```
FORM:
(fmt:echo 1 2 "DON'T CARE, PRINT" :anything (list (+ 41 1) (make-instance 'class)))

PRINTS TO *STANDARD-OUTPUT*:
1 2 "DON'T CARE, PRINT" :ANYTHING (42 #<CLASS COMMON-LISP:NIL {100570B163}>)
```

## echo.2

```
FORM:
(progn 
  (fmt:echo :-nl 1 2)
  (fmt:echo 3 4))
  
PRINTS TO *STANDARD-OUTPUT*:
1 2 3 4
```

