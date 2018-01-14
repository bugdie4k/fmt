# `F M T`

# tl;dr

See [examples](https://github.com/bugdie4k/fmt/tree/master/examples) or [tests](https://github.com/bugdie4k/fmt/blob/master/test/test.lisp).

# Contents

- [Motivation](#motivation)
- [Overview](#overview)
- [Documentation](#documentation)
  - [`fmt` and friends](#fmt-and-friends)
  - [`dbp` (and `dbp-reset-counter`)](#dbp-and-dbp-reset-counter)

# Motivation

I started writing this when I didn't know any better ways of debugging Common Lisp
other than inserting debug prints in form of `format` or `break` calls.
And there were several things that bothered me about the process.

 - **The tilde issue.**
 
 I didn't like how tilde (`~`) is hard to type 
 when you type it a lot. And it's something you often have to do 
 when formatting your debug prints.
 
 - **The placing issue.**
 
 I didn't like how easy it is to forget about where
 debug prints are placed and how I then had to grep all over the project 
 and check all the the `format` calls and pick the ones I don't need anymore. 
 
 - **The delimiter issue.**
 
 I didn't like that to print a clear
 delimiter for pieces of debug information everytime I had
 to do something like  
 `(format t "~%------------------~%")`, then `(format t "~%==================~%")`,  
 then `(format t "~%=!=!=!=!=!=!=!=!=!~%")`, and so on. 
 
So I've written this in an attempt to address these issues.  

Then I got acquainted with different cool features of SLIME like
inspector, evaluating in frame, macroexpansion in place, 
evaluation in place, how `(declaim (optimize (debug 3)))`
gives you more information in debugger, etc.  

Also I noticed that if you write `(format t "~@{~A~^ ~}~%" ARGS)` you
can write anything instead of ARGS one by one and it will act like an echo.
The insertion of such a form is easily automated with any good text editor
(Emacs + [yasnippet](https://github.com/joaotavora/yasnippet) for example) 
and it proved to be quite handy.  

I stopped using debug prints as much as I used to and the "echo" I "invented"
made debug printing much easier. 
But I always wanted to clean up the messy version of this lib, so here it is.

# Overview

- **The tilde issue.**

The solution to the tilde issue can be found in `fmt.lisp` file taking various forms.  
The `fmt` function has control-string that is just the same as 
control-string of a format, but colon (`:`) is used to specify format directives
instead of tilde (`~`). 

This means that all the format directives of CL's `format`
are now intoduced by colon. `~A` becomes `:A`, `~%` becomes `:%`, etc.
Also if you want to print a tilde, you just type a tilde, but if you wand to print
colon, you have to "backslash" colon by colon. Also stream argument is optional as
usually format (and hence fmt) is used to write to `*standard-output*`.

- **The placing issue.**

In the `dbp` macro one can specify a string which would prefix all the lines in a debug
message. Given the content of the prefix is a unique string (unique among other prefixes), 
it binds the log message to the place of a debug print in code and makes it easy to find
and remove on need.

- **The delimiter issue.**

The solution is `dbp` macro - stands for debug print. It has the `d` keyword
that can be used to create horizontal rule delimiters.

# Documentation

## `fmt` and friends

### fmt ([examples](https://github.com/bugdie4k/fmt/tree/master/examples#fmt1))

**SYNOPSIS**  
`(fmt [ :s stream ] [ :nl ] fmt-string fmt-args*)`
      
**DESCRIPTION**  
Same as format but with different syntax.

`fmt-string` is the same as control-string in format,
but with colon (`:`) as the character
to introduce the directive instead of tilde (`~`).

You can omit stream argument - `t` is default.

`:nl` adds newline to the end.      
      
<hr/>

### fmt4l ([examples](https://github.com/bugdie4k/fmt/tree/master/examples#fmt4l1))

**SYNOPSIS**  
`(fmt4l [ :s stream ] [ :nl ] 
        [ [ :d | :d+ ] delimiter ] fmt-string fmt-arg*)`
         
**DESCRIPTION**  
Allows to apply one `fmt-string' to each argument of `fmt-arg*'.
      
`fmt-string` and `:s` and `:nl` options are the same as in `fmt`.

`:d` allows to set a `delimiter` to insert between elements.

`:d+` does the same as `:d` and also adds `delimiter` to the end.

`delimiter` is also translated (same as `fmt-string`)
so you can specify `:%` as a delimiter to delimit with newline.

<hr/>

### fmts ([examples](https://github.com/bugdie4k/fmt/tree/master/examples#fmts1))

**SYNOPSIS**  
`(fmts [ :s stream ] [ [ :d | :d+] delimiter ]
       (fmt-string fmt-arg*)+)`

**DESCRIPTION**  
Allows to have multiple fmt calls in one form.

`fmt-string` and `:s`, `:d` and `:d+`  options
are the same as in `fmt`.

<hr/>

### echo ([examples](https://github.com/bugdie4k/fmt/tree/master/examples#echo1))

**SYNOPSIS**  
`(echo [ :-nl ] arg*)`


**DESCRIPTION**  
Prints args with `~S` formatting.
You can pass `:-nl` as the first argument to avoid newline."

<hr/>

### format+

**SYNOPSIS**  
`(format+ [ :s stream ] [ :nl ] control-string format-arg*)`


**DESCRIPTION**  
`fmt`'s counterpart with ordinary `format`'s `control-string`.

<hr/>

### format4l

**SYNOPSIS**  
`(format+ [ :s stream ] [ :nl ] control-string format-arg*)`


**DESCRIPTION**  
`fmt4l`'s counterpart with ordinary `format`'s `control-string`.

<hr/>

### formats

**SYNOPSIS**  
`(formats [ :s stream ] [ [ :d | :d+] delimiter ] (format-string format-arg*)+)`


**DESCRIPTION**  
`fmts`'s counterpart with ordinary `format`'s `control-string`.

<hr/>

### brk

**SYNOPSIS**  
`(brk control-string format-arg*)`

**DESCRIPTION**  
Same as break, but with different syntax.
`fmt`'s breaking counterpart.

<hr/>

### brk4l

**SYNOPSIS**  
`(brk4l control-string format-arg*)`


**DESCRIPTION**  
`fmt4l`'s breaking counterpart.

<hr/>

### breacho

**SYNOPSIS**  
`(brecho args)`


**DESCRIPTION**  
`echo`'s breaking counterpart.

<hr/>

## `dbp` (and `dbp-reset-counter`)
### ([examples](https://github.com/bugdie4k/fmt/tree/master/examples#dbp1))

TODO
