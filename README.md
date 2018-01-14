# `F M T`

# tl;dr

See [examples](examples) or [tests](test/test.lisp).

# Contents

- [Motivation](#motivation)
- [Overview](#overview)
- [Documentation](#documentation)  
  - [`dbp` (and `dbp-reset-counter`)](#dbp-and-dbp-reset-counter)
    - [`dbp` message structure](#dbp-message-structure)
    - [`dbp` returns](#dbp-returns)
    - [`dbp` keywords](#dbp-keywords)
  - [`fmt` and friends](#fmt-and-friends)

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
 delimiter for pieces of debug information every time I had
 to do something like `(format t "~%------~%")`,
 then `(format t "~%======~%")`, then `(format t "~%=!=!=!~%")`, and so on. 
 
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

The solution to the tilde issue can be found in `fmt.lisp` 
file taking various forms.  
The [`fmt`](#fmt-and-friends)
function has control-string that is just the same as 
control-string of a format, but colon (`:`) is used to specify format directives
instead of tilde (`~`). 

This means that all the format directives of CL's `format`
are now introduced by colon. `~A` becomes `:A`, `~%` becomes `:%`, etc.
Also if you want to print a tilde, you just type a tilde, 
but if you wand to print
colon, you have to "backslash" colon by colon (`::`). 
Also stream argument is optional as usually format (and hence fmt) 
is used to write to `*standard-output*`.

- **The placing issue.**

The solution is [`dbp`](#dbp-and-dbp-reset-counter) macro - stands for
debug print. In the `dbp` macro one can specify a
string which would prefix all the lines in a debug message. 
Given the content of the prefix
is a unique string (unique among other prefixes), it binds the log
message to the place of a debug print in code and makes it easy to find
and remove on need.

- **The delimiter issue.**

The [`dbp`](#dbp-and-dbp-reset-counter) has the `d` keyword
that can be used to create horizontal rule delimiters.

# Documentation

## `dbp` (and `dbp-reset-counter`)
#### ([examples](examples#dbp1))

`dbp` accepts arguments that will be used to construct the log message.
There are *keywords* that have some special effect on debug message.
If argument is not a *keyword*, it is printed as with `~S` formatting 
(it can be altered with the `?fletter` option).

### `dbp` message structure

This call
``` common-lisp
(fmt:dbp :p> :prefix-> :m> 1 d-= 2 r= 10 r= "20")
```
produces the following output (except my comment on the message structure, of course)
```
┌ 5   :PREFIX-> 1
│ 5   :PREFIX-> -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
└ 5   :PREFIX-> 2
| \_/ \_______/ \__________________________________________________________/
|  |      |                               \
|  |       \                               "--> MESSAGE
|   \       "---------------------------------> PREFIX
 \   "----------------------------------------> COUNTER
  "-------------------------------------------> CLIP
```

- **MESSAGE**  
  The message is entered after the `m>` keyword.
  Or if no *section designating* (that is `p>` or `m>`) keywords were entered.
 
- **PREFIX**  
  The prefix is entered after the `p>` keyword and is inserted after each newline
  in message along with COUNTER and CLIP.

- **COUNTER**  
  The counter is the unique number of a log message. 
  After each `dbp` call the internal counter increments. 
  It can be reset with the `dbp-reset-counter` function or with the `?rsc` keyword.
  Remove it with the `?no-counter` keyword.

- **CLIP**  
  The clip (maybe not the best name for a thing?) is intended to help to distinguish
  a separate multiline message among the other outputted text.
  Remove it with the `?no-clip` keyword.
  
### `dbp` returns

`dbp` can return things from it's body using keywords `r=` and `pr=`.
`r=` returns the following form from `dbp` call and does not print it.
`pr=` both returns and prints.
See [examples dbp.2 and dbp.3](examples#dbp2).

If there are multiple `r=` and `pr=` keywords `dbp` returns several values
in order from right to left. 
Also if the stream is set to `nil` (with the `?s` keyword) then `dbp` returns
the string with created message as the first value no matter what `r=` and `pr=`
keywords are there in the body.

### `dbp` keywords

#### Section designators

There are two sections you can define: *prefix* and *message*.
Keywords for these are:

- `p>` - prefix  

  USAGE: `p> arg*`
  
- `m>` - message  

  USAGE: `m> arg*`

Note that there can be several `p>` and `m>` in one `dbp` call in random order.
See this [example](example#dbp6).

#### Markup

There are several "markup" keywords that define the looks of a message.

- `d` - delimiter  

  USAGE: `d pattern`
  
  Creates a horizontal rule composed of `pattern`
  which can be a symbol, a string, a keyword symbol or anything else
  because `pattern` goes through `princ-to-string`.
  
  - **NOTE:** there is a special syntax for `d`.
    All keyword symbols that start with d also create horizontal rule.
    That is `:d--` and `d --` are equal.
  
- `nl` - newline  

  Plainly translated to "~%".
  
- `cnl` - conditional newline  

  Plainly translated to "~&".
  
- `l` - literally  

  USAGE: `l arg`
  
  Used to "backslash" keywords and print them literally.
  To output contents of an `l` symbol, you also have to "backslash" it.

#### Returns

- `r=` - return  

  USAGE: `r= arg`
  
  Adds `arg` to returned values.
  
- `pr=` - print return  

  USAGE: `pr= arg`
  
  Adds `arg` to returned values and prints it.  

#### Options

- `?wd` - word delimiter

    USAGE: `?wd arg`
    
    The arg goes through `princ-to-string` and becomes word delimiter.
    
    Default is space.
    
- `?fletter` - format letter

    USAGE: `?fletter arg`
    
    The arg goes through `princ-to-string` and becomes new
    format letter instead of `S`. Do not use tilde (`~`) when 
    entering format letter in `arg` - it is implied and added automatically.
    
    Default is `S`.
    
- `?s` - stream 

    USAGE: `?s stream`
    
    Stream can also be `t` or `nil`.
    `t` means printing to `*standard-output*`.
    `nil` means that string with message will be returned as the first argument
    from `dbp` call.
    
    Default is `t`.
    
- `?dw` - delimiter width

    USAGE: `?dw num`
    
    Sets the length of delimiters printed by `d` keyword.
    
    Default is 60.
    
- `?counter-w` - counter width

    USAGE: `?counter-w num`
    
    Default is 3.
    
- `?prefix-w` - counter width

    USAGE: `?prefix-w num`
    
    Default is83.
    
- `?cut-counter`

    If met cuts counter upto counter width.    

- `?cut-prefix`

    If met cuts prefix upto prefix width.    
    
- `?break` - break
  
  If meats `?break` keyword ignores stream set with `?s`
  and breaks with constructed message.
  
- `?rsc` - reset counter
  
  You can also use `dbp-reset-counter` for this.

- `?no-end-newline`

- `?no-counter`

- `no-clip`

## `fmt` and friends

### fmt ([examples](examples#fmt1))

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

### fmt4l ([examples](examples#fmt4l1))

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

### fmts ([examples](examples#fmts1))

**SYNOPSIS**  
`(fmts [ :s stream ] [ [ :d | :d+] delimiter ]
       (fmt-string fmt-arg*)+)`

**DESCRIPTION**  
Allows to have multiple fmt calls in one form.

`fmt-string` and `:s`, `:d` and `:d+`  options
are the same as in `fmt`.

<hr/>

### echo ([examples](examples#echo1))

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
