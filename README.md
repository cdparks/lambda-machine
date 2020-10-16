![Lambda Machine Screenshot][screenshot]

## What?

[Try it here][lambda-machine]! It's a machine for evaluating
expressions in the untyped lambda calculus. This machine has everything:
* Lambdas
* Variables
* Applications
* Non-recursive top-level definitions
* Uh, that's it

## Really?

Yep. Here's a grammar, if you like that kind of thing. I know I do.

```ebnf
definition
  = name, {name}, "=", expression ;          (* Definition *)

expression
  = lambda, name, {name}, ".", expression    (* Lambda abstraction *)
  | name                                     (* Variable *)
  | expression, expression                   (* Application *)
  | "(", expression, ")"                     (* Parentheses *)
  | {digit}                                  (* Natural number *)
  | "[", [expressions], "]"                  (* List *)
  ;

expressions
  = expression, [",", expressions] ;         (* One or more comma-separated expressions *)

lambda
  = "\"                                      (* Backslash *)
  | "λ"                                      (* Greek letter lambda *)
  ;

name
  = (letter | "_")                           (* Initial letter or underscore *)
  , {letter | "-"}                           (* Zero or more letters or hyphens *)
  , ["?"]                                    (* Optional question mark *)
  , {subscript | digit}                      (* Zero or more subscripts or digits *)
  ;

letter                                       (* Lowercase latin letters *)
  = "a" | "b" | "c" | "d" | "e" | "f" | "g"
  | "h" | "i" | "j" | "k" | "l" | "m" | "n"
  | "o" | "p" | "q" | "r" | "s" | "t" | "u"
  | "v" | "w" | "x" | "y" | "z" ;

subscript                                    (* Subscripts *)
  = "₀" | "₁" | "₂" | "₃" | "₄" | "₅" | "₆"
  | "₇" | "₈" | "₉" ;

digit                                        (* Decimal digits *)
  = "0" | "1" | "2" | "3" | "4" | "5" | "6"
  | "7" | "8" | "9" ;
```

Natural numbers and lists are desugared to plain lambda calculus during
parsing. A natural number **n** is parsed as a function that applies
**s** to **z** **n** times.

```plaingtext
0 -> \s. \z. z
1 -> \s. \z. s z
2 -> \s. \z. s (s z)
3 -> \s. \z. s (s (s z))
4 -> \s. \z. s (s (s (s z)))
```

A list is parsed as a right fold over its elements using **cons** and
**nil**.

```plaintext
[a]       -> \cons. \nil. cons a nil
[a, b]    -> \cons. \nil. cons a (cons b nil)
[a, b, c] -> \cons. \nil. cons a (cons b (cons c nil))
```

These work together too:

```plaintext
[0, 1, 2] -> \cons. \nil. cons (\s. \z. z) (cons (\s. \z. s z) (cons (\s. \z. s (s z)) nil))
```

## Why?

~~I've been working through the exercises in
[_Introduction to Functional Programming Through Lambda Calculus_][book]
by [Greg Michaelson][greg], and some of these expressions are tedious
to reduce by hand. I build this to do it for me.~~

Lol, I'm not doing that anymore. At this point, it's mostly just a fun
way for me to fiddle around with PureScript. Hopefully, it's also
useful to someone learning the lambda calculus for the first time.
Let [me][me] know if you're using it and how I can make it better.

## How?

Lambda Machine is written in [PureScript][purescript] and [React][react]
using the [purescript-react-basic][react-basic] bindings. Expressions
are converted to a locally nameless representation before being
evaluated in normal order using call-by-need. Specifically, I use a
tweaked version of the Template Instantiation Machine described in
Simon Peyton Jones' and David Lester's [_Implementing Functional Languages: A Tutorial_][ifl].

The Template Instantiation Machine is a graph reduction interpreter
that is typically considered too slow and inflexible for "real"
language implementations. However, for Lambda Machine, I only care
about retaining or reconstructing enough syntactic information to
easily visualize incremental lazy evaluation for humans. Speed is not
the goal, and the Template Instantiation Machine works just fine.

Tweaks:
1. The Template Instantiation Machine operates on lambda-lifted
supercombinators. Lambda Machine creates closures at runtime.
2. The Template Instantiation Machine applies functions to as many
arguments as are available on the stack. Lambda Machine applies
functions one argument at a time, like a person working with pen and
paper would.
3. The Template Instantiation Machine does not evaluate under lambdas.
Lambda Machine does. This makes certain functions (e.g. the predecessor
function on Church numerals) work that would otherwise get stuck too
early.
4. The Template Instantiation calls its stack-of-suspended-stacks the
"dump". Lambda Machine calls its stack-of-suspended-stacks the "stash".
I don't remember why I did this. Maybe I thought "stash" looked nicer
next to "stack". Maybe I thought "dump" was too ugly of a word to be
typing so much. Truly, it is a mystery lost to time.



Anyway, build and run it like this:

```bash
yarn setup
yarn build
yarn start
```

(Or, again, just go [here][lambda-machine].)

## Who?

Me, [Chris Parks][me], hi, hello, how are you? HMU if you have questions, comments, or, like, really
good sandwich recipes.

[screenshot]: https://raw.githubusercontent.com/cdparks/lambda-machine/main/static/images/lambda-machine.png
[lambda-machine]: https://lambda-machine.com
[book]: https://www.amazon.com/dp/0486478831
[greg]: https://www.macs.hw.ac.uk/~greg
[purescript]: https://www.purescript.org
[react]: https://reactjs.org/
[react-basic]: https://github.com/lumihq/purescript-react-basic
[ifl]: https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial
[me]: mailto:christopher.daniel.parks@gmail.com
