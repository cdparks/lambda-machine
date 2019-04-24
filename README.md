![Lambda Machine Screenshot][screenshot]

## What?

[Try it here][lambda-machine]! It's a machine for evaluating
expressions in the untyped lambda calculus. You get lambdas, variables,
applications, and non-recursive top-level definitions.

## Really?

Yarp. Here's a grammar, if you like that kind of thing:

```ebnf
definition
  = name, {param}, "=", expression ;         (* Definition *)

expression
  = lambda, param, {param}, ".", expression  (* Lambda abstraction *)
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

param
  = ["!"] name ;                             (* Strict params can be marked with a bang *)

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

## Why?

I've been working through the exercises in
[Introduction to Functional Programming Through Lambda Calculus][book]
by [Greg Michaelson][greg], and some of these expressions are tedious
to reduce by hand. It's nice to have something that does it for me!

## How?

Lambda Machine is written in [PureScript][purescript] and [React][react]
using the [purescript-react-basic][react-basic] bindings. Expressions
are converted to a locally nameless representation before being
evaluated in normal order _by default_. You can mark individual
parameters as being eagerly evaluated by prefixing them with a
bang (`!`). (Note: this is sneaky and new and I'm not sure it works yet.)

Build and run it like this:

```bash
yarn setup
yarn build
[xdg-]open index.html
```

## Who?

Me, [Chris Parks][me]. Feel free to say hi!

[screenshot]: https://raw.githubusercontent.com/cdparks/lambda-machine/master/static/images/lambda-machine.png
[lambda-machine]: https://lambda-machine.com
[book]: https://www.amazon.com/dp/0486478831
[greg]: https://www.macs.hw.ac.uk/~greg
[purescript]: https://www.purescript.org
[react]: https://reactjs.org/
[react-basic]: https://github.com/lumihq/purescript-react-basic
[me]: mailto:christopher.daniel.parks@gmail.com
