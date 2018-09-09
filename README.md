![Lambda Machine Screenshot][screenshot]

## What?

[Try it here][lambda-machine]! It's a machine for evaluating
expressions in the untyped lambda calculus. You get lambdas, variables,
applications, and non-recursive top-level definitions.

## Really?

Yarp. Here's a grammar, if you like that kind of thing:

```plaintext
<definition>
  ::= <name> [<name> ...] = <expression>       -- Definition

<expression>
  ::= \ <name> [<name> ...] . <expression>     -- Lambda
  |   <name>                                   -- Variable
  |   <expression> <expression>                -- Application
  |   ( <expression> )                         -- Parenthesization

<name>
  ::= [<lower> <underscore>]
      [<lower> <digit> <hyphen>]*
      [<question-mark>]?
      ([<prime>]* | [<subscript>]*)
```

There is also optional syntax for natural numbers and lists, but these
are desugared to plain lambda calculus at parse time:

```plaintext
[a, b, c]
    -> λcons. λnil. cons a (cons b (cons c nil))
3
    -> λs. λz. s (s (s z))
[1]
    -> λcons. λnil. cons (λs. λz. s z) nil
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
evaluated in normal order.

You can run it like this:

```bash
yarn setup
yarn build
[xdg-]open index.html
```

## Who?

Me, [Chris Parks][me]. Feel free to say hi!

[screenshot]: https://raw.githubusercontent.com/cdparks/lambda-machine/master/static/images/lambda-machine.png
[lambda-machine]: https://cdparks.github.io/lambda-machine
[book]: https://www.amazon.com/dp/0486478831
[greg]: https://www.macs.hw.ac.uk/~greg
[purescript]: https://www.purescript.org
[react]: https://reactjs.org/
[react-basic]: https://github.com/lumihq/purescript-react-basic
[me]: mailto:christopher.daniel.parks@gmail.com
