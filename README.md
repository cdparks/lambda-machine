#LambdaMachine

##What?

It's a machine for evaluating expressions in the untyped lambda calculus. You get lambdas, variables, applications, and <i>nothing else</i>. It's everything you need!

##Really?

Yep. Here's a grammar:

    ```plaintext
    <expression>
        = \ <name> . <expression>                    -- Lambda
        | <name>                                     -- Variable
        | <expression> <expression>                  -- Application
        | ( <expression> )                           -- Parenthesization

    <name> = <lower-case>[<lower-case> <numeric> <hyphen> <underscore>]*
    ```

For example, we could apply the constant function to the identity function twice:

    ```plaintext
    (\x. \y. x) (\a. a) (\b. b)
    (\y. \a. a) (\b. b)
    \a. a
    ```

##Why?

I've been working through the exercises in [Introduction to Functional Programming Through Lambda Calculus](http://www.amazon.com/Introduction-Functional-Programming-Calculus-Mathematics/dp/0486478831) by [Greg Michaelson](http://www.macs.hw.ac.uk/~greg/), and some of these expressions have become rather tedious to reduce by hand. It'd be nice to have something that would do it for me step-by-step, ya know?

##How?

It's written in [PureScript](http://www.purescript.org/) and [React](https://facebook.github.io/react/) using the [Thermite](https://github.com/paf31/purescript-thermite) library. Expressions are converted to a locally nameless representation before being evaluated in normal order.

It's not finished yet. I'm not sure how I want the UI to look, and what's there isn't all hooked up. You can run it like this:

    ```bash
    pulp dep install
    pulp build -O --to static/js/main.js
    open static/index.html
    ```

##Who?

Me, [Chris Parks](mailto:christopher.daniel.parks@gmail.com)

