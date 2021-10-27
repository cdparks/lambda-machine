module Components.Help
  ( component
  ) where

import Components.Markup

component :: {} -> JSX
component _ = markup
  [ title "Hello!"
  , para
    [ text "Welcome to Lambda Machine, a tool for stepping through expressions in the untyped lambda calculus."
    ]
  , title "Expressions"
  , para
    [ text "Lambda Machine accepts "
    , bold "expressions"
    , text " and "
    , bold "definitions"
    , text ". An "
    , bold "expression"
    , text " consists of variables, applications, and lambdas. Lambdas can be written with a backslash "
    , bold "\\"
    , text " or the Greek letter "
    , bold "λ"
    , text ". We'll use "
    , bold "\\"
    , text " since it's easier to type. Here's an expression that represents a function that accepts one argument and returns it immediately."
    ]
  , code
    [ "\\x. x"
    ]
  , para
    [ text "We call this function "
    , bold "identity"
    , text "."
    ]
  , para
    [ text "Here's a function that returns its first argument after throwing away its second argument."
    ]
  , code
    [ "\\x. \\y. x"
    ]
  , para
    [ text "We usually call this function "
    , bold "const"
    , text "."
    ]
  , para
    [ text "Note that the expression above is a function that returns another function."
    , text " For convenience we can write it as a single lambda with two arguments."
    ]
  , code
    [ "\\x y. x"
    ]
  , para
    [ text "We'll use the multi-argument form instead of manually nesting lambdas from now on."
    ]
  , para
    [ text "We can apply "
    , bold "const"
    , text " to "
    , bold "identity"
    , text " twice using "
    , bold "juxtaposition"
    , text ", which is a fancy word for \"putting things next to one another\"."
    ]
  , code
    [ "(\\x y. x) (\\x. x) (\\x. x)"
    ]
  , para
    [ text "Even if we rename the arguments to our "
    , bold "identity"
    , text " functions, they still do the same thing."
    ]
  , code
    [ "(\\x y. x) (\\a. a) (\\b. b)"
    ]
  , para
    [ text "Specifically, this expression reduces as follows."
    ]
  , notes
    [ "(\\x y. x) (\\a. a) (\\b. b)" ?~
      [ text "Replace "
      , mono "x"
      , text " with "
      , mono "(\\a. a)"
      , text " in "
      , mono "(\\x y. x)"
      ]
    , "(\\y. \\a. a) (\\b. b)" ?~
      [ text "Replace "
      , mono "y"
      , text " with "
      , mono "(\\b. b)"
      , text " in "
      , mono "(\\y. \\a. a)"
      ]
    , "\\a. a" ?~ []
    ]
  , title "Definitions"
  , para
    [ text "To avoid repeating ourselves, we can enter a "
    , bold "definition"
    , text ". A "
    , bold "definition"
    , text " consists of a name, zero or more arguments, an equals sign, and an "
    , bold "expression"
    , text ". Let's make definitions for "
    , bold "identity"
    , text " and "
    , bold "const"
    , text "."
    ]
  , code
    [ "identity = \\x. x"
    , "const = \\x y. x"
    ]
  , para
    [ text "We can also write these like so."
    ]
  , code
    [ "identity x = x"
    , "const x y = x"
    ]
  , para
    [ text "Now we can refer to them by name."
    ]
  , code
    [ "const identity identity"
    ]
  , para
    [ text "A name is reduced by replacing it with the expression on the right-hand-side of its definition."
    ]
  , notes
    [ "const identity identity" ?~
      [ text "Replace "
      , mono "const"
      , text " with "
      , mono "\\x. \\y. x"
      ]
    , "(\\x. \\y. x) identity identity" ?~
      [ text "Replace "
      , mono "x"
      , text " with "
      , mono "identity"
      , text " in "
      , mono "(\\x. \\y. x)"
      ]
    , "(\\y. identity) identity" ?~
      [ text "Replace "
      , mono "y"
      , text " with "
      , mono "identity"
      , text " in "
      , mono "(\\y. identity)"
      ]
    , "identity" ?~
      [ text "Replace "
      , mono "identity"
      , text " with "
      , mono "\\x. x"
      ]
    , "\\x. x" ?~ []
    ]
  , title "Syntactic Sugar"
  , para
    [ text "Lambda Machine can parse natural numbers. A natural number "
    , bold "n"
    , text " is parsed as a function that applies "
    , bold "s"
    , text " to "
    , bold "z n"
    , text " times."
    ]
  , code
    [ "0 -> \\s. \\z. z"
    , "1 -> \\s. \\z. s z"
    , "2 -> \\s. \\z. s (s z)"
    , "3 -> \\s. \\z. s (s (s z))"
    , "4 -> \\s. \\z. s (s (s (s z)))"
    ]
  , para
    [ text "You can read more about this encoding "
    , link
      { this: "here"
      , to: "en.wikipedia.org/wiki/Church_encoding"
      }
    , text "."
    ]
  , para
    [ text "Lambda Machine can also parse lists."
    , text " A list is parsed as a right fold over the elements using "
    , bold "cons"
    , text " and "
    , bold "nil"
    , text "."
    ]
  , code
    [ "[a] -> \\cons. \\nil. cons a nil"
    , "[a, b] -> \\cons. \\nil. cons a (cons b nil)"
    , "[a, b, c] -> \\cons. \\nil. cons a (cons b (cons c nil))"
    ]
  , para
    [ text "This works with natural numbers as well."
    ]
  , code
    [ "[1] -> \\cons. \\nil. cons (\\s. \\z. s z) nil"
    , "[1, 2] -> \\cons. \\nil. cons (\\s. \\z. s z) (cons (\\s. \\z. s (s z)) nil)"
    ]
  , title "Recursion"
  , para
    [ text "The lambda calculus doesn't have recursive definitions."
    , text " After all, how would an anonymous function call itself?"
    , text " Instead, there are several well-known "
    , bold "fixed-point combinators"
    , text " that can "
    , bold "pass a copy of a function to itself"
    , text ", which it can then apply (effectively giving us the power of recursion)."
    ]
  , para
    [ text "You can read about fixed-point combinators "
    , link
      { this: "here"
      , to: "en.wikipedia.org/wiki/Fixed-point_combinator"
      }
    , text "."
    ]
  , para
    [ text "We used to predefine this one:"
    ]
  , code
    [ "fix f = (λx. f (x x)) (λy. f (y y))"
    ]
  , para
    [ mono "fix"
    , text " is powerful enough to define recursive functions,"
    , text " but it can be difficult to follow their reduction."
    , text " Many steps are spent just duplicating the function"
    , text " and passing it to itself."
    , text " Therefore, lambda-machine allows top-level definitions to"
    , text " be defined using direct recursion."
    , text " For examples, see the definitions of "
    , mono "iterate"
    , text " and "
    , mono "repeat"
    , text "."
    ]
  , title "Bye!"
  , para
    [ text "Have fun and "
    , link
      { this: "let me know if you find this useful"
      , to: "github.com/cdparks/lambda-machine"
      }
    , text "!"
    ]
  ]
