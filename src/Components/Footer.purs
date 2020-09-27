module Components.Footer
  ( component
  ) where

import React.Basic (JSX, fragment)
import React.Basic.DOM as R

component :: {} -> JSX
component _ = fragment
  [ R.hr {}
  , R.a
    { className: "pull-right"
    , href: "https://github.com/cdparks/lambda-machine"
    , children: [R.text "Source on GitHub"]
    }
  ]
