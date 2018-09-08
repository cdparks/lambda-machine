module Components.Footer
  ( component
  ) where

import React.Basic as React
import React.Basic.DOM as R

component :: React.Component {}
component =
  React.stateless {displayName: "Footer", render}
 where
  render _ =
    React.fragment
      [ R.hr {}
      , R.a
        { className: "pull-right"
        , href: "https://github.com/cdparks/lambda-machine"
        , children: [R.text "Source on GitHub"]
        }
      ]
