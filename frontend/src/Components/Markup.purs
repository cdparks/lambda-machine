-- | Simplified, fixed-depth markup for tutorial and errors
module Components.Markup
  ( module X
  , Markup
  , Note
  , Leaf
  , markup
  , title
  , para
  , code
  , notes
  , note
  , (?~)
  , text
  , bold
  , mono
  , link
  ) where

import Lambda.Prelude hiding (note)

import React.Basic (JSX) as X
import React.Basic (JSX, fragment)
import React.Basic.DOM as R

-- | Nodes that can have children
newtype Markup = Markup JSX

-- | Nodes without children
newtype Leaf = Leaf JSX

-- | Nodes with a focus and a sequence of annotations
newtype Note = Note JSX

-- | Coerce `Markup` to `JSX
markup :: Array Markup -> JSX
markup = fragment <<< coerce

-- | Title fixed to h3
title :: String -> Markup
title = coerce <<< R.h3_ <<< pure <<< R.text

-- | Paragraph containing only leaf nodes
para :: Array Leaf -> Markup
para = coerce <<< R.p_ <<< coerce

-- | Preformatted block of text lines
code :: Array String -> Markup
code xs = coerce $ R.p
  { className: "preformatted"
  , children: formatPre <$> xs
  }

-- | Preformatted block of text lines each with zero or more annotations
notes :: Array Note -> Markup
notes xs = coerce $ R.table_
  [ R.tbody_ $ coerce xs
  ]

-- | Preformatted text line with zero or more annotations
note :: String -> Array Leaf -> Note
note x xs = coerce $ R.tr_
  [ R.td_
    [ R.span
      { className: "preformatted"
      , children: [formatPre x]
      }
    ]
  , R.td
    { className: "comment"
    , children: coerce xs
    }
  ]

infix 1 note as ?~

-- | Body text
text :: String -> Leaf
text = coerce <<< R.text

-- | Bold text
bold :: String -> Leaf
bold = coerce <<< R.strong_ <<< pure <<< R.text

-- | Monospaced text
mono :: String -> Leaf
mono x = coerce $ R.span
  { className: "monospace-font"
  , children: [R.text x]
  }

-- | Anchor
link :: { this :: String, to :: String } -> Leaf
link { this, to: url } = coerce $ R.a
  { href: "https://" <> url
  , target: "_blank"
  , rel: "noopener"
  , children: [R.text this]
  }

formatPre :: String -> JSX
formatPre x = R.text $ "  " <> x <> "\n"
