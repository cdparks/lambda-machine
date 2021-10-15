module Lambda.Language.Prelude
  ( defs
  ) where

import Lambda.Prelude

import Lambda.Language.Definition (Definition)
import Lambda.Language.Definition as Definition
import Lambda.Language.Parser (parse)
import Lambda.Language.Parser as Parser

-- | Default set of global definitions
--
-- Don't reorder these; only add new definitions at the end. This will
-- eventually allow us to store the set of prelude definitions used in
-- a session with a bitset. For example, 0x3 or 0b11 would mean only
-- const and identity are used:
--
--   bitfield | definition | used
--   0        | id         | ✓
--   1        | const      | ✓
--   2        | compose    | ✗
--   …
--
defs :: Array Definition
defs = Parser.unsafeRun parse <$>
  [ "identity x = x"
  , "const x y = x"
  , "compose f g x = f (g x)"
  , "fix f = f (fix f)"
  , "true t f = t"
  , "false t f = f"
  , "and x y = x y false"
  , "or x y = x true y"
  , "not x = x false true"
  , "succ n s z = s (n s z)"
  , "pred n s z = n (λg. λh. h (g s)) (λu. z) (λu. u)"
  , "add m n s z = m s (n s z)"
  , "mul m n s z = m (n s) z"
  , "zero? n = n (λx. false) true"
  , "cons x xs f z = f x (xs f z)"
  , "nil f z = z"
  , "foldr f z l = l f z"
  , "map f = foldr (compose cons f) nil"
  , "any = foldr or false"
  , "all = foldr and true"
  , "iterate f x = cons x (iterate f (f x))"
  , "repeat x = cons x (repeat x)"
  ]
