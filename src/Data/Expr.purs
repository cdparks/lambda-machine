module Data.Expr where

import Prelude
import Data.Maybe
import Control.Alt ((<|>))
import Data.Generic

import qualified Data.Map as Map
import Data.Array.Unsafe (unsafeIndex)
import Data.String (fromCharArray)

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Syntax

data Expr
  = Bound Name Int
  | Free Name
  | Bind Name Expr
  | App Expr Expr

derive instance genericExpr :: Generic Expr
instance showExpr :: Show Expr where
  show = gShow

syntaxToExpr :: Syntax -> Expr
syntaxToExpr = loop Map.empty
 where
  loop env a =
    case a of
      Var n ->
        maybe (Free n) (Bound n) (Map.lookup n env)
      Lambda n b ->
        let env' = Map.insert n 0 (map (+1) env)
        in Bind n (loop env' b)
      Apply f a ->
        App (loop env f) (loop env a)

exprToSyntax :: Expr -> Syntax
exprToSyntax e =
  case e of
    Bound n _ -> Var n
    Free n    -> Var n
    Bind n e  -> Lambda n (exprToSyntax e)
    App f a   -> Apply (exprToSyntax f) (exprToSyntax a)

instance prettyPrintExpr :: PrettyPrint Expr where
  prettyPrint = walk false
   where
    walk inApp e =
      case e of
        Bound n i -> show i
        Free n    -> n
        Bind _ b  -> parensIf inApp ("λ. " <> walk false b)
        App f a   -> walk true f <> " " <> walk true a

subscriptByIndex :: Array Char
subscriptByIndex = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉']

subscript :: Int -> String
subscript = digits >>> map (unsafeIndex subscriptByIndex) >>> fromCharArray

digits :: Int -> Array Int
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) <> [n `mod` 10]

step :: Expr -> Maybe Expr
step e =
  case e of
    App (Bind n e) m -> return (substitute m 0 e)
    App f a ->
      (App <$> step f <*> pure a)
      <|> (App <$> pure f <*> step a)
    Bind n e -> do
      e' <- step e
      return (Bind n e')
    Free n -> Nothing
    Bound n i -> Nothing

watch :: forall eff. Int -> Expr -> Eff (console :: CONSOLE | eff) Unit
watch 0 e = log (prettyPrint (exprToSyntax e))
watch n e = do
  log (prettyPrint (exprToSyntax e))
  maybe (return unit) (watch (n - 1)) (step e)

substitute :: Expr -> Int -> Expr -> Expr
substitute v = walk
 where
  walk index e =
    case e of
      Bound n i
        | i == index -> v
        | otherwise  -> Bound n i
      Free n -> Free n
      Bind n b -> Bind n (walk (index + 1) b)
      App f a -> App (walk index f) (walk index a)

