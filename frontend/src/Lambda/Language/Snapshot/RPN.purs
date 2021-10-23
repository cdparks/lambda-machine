module Lambda.Language.Snapshot.RPN
  ( RPN(..)
  , encode
  , decode
  , unsafeTag
  ) where

import Lambda.Prelude

import Data.Int.Bits ((.&.), (.|.), zshr, shl)
import Lambda.Language.Snapshot.Error (Error(..))
import Lambda.Language.Snapshot.Tag
  ( Tag(..)
  , _VAR
  , _NAT
  , _TAK
  , _LAM
  , _DEF
  , _APV
  , _AP0
  )

-- | Reverse polish notation for constructing ASTs
-- |
-- | Constructor | Tag  | Decimal | Binary
-- | ------------+------+---------+-------
-- | Var         | _VAR |       1 | 001
-- | Nat         | _NAT |       2 | 010
-- | Take        | _TAK |       3 | 011
-- | Lambda      | _LAM |       4 | 100
-- | Define      | _DEF |       5 | 101
-- | AppVar      | _APV |       6 | 110
-- | Apply       | _AP0 |       7 | 111
data RPN
  -- | Push name at index
  = Var Int
  -- | Push literal natural number
  | Nat Int
  -- | Take n elements from the stack and build a list
  | Take Int
  -- | Build lambda with name at index and body from stack
  | Lambda Int
  -- | Build definition with name at index and body from stack
  | Define Int
  -- | Apply name at index to top of stack
  | AppVar Int
  -- | Apply top of stack to element beneath it
  | Apply

derive instance eqRPN :: Eq RPN

instance Show RPN where
  show = case _ of
    Var i -> "var " <> show i
    Nat i -> show i
    Take i -> "take " <> show i
    Lambda i -> "lam " <> show i
    Define i -> "def " <> show i
    AppVar i -> "app " <> show i
    Apply -> "app"

-- | Encode RPN as a sequence of 32-bit ints
encode :: forall m. MonadThrow Error m => Array RPN -> m (Array Int)
encode = traverse case _ of
  Var i -> setTag _VAR i
  Nat i -> setTag _NAT i
  Take i -> setTag _TAK i
  Lambda i -> setTag _LAM i
  Define i -> setTag _DEF i
  AppVar i -> setTag _APV i
  Apply -> setTag _AP0 1

-- | Attempt to decode sequence of 32-bit ints back to RPN
decode :: forall m. MonadThrow Error m => Array Int -> m (Array RPN)
decode = traverse (step <<< untag)
 where
  step { tag, payload }
    | tag == _VAR = pure $ Var payload
    | tag == _NAT = pure $ Nat payload
    | tag == _TAK = pure $ Take payload
    | tag == _LAM = pure $ Lambda payload
    | tag == _DEF = pure $ Define payload
    | tag == _APV = pure $ AppVar payload
    | tag == _AP0 = pure $ Apply
    | otherwise = throwError $ UnrecognizedTag tag

-- | Attempt to tag payload if it fits in 29 bits
setTag :: forall m. MonadThrow Error m => Tag -> Int -> m Int
setTag tag payload = unsafeTag tag <$> check payload
 where
  check x
    | x < 0x00000000 = throwError $ PayloadOutOfRange tag payload
    | x > 0x1FFFFFFF = throwError $ PayloadOutOfRange tag payload
    | otherwise = pure x

-- | Split tagged value into tag and payload
untag :: Int -> { tag :: Tag, payload :: Int }
untag tagged = { tag, payload }
 where
  tag = Tag $ tagged .&. 0x7
  payload = tagged `zshr` 3

-- | Exported for testing only
unsafeTag :: Tag -> Int -> Int
unsafeTag tag x = (x `shl` 3) .|. un Tag tag
