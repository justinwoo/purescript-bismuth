module Bismuth where

-- | Identity function to apply the HasFlowRep constraint
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)
import Data.Function (id)
import Data.Function.Uncurried (Fn2)
import Data.List (List, intercalate, (:))
import Data.Monoid (mempty)
import Data.Nullable (Nullable)
import Data.Semigroup ((<>))
import Data.StrMap (StrMap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant)
import Prelude (Unit)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

-- | Identity function to apply the HasFlowRep constraint
toFlow :: forall a
   . HasFlowRep a
  => a -> a
toFlow = id

-- | Generate a Flow type signature for a given type. Takes the name to be used as an arg.
generateFlowType :: forall a
   . HasFlowRep a
  => String -> Proxy a -> String
generateFlowType name _ =
  "export type " <> name <> "=" <> ty
  where
    ty = toFlowRep (Proxy :: Proxy a)

-- | A convenience function for generating types taking a concrete value over a proxy.
generateFlowType' :: forall a
   . HasFlowRep a
  => String
  -> a
  -> String
generateFlowType' s a = generateFlowType s (Proxy :: Proxy a)

class HasFlowRep a where
  toFlowRep :: Proxy a -> String

instance numberHasFlowRep :: HasFlowRep Number where
  toFlowRep _ = "number"

instance stringHasFlowRep :: HasFlowRep String where
  toFlowRep _ = "string"

instance booleanHasFlowRep :: HasFlowRep Boolean where
  toFlowRep _ = "boolean"

instance foreignHasFlowRep :: HasFlowRep Foreign where
  toFlowRep _ = "any"

instance unitHasFlowRep :: HasFlowRep Unit where
  toFlowRep _ = "any"

instance strmapHasFlowRep ::
  ( HasFlowRep a
  ) => HasFlowRep (StrMap a) where
  toFlowRep _ = "{[key: string]:" <> ty <> "}"
    where
      ty = toFlowRep (Proxy :: Proxy a)

instance nullableHasFlowRep ::
  ( HasFlowRep a
  ) => HasFlowRep (Nullable a) where
  toFlowRep _ = ty <> " | null"
    where
      ty = toFlowRep (Proxy :: Proxy a)

instance arrayHasFlowRep ::
  ( HasFlowRep a
  ) => HasFlowRep (Array a) where
  toFlowRep _ = toFlowRep p <> "[]"
    where
      p = Proxy :: Proxy a

instance effHasFlowRep ::
  ( HasFlowRep a
  ) => HasFlowRep (Eff e a) where
  toFlowRep _ = "() => " <> a
    where
      a = toFlowRep (Proxy :: Proxy a)

instance functionHasFlowRep ::
  ( HasFlowRep a
  , HasFlowRep b
  ) => HasFlowRep (Function a b) where
  toFlowRep _ = "(a: " <> a <> ") => " <> b
    where
      a = toFlowRep (Proxy :: Proxy a)
      b = toFlowRep (Proxy :: Proxy b)

instance fn2HasFlowRep ::
  ( HasFlowRep a
  , HasFlowRep b
  , HasFlowRep c
  ) => HasFlowRep (Fn2 a b c) where
  toFlowRep _ =
      "(a: " <> a <>
      ", b: " <> b <>
      ") => " <> c
    where
      a = toFlowRep (Proxy :: Proxy a)
      b = toFlowRep (Proxy :: Proxy b)
      c = toFlowRep (Proxy :: Proxy c)

instance recordHasFlowRep ::
  ( RowToList row rl
  , HasFlowRepFields rl
  ) => HasFlowRep (Record row) where
  toFlowRep _ = "{" <> fields <> "}"
    where
      fields = intercalate "," (toFlowRepFields (RLProxy :: RLProxy rl))

class HasFlowRepFields (rl :: RowList) where
  toFlowRepFields :: RLProxy rl -> List String

instance consHasFlowRepFields ::
  ( HasFlowRepFields tail
  , IsSymbol name
  , HasFlowRep ty
  ) => HasFlowRepFields (Cons name ty tail) where
  toFlowRepFields _ = head : tail
    where
      key = reflectSymbol (SProxy :: SProxy name)
      val = toFlowRep (Proxy :: Proxy ty)
      head = key <> ":" <> val
      tailp = RLProxy :: RLProxy tail
      tail = toFlowRepFields tailp

instance nilHasFlowRepFields :: HasFlowRepFields Nil where
  toFlowRepFields _ = mempty

-- | a Variant is represented by VariantRep, which is a newtype record of
-- | `newtype VariantRep a = VariantRep { type ∷ String , value ∷ a }`
-- | as seen here:
-- | https://github.com/natefaubion/purescript-variant/blob/aef507e2972d294ecd735575371eccbc61ac1ac4/src/Data/Variant/Internal.purs#L31
instance fakeSumRecordHasFlowRep ::
  ( RowToList row rl
  , FakeSumRecordMembers rl
  ) => HasFlowRep (Variant row) where
  toFlowRep _ = intercalate "|" members
    where
      rlp = RLProxy :: RLProxy rl
      members = toFakeSumRecordMembers rlp

class FakeSumRecordMembers (rl :: RowList) where
  toFakeSumRecordMembers :: RLProxy rl -> List String

instance consFakeSumRecordMembers ::
  ( FakeSumRecordMembers tail
  , IsSymbol name
  , HasFlowRep ty
  ) => FakeSumRecordMembers (Cons name ty tail) where
  toFakeSumRecordMembers _ = head : tail
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      typ = Proxy :: Proxy ty
      val = toFlowRep typ
      head = "{type:\"" <> key <> "\", value:" <> val <> "}"
      tailp = RLProxy :: RLProxy tail
      tail = toFakeSumRecordMembers tailp

instance nilFakeSumRecordMembers :: FakeSumRecordMembers Nil where
  toFakeSumRecordMembers _ = mempty
