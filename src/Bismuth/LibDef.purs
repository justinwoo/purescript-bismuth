module Bismuth.LibDef where

import Prelude

import Bismuth (class HasFlowRep, toFlowRep)
import Data.Foldable (foldMap, intercalate)
import Data.StrMap (StrMap, toArrayWithKey)
import Type.Proxy (Proxy(..))

-- | Extra declarations to be added to module definitions
type Declarations = Array String

-- | All exported items of our exports
type Exports = StrMap String

-- | A convenience function for creating module definitions
-- | Takes a the module name, an array of declarations to be inserted, and exports
createModuleDefinition :: String -> Declarations -> Exports -> String
createModuleDefinition s declarations exports =
  "declare module '" <> s <> "' {\n"
  <> foldMap (flip (<>) ";\n") declarations <> "\n"
  <> "declare module.exports: {\n"
  <> intercalate ",\n" (toArrayWithKey renderItem exports)
  <> "\n}\n}"
  where
    renderItem k v = k <> ": " <> v

-- | A convenience function for declaring a flow type in a lib definiton
declareFlowType :: forall proxy a
   . HasFlowRep a
  => String -> proxy a -> String
declareFlowType name _ =
  "declare type " <> name <> "=" <> ty
  where
    ty = toFlowRep (Proxy :: Proxy a)

-- | A convenience function for declaring types taking a concrete value over a proxy.
declareFlowType' :: forall a
   . HasFlowRep a
  => String
  -> a
  -> String
declareFlowType' s a = declareFlowType s (Proxy :: Proxy a)
