module Test.Main where

import Prelude

import Bismuth (generateFlowType)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2)
import Data.Identity (Identity(..))
import Data.Nullable (Nullable)
import Data.StrMap (StrMap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Text.Prettier (defaultOptions, format)
import Type.Proxy (Proxy(..))

type A =
  { a :: Number
  , b :: String
  , c :: { d :: String }
  , e :: Array String
  , f :: Nullable String
  , g :: Number -> Number -> Number
  , h :: Fn2 Number Number Number
  , i :: Fn2 Number (Fn2 Number Number Number) Number
  , k :: StrMap Number
  , l :: Foreign
  , m :: Eff () String
  }

type VariantTest = Variant
  ( a :: String
  , b :: Number
  , c :: Boolean
  )

variantTestValue :: VariantTest
variantTestValue = inj (SProxy :: SProxy "a") "string"

main :: _
main = run [consoleReporter] do
  describe "purescript-bismuth" do
    describe "codegen" do
      it "can generate types" do
        liftEff generateFlowFile

generateFlowFile :: _
generateFlowFile = writeTextFile UTF8 "./test/generated.flow.js" values
  where
    values = format defaultOptions $ "//@flow\n" <> intercalate "\n"
      [ generateFlowType "A" (Identity variantTestValue) -- you can use Identity for values with a generic "proxy"
      , generateFlowType "VariantTest" (Proxy :: Proxy VariantTest)
      ]
