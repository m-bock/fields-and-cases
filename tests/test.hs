module Main where

import Data.String.Conversions (cs)
import Data.Text (replace)
import FieldsAndCases (Case (..), IsLang (..), PositionalField (..), PositionalFields (..), QualName (..), ToDef, ToRef, TypeDef (..), toRef)
import qualified FieldsAndCases as FnC
import GHC.Generics
import qualified GHC.Generics as GHC
import Lima.Converter (Format (..), convertTo, def)
import Relude
import Spec (unitTests)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex (mkRegex, subRegex)
import qualified Readme2
---

main :: IO ()
main = do
  genReadme
  defaultMain tests

genReadme :: IO ()
genReadme = do
  readmeMd <- readFileBS "README.md"

  readmeHs2 <- readFileBS "tests/Readme2.hs"

  let readmeMd2 = convertTo Hs Md def (cs readmeHs2)

  Readme2.main

  readmeOutput2 <- readFileBS "tests/Readme2.rs"
  let readmeOutput2' = "```rust\n" <> readmeOutput2 <> "\n```"


  let readmeMd' =
        cs readmeMd
          & replaceSection "example2" readmeMd2
          & replaceSection "example2out" (cs readmeOutput2')

  -- readmeHs <- readFileBS "tests/Readme.hs"
  writeFileBS "README.md" (cs readmeMd')

tests :: TestTree
tests = testGroup "Tests" [unitTests]

replaceSection :: Text -> Text -> Text -> Text
replaceSection name new doc =
  let pattern = "<!-- START:" <> name <> " -->(.|\n)*?<!-- END:" <> name <> " -->"
      replacement = "<!-- START:" <> name <> " -->\n" <> new <> "\n<!-- END:" <> name <> " -->"
   in regexReplace (cs pattern) replacement doc

-- Replace all occurrences of a pattern in a string
regexReplace :: Text -> Text -> Text -> Text
regexReplace pattern replacement input =
  let regex = mkRegex $ cs pattern
   in cs $ subRegex regex (cs input) (cs replacement)