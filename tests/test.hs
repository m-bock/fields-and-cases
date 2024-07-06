module Main where

import Data.String.Conversions (cs)
import Data.Text (replace)
import FieldsAndCases (Case (..), IsLang (..), PositionalField (..), QualName (..), ToDef, ToRef, TypeDef (..), toRef)
import qualified FieldsAndCases as FnC
import GHC.Generics
import qualified GHC.Generics as GHC
import Lima.Converter (Format (..), convertTo, def)
import qualified Readme
import Relude
import Spec (unitTests)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex (mkRegex, subRegex)

---

main :: IO ()
main = do
  genReadme
  defaultMain tests

genReadme :: IO ()
genReadme = do
  readmeMd <- readFileBS "README.md"

  readmeHs <- readFileBS "tests/Readme.hs"

  let readmeMd' = convertTo Hs Md def (cs readmeHs)

  Readme.main

  readmeOutput <- readFileBS "tests/Readme.rs"
  let readmeOutput' = "```rust\n" <> readmeOutput <> "\n```"

  let readmeMd'' =
        cs readmeMd
          & replaceSection "example" readmeMd'
          & replaceSection "exampleOut" (cs readmeOutput')

  writeFileBS "README.md" (cs readmeMd'')

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
