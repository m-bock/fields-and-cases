{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String.Conversions (cs)
import Data.Text (replace)
import GHC.Generics
import qualified GHC.Generics as GHC
--import Lima.Converter (Format (..), convertTo, def)
import qualified Readme
import Relude
import Spec (unitTests)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex (mkRegex, subRegex)

main :: IO ()
main = do
  defaultMain tests

-- genReadme :: IO ()
-- genReadme = do
--   readmeMd <- readFileBS "README.md"

--   readmeHs <- readFileBS "tests/Readme.hs"

--   let readmeExample = convertTo Hs Md def (cs readmeHs)

--   Readme.main

--   readmeOutputRust <- readFileBS "tests/outputs/demo.rs"
--   readmeOutputTypeScript <- readFileBS "tests/outputs/demo.ts"

--   let readmeMd' =
--         cs readmeMd
--           & replaceSection "example" readmeExample
--           & replaceSection
--             "exampleOutRust"
--             ("```rust\n" <> cs readmeOutputRust <> "\n```")
--           & replaceSection
--             "exampleOutTypeScript"
--             ("```ts\n" <> cs readmeOutputTypeScript <> "\n```")

--   writeFileBS "README.md" (cs readmeMd')

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
