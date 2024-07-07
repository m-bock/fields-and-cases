{-
<!--
-}
module Readme where

import Data.String.Conversions (cs)
import qualified Data.Text as Txt
import FieldsAndCases (LabeledField (LabeledField))
import qualified FieldsAndCases as FnC
import Relude
import System.Process (callCommand)

{-
-->
-}

data Activity
  = Working
  | Studying {hours :: Int, subject :: Maybe Text}
  | Training {location :: Location}
  deriving
    (Show, Eq, Generic)

data Location
  = Indoor
  | Outdoor
  deriving
    (Show, Eq, Generic)

data Vector = Vector
  { x :: Int,
    y :: Int
  }
  deriving
    (Show, Eq, Generic)

data Person = Person
  { name :: Text,
    age :: Int,
    isStudent :: Bool,
    friends :: [Text],
    activity :: Activity,
    coordinates :: Vector
  }
  deriving
    (Show, Eq, Generic)

{-
...
-}

newtype RustCode = RustCode Text
  deriving (Show, Eq)
  deriving newtype (IsString, Semigroup, FnC.IsLang, ToText)

{-
...
-}

instance FnC.ToRef RustCode Text where
  toRef = "String"

instance FnC.ToRef RustCode Int where
  toRef = "i32"

instance FnC.ToRef RustCode Bool where
  toRef = "bool"

{-
...
-}

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode (Maybe a) where
  toRef =
    "Option<" <> FnC.ref @a <> ">"

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode [a] where
  toRef =
    "Vec<" <> FnC.ref @a <> ">"

{-
...
-}

instance FnC.ToRef RustCode Activity

instance FnC.ToRef RustCode Location

instance FnC.ToRef RustCode Vector

instance FnC.ToRef RustCode Person

{-
...
-}

genRustTypeDef :: FnC.TypeDef RustCode -> Text
genRustTypeDef (FnC.TypeDef {typeName = FnC.QualName {typeName}, cases}) =
  case cases of
    [FnC.Case {tagName, caseFields = Just (FnC.CaseLabeledFields fields)}]
      | typeName == tagName ->
          genStruct typeName fields
    cases ->
      genEnum typeName cases
  where
    genStruct :: Text -> [FnC.LabeledField RustCode] -> Text
    genStruct name fields =
      fold
        [ "struct " <> name,
          "{",
          foldMap genField fields,
          "}"
        ]

    genEnum :: Text -> [FnC.Case RustCode] -> Text
    genEnum name cases =
      fold
        [ "enum " <> name,
          "{",
          foldMap
            ( \FnC.Case {tagName, caseFields} ->
                fold
                  [ tagName,
                    case caseFields of
                      Nothing -> ""
                      Just (FnC.CaseLabeledFields fields) -> fold ["{", foldMap genField fields, "}"],
                    ","
                  ]
            )
            cases,
          "}"
        ]

    genField :: LabeledField RustCode -> Text
    genField (LabeledField {fieldName, fieldType}) = fold [fieldName, ":", toText fieldType, ","]

{-
...
-}

code :: Text
code =
  unlines
    [ "//! This is an auto generated Rust Module\n",
      genRustTypeDef $ FnC.toDef @Person,
      genRustTypeDef $ FnC.toDef @Activity,
      genRustTypeDef $ FnC.toDef @Location,
      genRustTypeDef $ FnC.toDef @Vector
    ]

{-
...
-}

main :: IO ()
main = do
  let filePath = "tests/Readme.rs"
  writeFile filePath (cs code)
  callCommand ("rustfmt --force " <> filePath)
