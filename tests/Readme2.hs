{-

-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Readme2 where

{-

-}

import Data.String.Conversions (cs)
import qualified Data.Text as Txt
import FieldsAndCases (Case (..), CaseFields (..), IsLang, LabeledField (..), LabeledFields (..), QualName (..), Ref (..), ToRef, TypeDef (..))
import qualified FieldsAndCases as FnC
import Relude

{-
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

{- --- -}

newtype RustCode = RustCode Text
  deriving (Show, Eq)
  deriving newtype (IsString, Semigroup, IsLang, ToText)

{- --- -}

instance ToRef RustCode Activity

instance ToRef RustCode Location

instance ToRef RustCode Vector

instance ToRef RustCode Person

{- --- -}

instance FnC.ToRef RustCode Text where
  toRef = "String"

instance FnC.ToRef RustCode Int where
  toRef = "i32"

instance FnC.ToRef RustCode Bool where
  toRef = "bool"

{- --- -}

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode (Maybe a) where
  toRef =
    "Option<" <> ref @a <> ">"

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode [a] where
  toRef =
    "Vec<" <> ref @a <> ">"

{- --- -}

genRustTypeDef :: TypeDef RustCode -> Text
genRustTypeDef (TypeDef {typeName = QualName {typeName}, cases}) =
  case cases of
    [Case {tagName, caseFields = Just (CaseLabeledFields fields)}]
      | typeName == tagName ->
          genStruct typeName fields
    cases ->
      genEnum typeName cases

{- --- -}

genStruct :: Text -> [LabeledField RustCode] -> Text
genStruct typeName fields =
  unlines
    [ "struct " <> typeName <> "{",
      fields
        & map (\(LabeledField {fieldName, fieldType}) -> fieldName <> ": " <> toText fieldType)
        & Txt.intercalate ", ",
      "}"
    ]

genEnum :: Text -> [Case RustCode] -> Text
genEnum typeName cases =
  unlines
    [ "enum " <> typeName <> " {",
      cases
        & map (\(Case {tagName, caseFields}) -> tagName <> " " <> "")
        & Txt.intercalate ",\n",
      "}"
    ]
  where
    genFields :: Maybe (CaseFields RustCode) -> Text
    genFields = \case
      Nothing -> ""
      Just (CaseLabeledFields fields) ->
        unwords
          [ "{",
            fields
              & map (\(LabeledField {fieldName, fieldType}) -> fieldName <> ": " <> toText fieldType)
              & Txt.intercalate ", ",
            "}"
          ]
      Just (CasePositionalFields fields) -> error "positional fields not supported in this demo"

{- --- -}

code :: Text
code =
  unlines
    [ "//! This is an auto generated Rust Module\n",
      genRustTypeDef $ FnC.toDef @Person,
      genRustTypeDef $ FnC.toDef @Activity,
      genRustTypeDef $ FnC.toDef @Location,
      genRustTypeDef $ FnC.toDef @Vector
    ]

{- --- -}

main :: IO ()
main = do
  writeFile "tests/Readme2.rs" (cs code)