{-
<!--
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Readme`  ` where

{-

-}

import Data.String.Conversions (cs)
import qualified Data.Text as Txt
import qualified FieldsAndCases as FnC
import Relude

{-
--!>
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
  deriving newtype (IsString, Semigroup, FnC.IsLang, ToText)

{- --- -}

instance FnC.ToRef RustCode Activity

instance FnC.ToRef RustCode Location

instance FnC.ToRef RustCode Vector

instance FnC.ToRef RustCode Person

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
    "Option<" <> FnC.ref @a <> ">"

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode [a] where
  toRef =
    "Vec<" <> FnC.ref @a <> ">"

{- --- -}


genRustTypeDef :: FnC.TypeDef RustCode -> Text
genRustTypeDef (FnC.TypeDef {typeName = FnC.QualName {typeName}, cases}) =
  case cases of
    [FnC.Case {tagName, caseFields = Just (FnC.CaseLabeledFields fields)}]
      | typeName == tagName ->
          genRustStruct typeName fields
    cases ->
      genRustEnum typeName cases

{- --- -}

genRustStruct :: Text -> [FnC.LabeledField RustCode] -> Text
genRustStruct typeName fields =
  unlines
    [ "struct " <> typeName <> "{",
      fields
        & foldMap (\(FnC.LabeledField {fieldName, fieldType}) -> "  " <> fieldName <> ": " <> toText fieldType <> ",\n"),
      "}"
    ]

genRustEnum :: Text -> [FnC.Case RustCode] -> Text
genRustEnum typeName cases =
  unlines
    [ "enum " <> typeName <> " {",
      cases
        & map (\(FnC.Case {tagName, caseFields}) -> "  " <> tagName <> " " <> genFields caseFields)
        & Txt.intercalate ",\n",
      "}"
    ]
  where
    genFields :: Maybe (FnC.CaseFields RustCode) -> Text
    genFields = \case
      Nothing -> ""
      Just (FnC.CaseLabeledFields fields) ->
        unwords
          [ "{",
            fields
              & map (\(FnC.LabeledField {fieldName, fieldType}) -> fieldName <> ": " <> toText fieldType)
              & Txt.intercalate ", ",
            "}"
          ]
      Just (FnC.CasePositionalFields fields) -> error "positional fields not supported in this demo"

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