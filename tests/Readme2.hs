{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Readme2 where

import Data.String.Conversions (cs)
import qualified Data.Text as Txt
import FieldsAndCases (Case (..), Cases (..), Field (..), Fields (..), IsLang, PositionalFields (..), QualName (..), Ref (..), ToRef, TypeDef (..))
import qualified FieldsAndCases as FnC
import Relude

{-
# fields-and-cases

Generating other languages' types from Haskell types is a straightforward task.
We a custom type class we can do this as follows:
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

instance FnC.ToRef RustCode Text where
  toRef = "String"

instance FnC.ToRef RustCode Int where
  toRef = "i32"

instance FnC.ToRef RustCode Bool where
  toRef = "bool"

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode (Maybe a) where
  toRef =
    "Option<" <> ref @a <> ">"

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode [a] where
  toRef =
    "Vec<" <> ref @a <> ">"

genRustTypeDef :: TypeDef RustCode -> Text
genRustTypeDef (TypeDef {typeName = QualName {typeName}, cases}) = case cases of
  -- Match Data Type with feilds (Record, Struct, etc..)
  Cases [CaseWithFields {tagName, fields = Fields fields}]
    | typeName == tagName ->
        unlines
          [ "struct " <> typeName <> " {",
            fields
              & map (\entry -> "  " <> entry.fieldName <> ": " <> toText entry.fieldType)
              & Txt.intercalate ",\n",
            "}"
          ]
  -- Math Data Type with cases (Tagged Union, Sum Type, etc..)
  Cases cases ->
    unlines
      [ "enum " <> typeName <> " {",
        cases
          & map
            ( \case
                CaseWithFields {tagName, fields = Fields fields} ->
                  unlines
                    [ "  " <> tagName <> " {",
                      fields
                        & map (\entry -> "    " <> entry.fieldName <> ": " <> toText entry.fieldType)
                        & Txt.intercalate ",\n",
                      "  }"
                    ]
                CaseNoFields {tagName} -> "  " <> tagName
                CaseWithPositionalFields {tagName, positionalFields = PositionalFields fields} ->
                  fields
                    & map (\entry -> "    " <> toText entry.fieldType)
                    & Txt.intercalate ", "
                    & \case
                      "" -> "  " <> tagName
                      x -> "  " <> tagName <> " (" <> x <> ")"
            )
          & Txt.intercalate ",\n",
        "}"
      ]

code :: Text
code =
  unlines
    [ "//! This is an auto generated Rust Module",
      "",
      "",
      "/// Auto generated Person type",
      genRustTypeDef $ FnC.toDef @Person,
      "",
      "/// Auto generated Activity type",
      genRustTypeDef $ FnC.toDef @Activity,
      "",
      "/// Custom Location type defition in case you want to opt out of the auto generation",
      "struct " <> toText (ref @Location @RustCode) <> "(i32, i32)",
      "",
      "/// Auto generated Vector type",
      genRustTypeDef $ FnC.toDef @Vector
    ]

r =
  [ "struct Person {",
    "  name: String,",
    "  age: i32,",
    "  isStudent: bool,",
    "  friends: Vec<String>,",
    "  activity: Activity",
    "}",
    "",
    "enum Activity {",
    "  Working { hard: bool, hours: i32 },",
    "  Studying { hours: i32, subject: String },",
    "  Sleeping { dream: Option<String> }",
    "}",
    ""
  ]