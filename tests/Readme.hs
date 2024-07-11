{-
### Module setup

We'll need to activate the following language extensions:
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
<!--
-}

module Readme (main) where

{-
-->
-}

{-
As well as those imports for this demo:
-}

import qualified Data.Text as Txt
import qualified FieldsAndCases as FnC
import Relude
import System.Process (callCommand)

{-
### Define custom types

Let' say we have the following data types in Haskell:
-}

data Activity
  = Working
  | Studying {hours :: Int, subject :: Maybe Text}
  | Training {place :: Place}
  deriving
    (Show, Eq, Generic)

data Place
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
    activity :: Maybe Activity,
    coordinates :: Vector
  }
  deriving
    (Show, Eq, Generic)

{-

We use those types in other codebases that are written in different languages.
Now we want to have a flexible yet automated way to generate the equivalent data types in those languages.
We'll do so as an example for Rust and for TypeScript. The library is language agnostic and can be used for any language.

### Define "language types" for target language

First we define a types that represents the type expressions of the target languages.
In this demo it's a simple newtype wrapper around Text.
That already works very well, but you could also define and use a custom AST type instead.
Most importantly it needs an instance of `FnC.IsTypeExpr`.
In our case we can derive all instances.

-}

{-
Rust:
-}

newtype Rust = Rust Text
  deriving (Show, Eq, IsString, Semigroup, ToText, FnC.IsTypeExpr)

{-
TypeScript:
-}

newtype TypeScript = TypeScript Text
  deriving (Show, Eq, IsString, Semigroup, ToText, FnC.IsTypeExpr)

{-

### Define instances

Now we define instances for the `FnC.TypeExpr` typeclass.
It's a typeclass parameterized by two types:
- The type we want to generate a reference for (`Text`, `Int`, `Bool`, `Maybe a`, `[a]`, ...)
- The language type (`Rust` or `TypeScript` in this case)

This works like the well known `Show` typeclass.

#### Primitive types

Let's start with instance for the primitive types.
Note that since we are using 'OverloadedStrings' we can use string literals directly,
`typeExpr = "bool"` is equivalent to `typeExpr = fromString "bool" :: Rust`:

-}

{-
Rust:
-}

instance FnC.TypeExpr Bool Rust where
  typeExpr = "bool"

instance FnC.TypeExpr Int Rust where
  typeExpr = "i32"

instance FnC.TypeExpr Text Rust where
  typeExpr = "String"

{-
TypeScript:
-}

instance FnC.TypeExpr Bool TypeScript where
  typeExpr = "boolean"

instance FnC.TypeExpr Int TypeScript where
  typeExpr = "number"

instance FnC.TypeExpr Text TypeScript where
  typeExpr = "string"

{-

#### Composite types

And then add some instance for composite types.
We use `FnC.typeExpr` to recursively reference type arguments.

-}

{-
Rust:
-}

instance (FnC.TypeExpr a Rust) => FnC.TypeExpr (Maybe a) Rust where
  typeExpr =
    "Option<" <> FnC.typeExpr @a <> ">"

instance (FnC.TypeExpr a Rust) => FnC.TypeExpr [a] Rust where
  typeExpr =
    "Vec<" <> FnC.typeExpr @a <> ">"

{-
TypeScript:
-}

instance (FnC.TypeExpr a TypeScript) => FnC.TypeExpr (Maybe a) TypeScript where
  typeExpr =
    "(null | " <> FnC.typeExpr @a <> ")"

instance (FnC.TypeExpr a TypeScript) => FnC.TypeExpr [a] TypeScript where
  typeExpr =
    "Array<" <> FnC.typeExpr @a <> ">"

{-

#### Custom types

Until now we have covered the basic types. Now we define instances for our custom types.
Luckily they can be easily derived, we can even derive them each for all target languages at once:

-}

instance (FnC.IsTypeExpr lang) => FnC.TypeExpr Person lang

instance (FnC.IsTypeExpr lang) => FnC.TypeExpr Activity lang

instance (FnC.IsTypeExpr lang) => FnC.TypeExpr Place lang

instance (FnC.IsTypeExpr lang) => FnC.TypeExpr Vector lang


{-

### Define

However, we need a function that generates the Rust code for a given type definition.
It is very straightforward to implement, we just need to pattern match on the cases of the type definition.
We don't need to deal with tricky wizardry like generics or typeclasses, this is all handled by the library:

-}

{- ... -}

printRust :: FnC.TypeDef Rust -> Text
printRust typeDef@(FnC.TypeDef {qualifiedName = FnC.QualifiedName {typeName}, cases}) =
  case FnC.matchRecordLikeDataType typeDef of
    Just (tagName, fields) ->
      fold ["struct " <> typeName, "{", foldMap printField fields, "}", "\n"]
    Nothing ->
      fold ["enum " <> typeName, "{", foldMap printCase cases, "}", "\n"]
  where
    printField (FnC.Field {fieldName, fieldType}) =
      fold
        [fieldName, ":", toText fieldType, ","]

    printCase (FnC.Case {tagName, caseArgs}) =
      fold
        [ tagName,
          case caseArgs of
            Nothing -> ","
            Just (FnC.CaseFields fields) ->
              fold ["{", foldMap printField fields, "}", ","]
        ]

{- ... -}

printTypeScript :: FnC.TypeDef TypeScript -> Text
printTypeScript typeDef@(FnC.TypeDef {qualifiedName = FnC.QualifiedName {typeName}, cases}) =
  case typeDef of
    (FnC.matchRecordLikeDataType -> Just (tagName, fields)) ->
      fold ["type " <> typeName, " = {", foldMap printField fields, "}", "\n"]
    (FnC.isEnumWithoutData -> True) ->
      fold ["type " <> typeName, " = ", foldMap printCaseNoData cases, "\n"]
    _ ->
      fold ["type " <> typeName, " = ", foldMap printCase cases, "\n"]
  where
    printField (FnC.Field {fieldName, fieldType = TypeScript code}) =
      fold
        [fieldName, if Txt.isPrefixOf "(null |" code then "?" else "", ":", code, ";"]

    printCase (FnC.Case {tagName, caseArgs}) =
      fold
        [ "| {",
          "tag: '" <> tagName <> "'",
          case caseArgs of
            Nothing -> ","
            Just (FnC.CaseFields fields) ->
              fold [", value: {", foldMap printField fields, "}", ","],
          "}"
        ]

    printCaseNoData (FnC.Case {tagName}) =
      "| '" <> tagName <> "'"

{-

### Compose a module for the target language

Finally we can define a rust module that contains the generated code:
-}

type ExportTypes =
  '[ Person,
     Activity,
     Place,
     Vector
   ]

codeRust :: Text
codeRust =
  unlines
    [ "//! This is an auto generated Rust Module\n",
      unlines $ map printRust (FnC.toTypeDefs @ExportTypes @Rust)
    ]

codeTypeScript :: Text
codeTypeScript =
  unlines
    [ "// This is an auto generated TypeScript Module\n",
      unlines $ map printTypeScript (FnC.toTypeDefs @ExportTypes @TypeScript)
    ]

{-

### Write generated code to a file

And we can write the generated code to a file, as well as format it with rustfmt:
-}

main :: IO ()
main = do
  do
    let filePath = "tests/outputs/demo.rs"
    writeFile filePath (toString codeRust)
    callCommand ("rustfmt --force " <> filePath)

  do
    let filePath = "tests/outputs/demo.ts"
    writeFile filePath (toString codeTypeScript)
    callCommand ("npx prettier --write " <> filePath)

{-

### Bonus: Generate JSON serialization

-}

printRustSerialize :: FnC.TypeDef Rust -> Text
printRustSerialize (FnC.TypeDef {qualifiedName = FnC.QualifiedName {typeName}, cases}) =
  case cases of
    [FnC.Case {tagName, caseArgs = Just (FnC.CaseFields fields)}]
      | typeName == tagName ->
          printStruct typeName fields <> "\n"
    cases ->
      error "Only structs are supported in this demo"
  where
    printStruct :: Text -> [FnC.Field Rust] -> Text
    printStruct name fields =
      fold
        [ "impl Serialize for " <> name,
          "{",
          "  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>",
          "  where",
          "    S: Serializer,",
          "  {",
          "    let mut state = serializer.serialize_struct(\"" <> name <> "\", " <> show (length fields) <> ")?;",
          foldMap printField fields,
          "    state.end()",
          "  }",
          "}"
        ]

    printField :: FnC.Field Rust -> Text
    printField = \case
      (FnC.Field {fieldName, fieldType}) ->
        fold ["state.serialize_field(\"", fieldName, "\", &self.", fieldName, ")?;"]
