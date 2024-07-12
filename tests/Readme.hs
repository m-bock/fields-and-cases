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
{-# LANGUAGE NoImplicitPrelude #-}

{-
<!--
-}

module Readme where -- (main) where

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
import qualified Test.Tasty as Spec
import qualified Test.Tasty.HUnit as Spec

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

### Define "type expression" types for target languages

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

### Define `TypeExpr` instances

Now we define instances for the `FnC.TypeExpr` typeclass.
It's a typeclass parameterized by two types:
- The type we want to generate a reference for (`Text`, `Int`, `Bool`, `Maybe a`, `[a]`, ...)
- The language type (`Rust` or `TypeScript` in this case)

This works like the well known `Show` typeclass.
With the difference that we don't show values but types.

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
Now let's demonstrate what we can do with the definitions we have so far.
The library provides a function `toTypeDef`
that generates a `FnC.TypeDef` for a given type.
We need to pass two types via "visible type application":
-}

typeDefActivityRust1 :: FnC.TypeDef Rust
typeDefActivityRust1 = FnC.toTypeDef @Activity @Rust

{-
This results in the following data:
-}
typeDefActivityRust2 :: FnC.TypeDef Rust
typeDefActivityRust2 =
  FnC.TypeDef
    { qualifiedName = FnC.QualifiedName {moduleName = "Readme", typeName = "Activity"},
      cases =
        [ FnC.Case
            { tagName = "Working",
              caseArgs = Nothing
            },
          FnC.Case
            { tagName = "Studying",
              caseArgs =
                Just
                  ( FnC.CaseFields
                      [ FnC.Field {fieldName = "hours", fieldType = Rust "i32"},
                        FnC.Field {fieldName = "subject", fieldType = Rust "Option<String>"}
                      ]
                  )
            },
          FnC.Case
            { tagName = "Training",
              caseArgs =
                Just
                  ( FnC.CaseFields
                      [ FnC.Field {fieldName = "place", fieldType = Rust "Place"}
                      ]
                  )
            }
        ]
    }

{-
In a small unit test we can proof
that the manual and the auto generated type definitions are equal:
-}

unitTests :: Spec.TestTree
unitTests =
  Spec.testCase
    "toTypeDef"
    (Spec.assertEqual "" typeDefActivityRust1 typeDefActivityRust2)

{-

### Convert `TypeDef` to text

After having seen the generated data we can now convert it to text.
It is very straightforward to implement,
we just need to pattern match on the cases of the type definition.
We don't need to deal with tricky wizardry like generics or typeclasses, this is all handled by the library:

-}

{-
Rust:
-}

printRustDef :: FnC.TypeDef Rust -> Text
printRustDef = unwords . printType
  where
    printType typeDef@(FnC.TypeDef {qualifiedName = FnC.QualifiedName {typeName}, cases}) =
      case FnC.matchRecordLikeDataType typeDef of
        Just (tagName, fields) ->
          ["struct", typeName, "{"] <> concatMap printField fields <> ["}\n"]
        Nothing ->
          ["enum", typeName, "{"] <> concatMap printCase cases <> ["}\n"]

    printField (FnC.Field {fieldName, fieldType}) =
      [fieldName, ":", toText fieldType, ","]

    printCase (FnC.Case {tagName, caseArgs}) =
      case caseArgs of
        Nothing ->
          [tagName, ","]
        Just (FnC.CaseFields fields) ->
          [tagName, "{"] <> concatMap printField fields <> ["},"]

{-
TypeScript:
-}

printTypeScriptDef :: FnC.TypeDef TypeScript -> Text
printTypeScriptDef = unwords . printDef
  where
    printDef typeDef@(FnC.TypeDef {qualifiedName = FnC.QualifiedName {typeName}}) =
      ["type", typeName, "="] <> printType typeDef <> ["\n"]

    printType typeDef@(FnC.TypeDef {cases}) =
      case FnC.matchRecordLikeDataType typeDef of
        Just (tagName, fields) ->
          ["{"] <> concatMap printField fields <> ["}"]
        Nothing ->
          concatMap (printCase $ FnC.isEnumWithoutData typeDef) cases

    printField (FnC.Field {fieldName, fieldType}) =
      [fieldName, if omittable then "?" else "", ":", toText fieldType, ";"]
      where
        omittable = Txt.isPrefixOf "(null |" $ toText fieldType

    printCase noData (FnC.Case {tagName, caseArgs}) =
      ["|"]
        <> if noData
          then ["'" <> tagName <> "'"]
          else ["{", "tag:", "'" <> tagName <> "'"] <> printCaseArgs caseArgs <> ["}"]

    printCaseArgs = \case
      Nothing ->
        []
      Just (FnC.CaseFields fields) ->
        [",", "value:", "{"] <> concatMap printField fields <> ["}"]

{-

### Compose modules for the target language

Since we want to generate code for the same types in multiple languages,
we can define a list of the types we want to export:
-}

type ExportTypes =
  '[ Person,
     Activity,
     Place,
     Vector
   ]

{-
And finally we can define modules containing the generated code:
-}

codeRust :: Text
codeRust =
  unlines
    [ "//! This is an auto generated Rust Module\n",
      unlines $ map printRustDef (FnC.toTypeDefs @ExportTypes @Rust)
    ]

codeTypeScript :: Text
codeTypeScript =
  unlines
    [ "// This is an auto generated TypeScript Module\n",
      unlines $ map printTypeScriptDef (FnC.toTypeDefs @ExportTypes @TypeScript)
    ]

{-

### Write generated code to a file

And we can write the generated code to a file,
as well as format it with appropriate code formatters:
-}

main :: IO ()
main = do
  Spec.defaultMain unitTests

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
