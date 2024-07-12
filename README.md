# fields-and-cases

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Example: Generate Rust and TypeScript types from Haskell](#example-generate-rust-and-typescript-types-from-haskell)
  - [Module setup](#module-setup)
  - [Define custom types](#define-custom-types)
  - [Define "type expression" types for target languages](#define-type-expression-types-for-target-languages)
  - [Define `TypeExpr` instances](#define-typeexpr-instances)
    - [Primitive types](#primitive-types)
    - [Composite types](#composite-types)
    - [Custom types](#custom-types)
  - [Convert `TypeDef` to text](#convert-typedef-to-text)
  - [Compose modules for the target language](#compose-modules-for-the-target-language)
  - [Write generated code to a file](#write-generated-code-to-a-file)
  - [Further ideas: JSON serialization](#further-ideas-json-serialization)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Example: Generate Rust and TypeScript types from Haskell

<!-- START:example -->
### Module setup

We'll need to activate the following language extensions:

```haskell
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
```

<!--

```haskell
module Readme where -- (main) where
```

-->


As well as those imports for this demo:

```haskell
import Control.Exception (catch, throw)
import qualified Data.Text as Txt
import qualified FieldsAndCases as FnC
import qualified GHC.IO.Exception as Ex
import Relude
import System.Process (callCommand)
import qualified Test.Tasty as Spec
import qualified Test.Tasty.HUnit as Spec
import GHC.IO.Exception (ExitCode(ExitSuccess))
```

### Define custom types

Let' say we have the following data types in Haskell:

```haskell
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
```

We use those types in other codebases that are written in different languages.
Now we want to have a flexible yet automated way to generate the equivalent data types in those languages.
We'll do so as an example for Rust and for TypeScript. The library is language agnostic and can be used for any language.

### Define "type expression" types for target languages

First we define a types that represents the type expressions of the target languages.
In this demo it's a simple newtype wrapper around Text.
That already works very well, but you could also define and use a custom AST type instead.
Most importantly it needs an instance of `FnC.IsTypeExpr`.
In our case we can derive all instances.



Rust:

```haskell
newtype Rust = Rust Text
  deriving (Show, Eq, IsString, Semigroup, ToText, FnC.IsTypeExpr)
```

TypeScript:

```haskell
newtype TypeScript = TypeScript Text
  deriving (Show, Eq, IsString, Semigroup, ToText, FnC.IsTypeExpr)
```

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



Rust:

```haskell
instance FnC.TypeExpr Bool Rust where
  typeExpr = "bool"

instance FnC.TypeExpr Int Rust where
  typeExpr = "i32"

instance FnC.TypeExpr Text Rust where
  typeExpr = "String"
```

TypeScript:

```haskell
instance FnC.TypeExpr Bool TypeScript where
  typeExpr = "boolean"

instance FnC.TypeExpr Int TypeScript where
  typeExpr = "number"

instance FnC.TypeExpr Text TypeScript where
  typeExpr = "string"
```

#### Composite types

And then add some instance for composite types.
We use `FnC.typeExpr` to recursively reference type arguments.



Rust:

```haskell
instance (FnC.TypeExpr a Rust) => FnC.TypeExpr (Maybe a) Rust where
  typeExpr =
    "Option<" <> FnC.typeExpr @a <> ">"

instance (FnC.TypeExpr a Rust) => FnC.TypeExpr [a] Rust where
  typeExpr =
    "Vec<" <> FnC.typeExpr @a <> ">"
```

TypeScript:

```haskell
instance (FnC.TypeExpr a TypeScript) => FnC.TypeExpr (Maybe a) TypeScript where
  typeExpr =
    "(null | " <> FnC.typeExpr @a <> ")"

instance (FnC.TypeExpr a TypeScript) => FnC.TypeExpr [a] TypeScript where
  typeExpr =
    "Array<" <> FnC.typeExpr @a <> ">"
```

#### Custom types

Until now we have covered the basic types. Now we define instances for our custom types.
Luckily they can be easily derived, we can even derive them each for all target languages at once:

```haskell
instance (FnC.IsTypeExpr lang) => FnC.TypeExpr Person lang

instance (FnC.IsTypeExpr lang) => FnC.TypeExpr Activity lang

instance (FnC.IsTypeExpr lang) => FnC.TypeExpr Place lang

instance (FnC.IsTypeExpr lang) => FnC.TypeExpr Vector lang
```

Now let's demonstrate what we can do with the definitions we have so far.
The library provides a function `toTypeDef`
that generates a `FnC.TypeDef` for a given type.
We need to pass two types via "visible type application":

```haskell
typeDefActivityRust1 :: FnC.TypeDef Rust
typeDefActivityRust1 = FnC.toTypeDef @Activity @Rust
```

This results in the following data:

```haskell
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
```

In a small unit test we can proof
that the manual and the auto generated type definitions are equal:

```haskell
unitTests :: Spec.TestTree
unitTests =
  Spec.testCase
    "toTypeDef"
    (Spec.assertEqual "" typeDefActivityRust1 typeDefActivityRust2)
```

### Convert `TypeDef` to text

After having seen the generated data we can now convert it to text.
It is very straightforward to implement,
we just need to pattern match on the cases of the type definition.
We don't need to deal with tricky wizardry like generics or typeclasses, this is all handled by the library:



Rust:

```haskell
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
```

TypeScript:

```haskell
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
```

### Compose modules for the target language

Since we want to generate code for the same types in multiple languages,
we can define a list of the types we want to export:

```haskell
type ExportTypes =
  '[ Person,
     Activity,
     Place,
     Vector
   ]
```

And finally we can define modules containing the generated code:

```haskell
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
```

### Write generated code to a file

And we can write the generated code to a file,
as well as format it with appropriate code formatters:

```haskell
main :: IO ()
main = do
  -- Verify the assertions from above
  Spec.defaultMain unitTests
    `catch` \e ->
      when (e /= ExitSuccess) $ throw e

  do
    let filePath = "tests/outputs/demo.rs"
    writeFile filePath (toString codeRust)
    callCommand ("rustfmt --force " <> filePath)

  do
    let filePath = "tests/outputs/demo.ts"
    writeFile filePath (toString codeTypeScript)
    callCommand ("npx prettier --write " <> filePath)
```

### Further ideas: JSON serialization

One obvious next step would be to generate JSON serialization code.
Because the types we generate for different languages
are not necessarily the same as the Haskell types,
they're just intended to be close enough to be useful.
If you look again at the value `typeDefActivityRust2`
that was provided as an example above,
it's evident that we could generate JSON serialization code from that.

<!-- END:example -->

The result will look like this:

<!-- START:exampleOutRust -->
```rust
//! This is an auto generated Rust Module

struct Person {
    name: String,
    age: i32,
    isStudent: bool,
    friends: Vec<String>,
    activity: Option<Activity>,
    coordinates: Vector,
}

enum Activity {
    Working,
    Studying { hours: i32, subject: Option<String> },
    Training { place: Place },
}

enum Place {
    Indoor,
    Outdoor,
}

struct Vector {
    x: i32,
    y: i32,
}

```
<!-- END:exampleOutRust -->

...

<!-- START:exampleOutTypeScript -->
```ts
// This is an auto generated TypeScript Module

type Person = {
  name: string;
  age: number;
  isStudent: boolean;
  friends: Array<string>;
  activity?: null | Activity;
  coordinates: Vector;
};

type Activity =
  | { tag: "Working" }
  | { tag: "Studying"; value: { hours: number; subject?: null | string } }
  | { tag: "Training"; value: { place: Place } };

type Place = "Indoor" | "Outdoor";

type Vector = { x: number; y: number };

```
<!-- END:exampleOutTypeScript -->
