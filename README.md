# fields-and-cases

## Example

<!-- START:example -->
<!--

```haskell
module Readme where

import Data.String.Conversions (cs)
import qualified Data.Text as Txt
import FieldsAndCases (LabeledField (LabeledField))
import qualified FieldsAndCases as FnC
import Relude
import System.Process (callCommand)
```

-->


### Define custom types

Let' say we have the following data types in Haskell:

```haskell
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
```

We use those types in other codebases that are written in different languages.
Now we want to have a flexible yet automated way to generate the equivalent data types in those languages.
We'll do so as an example for Rust. The library is language agnostic and can be used for any language.

### Define type for target language


First we define a type that represents the Rust code. In this demo it's a simple newtype wrapper around Text.
That already works very well, but you could also define and use a custom AST type instead.
All it needs is an instance of IsLang and ToText. In our simple case we can derive those instances.

```haskell
newtype RustCode = RustCode Text
  deriving (Show, Eq)
  deriving newtype (IsString, Semigroup, FnC.IsLang, ToText)
```

Now we define instances for the ToRef typeclass. It's a typeclass parameterized by two types:
- The language type (RustCode in this case)
- The type we want to generate a reference for (Text, Int, Bool, Maybe a, [a], ...)

### Define instances for primitive types


Let's start with instance for the primitive types:

```haskell
instance FnC.ToRef RustCode Text where
  toRef = "String"

instance FnC.ToRef RustCode Int where
  toRef = "i32"

instance FnC.ToRef RustCode Bool where
  toRef = "bool"
```

### Define instances for composite types

And then add some instance for composite types. We use the `ref` function to reference type arguments:

```haskell
instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode (Maybe a) where
  toRef =
    "Option<" <> FnC.ref @a <> ">"

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode [a] where
  toRef =
    "Vec<" <> FnC.ref @a <> ">"
```

Until now we have covered the basic types. Now we define instances for our buisness types.
We don't need to define all those fields and cases manually:

```haskell
instance FnC.ToRef RustCode Activity

instance FnC.ToRef RustCode Location

instance FnC.ToRef RustCode Vector

instance FnC.ToRef RustCode Person
```

### Define 


However, we need a function that generates the Rust code for a given type definition.
It is very straightforward to implement, we just need to pattern match on the cases of the type definition.
We don't need to deal with tricky wizardry like generics or typeclasses, this is all handled by the library:

```haskell
genRustTypeDef :: FnC.TypeDef RustCode -> Text
genRustTypeDef (FnC.TypeDef {typeName = FnC.QualName {typeName}, cases}) =
  case cases of
    [FnC.Case {tagName, caseFields = Just (FnC.CaseLabeledFields fields)}]
      | typeName == tagName ->
          genStruct typeName fields <> "\n"
    cases ->
      genEnum typeName cases <> "\n"
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
          foldMap genCase cases,
          "}"
        ]

    genCase :: FnC.Case RustCode -> Text
    genCase (FnC.Case {tagName, caseFields}) =
      fold
        [ tagName,
          case caseFields of
            Nothing -> ""
            Just (FnC.CaseLabeledFields fields) ->
              fold ["{", foldMap genField fields, "}"],
          ","
        ]

    genField :: LabeledField RustCode -> Text
    genField (LabeledField {fieldName, fieldType}) =
      fold [fieldName, ":", toText fieldType, ","]
```

### Compose a module in the target language


Finally we can define a rust module that contains the generated code:

```haskell
code :: Text
code =
  unlines
    [ "//! This is an auto generated Rust Module\n",
      genRustTypeDef $ FnC.toDef @Person,
      genRustTypeDef $ FnC.toDef @Activity,
      genRustTypeDef $ FnC.toDef @Location,
      genRustTypeDef $ FnC.toDef @Vector
    ]
```

### Write generated code to a file

And we can write the generated code to a file, as well as format it with rustfmt:

```haskell
main :: IO ()
main = do
  let filePath = "tests/Readme.rs"
  writeFile filePath (cs code)
  callCommand ("rustfmt --force " <> filePath)
```

<!-- END:example -->

The result will look like this:

<!-- START:exampleOut -->
```rust
//! This is an auto generated Rust Module

struct Person {
    name: String,
    age: i32,
    isStudent: bool,
    friends: Vec<String>,
    activity: Activity,
    coordinates: Vector,
}

enum Activity {
    Working,
    Studying { hours: i32, subject: Option<String> },
    Training { location: Location },
}

enum Location {
    Indoor,
    Outdoor,
}

struct Vector {
    x: i32,
    y: i32,
}

```
<!-- END:exampleOut -->
