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

...

```haskell
newtype RustCode = RustCode Text
  deriving (Show, Eq)
  deriving newtype (IsString, Semigroup, FnC.IsLang, ToText)
```

...

```haskell
instance FnC.ToRef RustCode Text where
  toRef = "String"

instance FnC.ToRef RustCode Int where
  toRef = "i32"

instance FnC.ToRef RustCode Bool where
  toRef = "bool"
```

...

```haskell
instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode (Maybe a) where
  toRef =
    "Option<" <> FnC.ref @a <> ">"

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode [a] where
  toRef =
    "Vec<" <> FnC.ref @a <> ">"
```

...

```haskell
instance FnC.ToRef RustCode Activity

instance FnC.ToRef RustCode Location

instance FnC.ToRef RustCode Vector

instance FnC.ToRef RustCode Person
```

...

```haskell
genRustTypeDef :: FnC.TypeDef RustCode -> Text
genRustTypeDef (FnC.TypeDef {typeName = FnC.QualName {typeName}, cases}) =
  case cases of
    [FnC.Case {tagName, caseFields = Just (FnC.CaseLabeledFields fields)}]
      | typeName == tagName ->
          genRustStruct typeName fields
    cases ->
      genRustEnum typeName cases
```

...

```haskell
genRustStruct :: Text -> [FnC.LabeledField RustCode] -> Text
genRustStruct name fields =
  fold
    [ "struct " <> name,
      "{",
      foldMap (\(LabeledField {fieldName, fieldType}) -> fold [fieldName, ":", toText fieldType, ","]) fields,
      "}"
    ]

genRustEnum :: Text -> [FnC.Case RustCode] -> Text
genRustEnum name cases =
  fold
    [ "enum " <> name,
      "{",
      foldMap
        ( \FnC.Case {tagName, caseFields} ->
            fold
              [ tagName,
                case caseFields of
                  Nothing -> ""
                  Just (FnC.CaseLabeledFields fields) ->
                    fold
                      [ "{",
                        foldMap (\(LabeledField {fieldName, fieldType}) -> fold [fieldName, ":", toText fieldType, ","]) fields,
                        "}"
                      ],
                ","
              ]
        )
        cases,
      "}"
    ]
```

...

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

...

```haskell
main :: IO ()
main = do
  let filePath = "tests/Readme.rs"
  writeFile filePath (cs code)
  callCommand ("rustfmt --force " <> filePath)
```

<!-- END:example -->

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
