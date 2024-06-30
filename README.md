```haskell
module Readme where

import FieldsAndCases (Case (..), Cases (..), QualName (..), TextLang, TypeDef (..))
import qualified FieldsAndCases as FnC
import Relude
import FieldsAndCases (Fields(..))
import FieldsAndCases (Field(..))
import qualified Data.Text as Txt
import Data.String.Conversions (cs)
import FieldsAndCases (ToRef)
import FieldsAndCases (IsLang)
```

# fields-and-cases

Generating other languages' types from Haskell types is a straightforward task.
We a custom type class we can do this as follows:

```haskell
data Activity
  = Working {hard :: Bool, hours :: Int}
  | Studying {hours :: Int, subject :: Text}
  | Sleeping {dream :: Text}
  deriving (Show, Eq, Generic, ToRef RustCode)

data Person = Person
  { name :: Text,
    age :: Int,
    isStudent :: Bool,
    friends :: [Text],
    activity :: Activity
  }
  deriving (Show, Eq)
```

<!-- --- -->

```haskell
class ToRust a where
  toRust :: Text

instance ToRust Text where
  toRust = "String"

instance ToRust Int where
  toRust = "i32"

instance ToRust Bool where
  toRust = "bool"

instance (ToRust a) => ToRust [a] where
  toRust = "Vec<" <> toRust @a <> ">"

instance ToRust Activity where
  toRust = "Activity"
```

<!-- --- -->

```haskell
rustModule :: Text
rustModule =
  unlines
    [ "struct Person {",
      "  name: " <> toRust @Text <> ",",
      "  age: " <> toRust @Int <> ",",
      "  is_student: " <> toRust @Bool <> ",",
      "  friends: " <> toRust @[Text] <> ",",
      "  activity: " <> toRust @Activity,
      "}",
      "",
      "enum Activity {",
      "  Working { hard: " <> toRust @Bool <> ", hours: " <> toRust @Int <> " },",
      "  Studying { hours: " <> toRust @Int <> ", subject: " <> toRust @Text <> " },",
      "  Sleeping  { dream: " <> toRust @Text <> " }"
    ]

rustModuleGenerated :: Text
rustModuleGenerated =
  unlines
    [ "struct Person {",
      "  name: String,",
      "  age: i32,",
      "  is_student: bool,",
      "  friends: Vec<String>,",
      "  activity: Activity",
      "}",
      "",
      "enum Activity {",
      "  Working { hard: bool, hours: i32 },",
      "  Studying { hours: i32, subject: String },",
      "  Sleeping { dream: String }",
      "}"
    ]
```

<!-- --- -->

```haskell
newtype RustCode = RustCode TextLang
  deriving (Show, Eq)
  deriving newtype (IsString, Semigroup, IsLang)

instance FnC.ToRef RustCode Text where
  toRef = "String"

instance FnC.ToRef RustCode Int where
  toRef = "i32"

instance FnC.ToRef RustCode Bool where
  toRef = "bool"

instance (FnC.ToRef RustCode a) => FnC.ToRef RustCode [a] where
  toRef = "Vec<" <> FnC.toRef @RustCode @a <> ">"

-- instance FnC.ToRef RustCode Activity where
--   toRef = FnC.toRef @_ @Activity

instance FnC.ToRef RustCode Person where
  toRef = FnC.toRef @_ @Person

f :: TypeDef RustCode -> Text
f (FnC.selectCasesMultiFields -> Just x) = ""
f (FnC.selectFields -> Just (QualName {typeName}, fields)) =
  unlines
    [ "struct " <> typeName <> " {",
      -- Txt.intercalate ",\n" $ map (\(Field {fieldName, fieldType}) -> fieldName <> ": " <> fieldType) fields,
      "}"
    ]
f _ = error "unsupported"

-- class ToRef' a lang where
--   toRef' :: lang

-- instance (FnC.ToRef lang a ) =>  ToRef' a lang where
--   toRef' = FnC.toRef @lang @a
```
