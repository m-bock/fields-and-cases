```haskell
module Readme where

import Relude
```

# fields-and-cases

Generating other languages' types from Haskell types is a straightforward task.
We a custom type class we can do this as follows:

```haskell
class ToRustDef a where
  toRustDef :: Text

class ToRustRef a where
  toRustRef :: Text

---

instance ToRustRef Text where
  toRustRef = "String"

instance ToRustRef Int where
  toRustRef = "i32"

instance ToRustRef Bool where
  toRustRef = "bool"

instance (ToRustRef a) => ToRustRef [a] where
  toRustRef = "Vec<" <> toRustRef @a <> ">"

data Activity
  = Working {hard :: Bool, hours :: Int}
  | Studying {hours :: Int, subject :: Text}
  | Sleeping {dream :: Text}
  deriving (Show, Eq)

instance ToRustRef Activity where
  toRustRef = "Activity"

data Person = Person
  { name :: Text,
    age :: Int,
    isStudent :: Bool,
    friends :: [Text],
    activity :: Activity
  }
  deriving (Show, Eq)

instance ToRustDef Person where
  toRustDef =
    unlines
      [ "#[derive(Debug, PartialEq)]",
        "struct Person {",
        "  name: " <> toRustRef @Text <> ",",
        "  age: " <> toRustRef @Int <> ",",
        "  is_student: " <> toRustRef @Bool <> ",",
        "  friends: " <> toRustRef @[Text] <> ",",
        "  activity: " <> toRustRef @Activity,
        "}"
      ]

instance ToRustDef Activity where
  toRustDef =
    unlines
      [ "#[derive(Debug, PartialEq)]",
        "enum Activity {",
        "  Working {",
        "    hard: " <> toRustRef @Bool <> ",",
        "    hours: " <> toRustRef @Int,
        "  },",
        "  Studying {",
        "    hours: " <> toRustRef @Int <> ",",
        "    subject: " <> toRustRef @Text,
        "  },",
        "  Sleeping {",
        "    dream: " <> toRustRef @Text <> " }",
        "}"
      ]

rustModule :: Text
rustModule =
  unlines
    [ toRustDef @Activity,
      toRustDef @Person
    ]
```
