module Readme1 where

import Data.String.Conversions (cs)
import qualified Data.Text as Txt
import Relude

{-
# fields-and-cases

Generating other languages' types from Haskell types is a straightforward task.
We a custom type class we can do this as follows:
-}

data Activity
  = Working {hard :: Bool, hours :: Int}
  | Studying {hours :: Int, subject :: Text}
  | Sleeping {dream :: Text}
  deriving (Show, Eq, Generic)

data Person = Person
  { name :: Text,
    age :: Int,
    isStudent :: Bool,
    friends :: [Text],
    activity :: Activity
  }
  deriving (Show, Eq)

{- --- -}

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

{- --- -}

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
