module FieldsAndCases where

import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Relude

data TypeDef lang = TypeDef
  { typeName :: QualName,
    cases :: Cases lang
  }
  deriving (Show, Eq)

newtype Cases lang = Cases [Case lang]
  deriving (Monoid, Semigroup, Show, Eq)

data Case lang
  = CaseWithFields
      { tagName :: Text,
        fields :: Fields lang
      }
  | CaseNoFields
      { tagName :: Text
      }
  | CaseWithPositionalFields
      { tagName :: Text,
        positionalFields :: PositionalFields lang
      }
  deriving (Show, Eq)

newtype Fields lang = Fields
  { fields :: [Field lang]
  }
  deriving (Monoid, Semigroup, Show, Eq)

newtype PositionalFields lang = PositionalFields
  { fields :: [PositionalField lang]
  }
  deriving (Monoid, Semigroup, Show, Eq)

data Field lang = Field
  { fieldName :: Text,
    fieldType :: lang
  }
  deriving (Show, Eq)

newtype PositionalField lang = PositionalField
  { fieldType :: lang
  }
  deriving (Show, Eq)

data QualName = QualName
  { moduleName :: Text,
    typeName :: Text
  }
  deriving (Show, Eq)

--- Utils ---

-- Function to get the Rep of a type without a value
getRep :: forall rep a x. (Generic a, Rep a ~ rep) => Proxy a -> rep x
getRep _ = from (error "no value" :: a)

---

type TextLang = LinearLang Text

newtype LinearLang lang = LinearLang [LinearLangItem lang]
  deriving (Show, Eq)

data LinearLangItem lang
  = Ref QualName
  | Code lang
  deriving (Show, Eq)

instance (IsString lang) => IsString (LinearLang lang) where
  fromString s = LinearLang [Code $ fromString s]

instance Semigroup (LinearLang lang) where
  (LinearLang a) <> (LinearLang b) = LinearLang $ a <> b

instance IsLang (LinearLang lang) where
  typeRef qualName = LinearLang [Ref qualName]

---

class IsLang lang where
  typeRef :: QualName -> lang

--- ToRef ---

class ToRef lang a where
  toRef :: lang
  default toRef ::
    (Generic a, GToRef (Rep a) lang) => lang
  toRef =
    gToRef $ getRep (Proxy :: Proxy a)

class GToRef rep lang where
  gToRef :: rep a -> lang

-- Match Data Type
instance
  (KnownSymbol typeName, KnownSymbol moduleName, IsLang lang) =>
  GToRef
    (M1 {- MetaInfo -} D {- DataType -} ('MetaData typeName moduleName packageName isNewtype) cases)
    lang
  where
  gToRef _ = result
    where
      moduleName :: Text
      moduleName = fromString $ symbolVal (Proxy @moduleName)

      typeName :: Text
      typeName = fromString $ symbolVal (Proxy @typeName)

      qualName :: QualName
      qualName = QualName {moduleName, typeName}

      result :: lang
      result = typeRef qualName

--- ToDef ---

class ToDef lang a where
  toDef :: TypeDef lang
  default toDef ::
    (Generic a, GToDef (Rep a) TypeDef lang) => TypeDef lang
  toDef =
    gToDef $ getRep (Proxy :: Proxy a)

class GToDef rep def lang where
  gToDef :: rep a -> def lang

-- Match Data Type
instance
  (GToDef cases Cases lang, KnownSymbol typeName, KnownSymbol moduleName) =>
  GToDef
    (M1 {- MetaInfo -} D {- DataType -} ('MetaData typeName moduleName packageName isNewtype) cases)
    TypeDef
    lang
  where
  gToDef _ = result
    where
      cases :: Cases lang
      cases = gToDef (error "no value" :: cases x)

      moduleName :: Text
      moduleName = fromString $ symbolVal (Proxy @moduleName)

      typeName :: Text
      typeName = fromString $ symbolVal (Proxy @typeName)

      qualName :: QualName
      qualName = QualName {moduleName, typeName}

      result :: TypeDef lang
      result = TypeDef qualName cases

-- Match Sum
instance
  (GToDef lhs Cases lang, GToDef rhs Cases lang) =>
  GToDef
    (lhs :+: rhs)
    Cases
    lang
  where
  gToDef _ = result
    where
      lhs :: Cases lang
      lhs = gToDef (error "no value" :: lhs x)

      rhs :: Cases lang
      rhs = gToDef (error "no value" :: rhs x)

      result :: Cases lang
      result = lhs <> rhs

-- Match Constructor with fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName, GToDef fields Fields lang) =>
  GToDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'True) fields)
    Cases
    lang
  where
  gToDef _ = result
    where
      fields :: Fields lang
      fields = gToDef (error "no value" :: fields x)

      ctorName :: Text
      ctorName = fromString $ symbolVal (Proxy @ctorName)

      case_ :: Case lang
      case_ = CaseWithFields ctorName fields

      result :: Cases lang
      result = Cases [case_]

-- Match Constructor with positional fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName, GToDef fields PositionalFields lang) =>
  GToDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'False {- hasSelectors -}) fields)
    Cases
    lang
  where
  gToDef _ = result
    where
      ctorName :: Text
      ctorName = fromString $ symbolVal (Proxy @ctorName)

      fields :: PositionalFields lang
      fields = gToDef (error "no value" :: fields x)

      case_ :: Case lang
      case_ = CaseWithPositionalFields ctorName fields

      result :: Cases lang
      result = Cases [case_]

-- Match Constructor without fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName) =>
  GToDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'False {- hasSelectors -}) U1 {- Unit -})
    Cases
    lang
  where
  gToDef _ = result
    where
      ctorName :: Text
      ctorName = fromString $ symbolVal (Proxy @ctorName)

      case_ :: Case lang
      case_ = CaseNoFields ctorName

      result :: Cases lang
      result = Cases [case_]

-- Match Product
instance
  (GToDef lhs fields lang, GToDef rhs fields lang, Semigroup (fields lang)) =>
  GToDef
    (lhs :*: rhs)
    fields
    lang
  where
  gToDef _ = result
    where
      lhs :: fields lang
      lhs = gToDef (error "no value" :: lhs x)

      rhs :: fields lang
      rhs = gToDef (error "no value" :: rhs x)

      result :: fields lang
      result = lhs <> rhs

-- Match Field
instance
  {-# OVERLAPPABLE #-}
  (ToRef lang a, KnownSymbol fieldName) =>
  GToDef
    (M1 {- MetaInfo -} S {- Selector -} ('MetaSel ('Just fieldName) srcUnpackedness srcStrictness inferedStrictness) (K1 R a))
    Fields
    lang
  where
  gToDef _ = result
    where
      fieldName :: Text
      fieldName = fromString $ symbolVal (Proxy @fieldName)

      fieldType :: lang
      fieldType = toRef @lang @a

      field :: Field lang
      field = Field {fieldName, fieldType}

      result :: Fields lang
      result = Fields [field]

-- Match Positional Field
instance
  {-# OVERLAPPABLE #-}
  (ToRef lang a) =>
  GToDef
    (M1 {- MetaInfo -} S {- Selector -} ('MetaSel 'Nothing srcUnpackedness srcStrictness inferedStrictness) (K1 R a))
    PositionalFields
    lang
  where
  gToDef _ = result
    where
      fieldType :: lang
      fieldType = toRef @lang @a

      field :: PositionalField lang
      field = PositionalField {fieldType}

      result :: PositionalFields lang
      result = PositionalFields [field]
