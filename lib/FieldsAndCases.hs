module FieldsAndCases where

import Data.String.Conversions (cs)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Relude

data TypeDef lang = TypeDef
  { typeName :: QualName,
    cases :: [Case lang]
  }
  deriving (Show, Eq)

data Case lang = Case
  { tagName :: Text,
    caseFields :: Maybe (CaseFields lang)
  }
  deriving (Show, Eq)

data CaseFields lang
  = CasePositionalFields [PositionalField lang]
  | CaseLabeledFields [LabeledField lang]
  deriving (Show, Eq)

data LabeledField lang = LabeledField
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

---

class IsLang lang where
  typeRef :: QualName -> lang

instance IsLang Text where
  typeRef (QualName {typeName}) = fromString $ cs typeName

--- ToRef ---

class ToRef lang a where
  toRef :: lang
  default toRef ::
    (IsLang lang, Generic a, GToRef (Rep a)) => lang
  toRef =
    typeRef $ gToRef $ getRep (Proxy :: Proxy a)

class GToRef rep where
  gToRef :: rep a -> QualName

-- Match Data Type
instance
  (KnownSymbol typeName, KnownSymbol moduleName) =>
  GToRef
    (M1 {- MetaInfo -} D {- DataType -} ('MetaData typeName moduleName packageName isNewtype) cases)
  where
  gToRef _ = result
    where
      moduleName :: Text
      moduleName = fromString $ symbolVal (Proxy @moduleName)

      typeName :: Text
      typeName = fromString $ symbolVal (Proxy @typeName)

      result :: QualName
      result = QualName {moduleName, typeName}

class Ref a lang where
  ref :: lang

instance (ToRef lang a) => Ref a lang where
  ref = toRef @lang @a

--- ToDef ---

class ToDef a lang where
  toDef :: TypeDef lang

instance (Generic a, GToDef (Rep a) TypeDef lang) => ToDef a lang where
  toDef = gToDef $ getRep (Proxy :: Proxy a)

class GToDef rep def lang where
  gToDef :: rep a -> def lang

-- Match Data Type
instance
  (GToDef cases (Compose [] Case) lang, KnownSymbol typeName, KnownSymbol moduleName) =>
  GToDef
    (M1 {- MetaInfo -} D {- DataType -} ('MetaData typeName moduleName packageName isNewtype) cases)
    TypeDef
    lang
  where
  gToDef _ = result
    where
      cases :: Compose [] Case lang
      cases = gToDef (error "no value" :: cases x)

      moduleName :: Text
      moduleName = fromString $ symbolVal (Proxy @moduleName)

      typeName :: Text
      typeName = fromString $ symbolVal (Proxy @typeName)

      qualName :: QualName
      qualName = QualName {moduleName, typeName}

      result :: TypeDef lang
      result = TypeDef qualName (coerce cases)

-- Match Sum
instance
  (GToDef lhs (Compose [] Case) lang, GToDef rhs (Compose [] Case) lang) =>
  GToDef
    (lhs :+: rhs)
    (Compose [] Case)
    lang
  where
  gToDef _ = result
    where
      lhs :: Compose [] Case lang
      lhs = gToDef (error "no value" :: lhs x)

      rhs :: Compose [] Case lang
      rhs = gToDef (error "no value" :: rhs x)

      result :: Compose [] Case lang
      result = lhs <> rhs

-- Match Constructor with fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName, GToDef fields (Compose [] LabeledField) lang) =>
  GToDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'True) fields)
    (Compose [] Case)
    lang
  where
  gToDef _ = result
    where
      fields :: Compose [] LabeledField lang
      fields = gToDef (error "no value" :: fields x)

      tagName :: Text
      tagName = fromString $ symbolVal (Proxy @ctorName)

      case_ :: Case lang
      case_ =
        Case
          { tagName,
            caseFields = Just $ CaseLabeledFields (coerce fields)
          }

      result :: Compose [] Case lang
      result = coerce [case_]

-- Match Constructor with positional fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName, GToDef fields (Compose [] PositionalField) lang) =>
  GToDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'False {- hasSelectors -}) fields)
    (Compose [] Case)
    lang
  where
  gToDef _ = result
    where
      tagName :: Text
      tagName = fromString $ symbolVal (Proxy @ctorName)

      fields :: Compose [] PositionalField lang
      fields = gToDef (error "no value" :: fields x)

      case_ :: Case lang
      case_ =
        Case
          { tagName,
            caseFields = Just $ CasePositionalFields (coerce fields)
          }

      result :: Compose [] Case lang
      result = coerce [case_]

-- Match Constructor without fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName) =>
  GToDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'False {- hasSelectors -}) U1 {- Unit -})
    (Compose [] Case)
    lang
  where
  gToDef _ = result
    where
      tagName :: Text
      tagName = fromString $ symbolVal (Proxy @ctorName)

      case_ :: Case lang
      case_ = Case {tagName, caseFields = Nothing}

      result :: Compose [] Case lang
      result = coerce [case_]

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
    (Compose [] LabeledField)
    lang
  where
  gToDef _ = result
    where
      fieldName :: Text
      fieldName = fromString $ symbolVal (Proxy @fieldName)

      fieldType :: lang
      fieldType = toRef @lang @a

      field :: LabeledField lang
      field = LabeledField {fieldName, fieldType}

      result :: Compose [] LabeledField lang
      result = coerce [field]

-- Match Positional Field
instance
  {-# OVERLAPPABLE #-}
  (ToRef lang a) =>
  GToDef
    (M1 {- MetaInfo -} S {- Selector -} ('MetaSel 'Nothing srcUnpackedness srcStrictness inferedStrictness) (K1 R a))
    (Compose [] PositionalField)
    lang
  where
  gToDef _ = result
    where
      fieldType :: lang
      fieldType = toRef @lang @a

      field :: PositionalField lang
      field = PositionalField {fieldType}

      result :: Compose [] PositionalField lang
      result = coerce [field]

--- Utils ---

-- Function to get the Rep of a type without a value
getRep :: forall rep a x. (Generic a, Rep a ~ rep) => Proxy a -> rep x
getRep _ = from (error "no value" :: a)
