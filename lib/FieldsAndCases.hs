{- | 

Code generate type definitions in any language based on Haskell types.

-}
module FieldsAndCases
  ( matchRecordLikeDataType,
    isEnumWithoutData,
    ToTypeDefs (..),
    ToTypeDef (..),
    IsTypeExpr (..),
    TypeExpr (..),
    TypeDef (..),
    Case (..),
    CaseArgs (..),
    Field (..),
    PositionalArg (..),
    QualifiedName (..),
    GToTypeDef(..),
    GToTypeRef(..)
  )
where

import Data.String.Conversions (cs)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Relude

-- | A
data TypeDef texpr = TypeDef
  { qualifiedName :: QualifiedName,
    cases :: [Case texpr]
  }
  deriving (Show, Eq)

-- | A
data Case texpr = Case
  { tagName :: Text,
    caseArgs :: Maybe (CaseArgs texpr)
  }
  deriving (Show, Eq)

-- | A
data CaseArgs texpr
  = CasePositionalArgs [PositionalArg texpr]
  | CaseFields [Field texpr]
  deriving (Show, Eq)

-- | A
data Field texpr = Field
  { fieldName :: Text,
    fieldType :: texpr
  }
  deriving (Show, Eq)

-- | A
newtype PositionalArg texpr = PositionalArg
  { fieldType :: texpr
  }
  deriving (Show, Eq)

-- | A
data QualifiedName = QualifiedName
  { moduleName :: Text,
    typeName :: Text
  }
  deriving (Show, Eq)

-- | Data types with a single constructor and labeled fields
-- | are considered record-like.
matchRecordLikeDataType :: TypeDef texpr -> Maybe (Text, [Field texpr])
matchRecordLikeDataType (TypeDef {qualifiedName = QualifiedName {typeName}, cases}) =
  case cases of
    [Case {tagName, caseArgs = Just (CaseFields fields)}]
      | typeName == tagName -> Just (typeName, fields)
    _ -> Nothing


-- | A
isEnumWithoutData :: TypeDef texpr -> Bool
isEnumWithoutData (TypeDef {qualifiedName = QualifiedName {typeName}, cases}) =
  all isEnumCase cases
  where
    isEnumCase (Case {caseArgs = Nothing}) = True
    isEnumCase _ = False

---

-- | A
class ToTypeDefs (xs :: [Type]) texpr where
  toTypeDefs :: [TypeDef texpr]

instance ToTypeDefs '[] texpr where
  toTypeDefs = []

instance (ToTypeDef x texpr, ToTypeDefs xs texpr) => ToTypeDefs (x ': xs) texpr where
  toTypeDefs = toTypeDef @x @texpr : toTypeDefs @xs @texpr

---

-- | A
class IsTypeExpr texpr where
  typeRef :: QualifiedName -> texpr

instance IsTypeExpr Text where
  typeRef (QualifiedName {typeName}) = fromString $ cs typeName

--- ToTypeRef ---

-- | A
class TypeExpr a texpr where
  typeExpr :: texpr
  default typeExpr ::
    (IsTypeExpr texpr, Generic a, GToTypeRef (Rep a)) => texpr
  typeExpr =
    typeRef $ gToTypeRef $ getRep (Proxy :: Proxy a)

-- | A
class GToTypeRef rep where
  gToTypeRef :: rep a -> QualifiedName

-- Match Data Type
instance
  (KnownSymbol typeName, KnownSymbol moduleName) =>
  GToTypeRef
    (M1 {- MetaInfo -} D {- DataType -} ('MetaData typeName moduleName packageName isNewtype) cases)
  where
  gToTypeRef _ = result
    where
      moduleName :: Text
      moduleName = fromString $ symbolVal (Proxy @moduleName)

      typeName :: Text
      typeName = fromString $ symbolVal (Proxy @typeName)

      result :: QualifiedName
      result = QualifiedName {moduleName, typeName}

--- ToTypeDef ---

-- | A
class ToTypeDef a texpr where
  toTypeDef :: TypeDef texpr

instance (Generic a, GToTypeDef (Rep a) (TypeDef texpr)) => ToTypeDef a texpr where
  toTypeDef = gToTypeDef $ getRep (Proxy :: Proxy a)

-- | A
class GToTypeDef rep def where
  gToTypeDef :: rep a -> def

-- Match Data Type
instance
  (GToTypeDef cases [Case texpr], KnownSymbol typeName, KnownSymbol moduleName) =>
  GToTypeDef
    (M1 {- MetaInfo -} D {- DataType -} ('MetaData typeName moduleName packageName isNewtype) cases)
    (TypeDef texpr)
  where
  gToTypeDef _ = result
    where
      cases :: [Case texpr]
      cases = gToTypeDef (error "no value" :: cases x)

      moduleName :: Text
      moduleName = fromString $ symbolVal (Proxy @moduleName)

      typeName :: Text
      typeName = fromString $ symbolVal (Proxy @typeName)

      qualifiedName :: QualifiedName
      qualifiedName = QualifiedName {moduleName, typeName}

      result :: TypeDef texpr
      result = TypeDef qualifiedName (coerce cases)

-- Match Sum
instance
  (GToTypeDef lhs [Case texpr], GToTypeDef rhs [Case texpr]) =>
  GToTypeDef
    (lhs :+: rhs)
    [Case texpr]
  where
  gToTypeDef _ = result
    where
      lhs :: [Case texpr]
      lhs = gToTypeDef (error "no value" :: lhs x)

      rhs :: [Case texpr]
      rhs = gToTypeDef (error "no value" :: rhs x)

      result :: [Case texpr]
      result = lhs <> rhs

-- Match Constructor with fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName, GToTypeDef fields [Field texpr]) =>
  GToTypeDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'True {- hasSelectors -}) fields)
    [Case texpr]
  where
  gToTypeDef _ = result
    where
      fields :: [Field texpr]
      fields = gToTypeDef (error "no value" :: fields x)

      tagName :: Text
      tagName = fromString $ symbolVal (Proxy @ctorName)

      case_ :: Case texpr
      case_ =
        Case
          { tagName,
            caseArgs = Just $ CaseFields (coerce fields)
          }

      result :: [Case texpr]
      result = coerce [case_]

-- Match Constructor with positional fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName, GToTypeDef fields [PositionalArg texpr]) =>
  GToTypeDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'False {- hasSelectors -}) fields)
    [Case texpr]
  where
  gToTypeDef _ = result
    where
      tagName :: Text
      tagName = fromString $ symbolVal (Proxy @ctorName)

      fields :: [PositionalArg texpr]
      fields = gToTypeDef (error "no value" :: fields x)

      case_ :: Case texpr
      case_ =
        Case
          { tagName,
            caseArgs = Just $ CasePositionalArgs (coerce fields)
          }

      result :: [Case texpr]
      result = coerce [case_]

-- Match Constructor without fields
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol ctorName) =>
  GToTypeDef
    (M1 {- MetaInfo -} C {- Constructor -} ('MetaCons ctorName fixity 'False {- hasSelectors -}) U1 {- Unit -})
    [Case texpr]
  where
  gToTypeDef _ = result
    where
      tagName :: Text
      tagName = fromString $ symbolVal (Proxy @ctorName)

      case_ :: Case texpr
      case_ = Case {tagName, caseArgs = Nothing}

      result :: [Case texpr]
      result = coerce [case_]

-- Match Product
instance
  (GToTypeDef lhs fields, GToTypeDef rhs fields, Semigroup fields) =>
  GToTypeDef
    (lhs :*: rhs)
    fields
  where
  gToTypeDef _ = result
    where
      lhs :: fields
      lhs = gToTypeDef (error "no value" :: lhs x)

      rhs :: fields
      rhs = gToTypeDef (error "no value" :: rhs x)

      result :: fields
      result = lhs <> rhs

-- Match Field
instance
  {-# OVERLAPPABLE #-}
  (TypeExpr a texpr, KnownSymbol fieldName) =>
  GToTypeDef
    (M1 {- MetaInfo -} S {- Selector -} ('MetaSel ('Just fieldName) srcUnpackedness srcStrictness inferedStrictness) (K1 R a))
    [Field texpr]
  where
  gToTypeDef _ = result
    where
      fieldName :: Text
      fieldName = fromString $ symbolVal (Proxy @fieldName)

      fieldType :: texpr
      fieldType = typeExpr @a @texpr

      field :: Field texpr
      field = Field {fieldName, fieldType}

      result :: [Field texpr]
      result = coerce [field]

-- Match Positional Field
instance
  {-# OVERLAPPABLE #-}
  (TypeExpr a texpr) =>
  GToTypeDef
    (M1 {- MetaInfo -} S {- Selector -} ('MetaSel 'Nothing srcUnpackedness srcStrictness inferedStrictness) (K1 R a))
    [PositionalArg texpr]
  where
  gToTypeDef _ = result
    where
      fieldType :: texpr
      fieldType = typeExpr @a @texpr

      field :: PositionalArg texpr
      field = PositionalArg {fieldType}

      result :: [PositionalArg texpr]
      result = coerce [field]

--- Utils ---

-- Function to get the Rep of a type without a value
getRep :: forall rep a x. (Generic a, Rep a ~ rep) => Proxy a -> rep x
getRep _ = from (error "no value" :: a)
