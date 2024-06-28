{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FieldsAndCases where

import GHC.Generics
import Relude

data Def lang
  = Fields Text [(Text, lang)]
  | Cases Text [(Text, [lang])]
  deriving (Show, Eq)

--- Utils ---

-- Function to get the Rep of a type without a value
getRep :: forall rep a x. (Generic a, Rep a ~ rep) => Proxy a -> rep x
getRep _ = from (error "No value needed" :: a)

--- ToRef ---

class ToRef a lang where
  toRef :: Proxy a -> lang
  default toRef :: (Generic a, GToRef (Rep a) lang) => Proxy a -> lang
  toRef = gToRef . getRep

class GToRef rep lang where
  gToRef :: rep (Proxy a) -> lang


--- ToDef ---

class ToDef a lang where
  toDef :: Proxy a -> Def lang
  default toDef :: (Generic a, GToDef (Rep a) lang) => Proxy a -> Def lang
  toDef = gToDef . getRep

class GToDef rep lang where
  gToDef :: rep a -> Def lang

-- Match a "Fields" definition:
-- A datatype (not Newtype) where type name and constructor names are the same
instance
  GToDef
    ( M1 -- MetaInfo
        D -- DataType
        ('MetaData "Person" "Main" "main" 'False)
        ( M1 -- MetaInfo
            C -- Constructor
            ('MetaCons "Person" 'PrefixI 'True)
            fields
        )
    )
    b
  where
  gToDef _ = Fields "Person" []

-- Match a "Cases" definition:
-- A datatype (not Newtype) where type name and constructor names are different

-- Match as "Sum" (multiple cases)

-- Match as "Product" (multiple fields)

-- Match a Case (constructor) definition:

-- Match a Field definition: