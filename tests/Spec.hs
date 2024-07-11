{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}




module Spec where

import Data.String.Conversions (cs)
import Data.Text (replace)
import qualified FieldsAndCases as FnC
import GHC.Generics
import qualified GHC.Generics as GHC
import Lima.Converter (Format (..), convertTo, def)
import Relude
import Test.Tasty
import Test.Tasty.HUnit
import FieldsAndCases (IsTypeExpr)
import FieldsAndCases (TypeExpr)

newtype Code = Code Text
  deriving (Show, Eq)
  deriving newtype (IsString, Semigroup, IsTypeExpr, ToText)

instance TypeExpr Int Code where
  typeExpr = "Int"

instance TypeExpr Float Code where
  typeExpr = "Float"

instance TypeExpr Bool Code where
  typeExpr = "Bool"

instance TypeExpr Text Code where
  typeExpr = "Text"

instance (TypeExpr a Code) => TypeExpr [a] Code where
  typeExpr = "Vec<" <> FnC.typeExpr @a <> ">"

data SampleType2 = SampleType2 Int Bool
  deriving (Eq, Show, Generic)

instance TypeExpr SampleType2 Code

data SampleType
  = Case1 {fieldA :: Int, fieldB :: SampleType2, fieldC :: [Float]}
  | Case2 Int Text Float
  | Case3
  deriving (Eq, Show, Generic)

instance TypeExpr SampleType Code

-- ghci> :kind! Rep SampleType
-- Rep SampleType :: * -> *
type Re =
  M1
    D
    ('MetaData "SampleType" "Spec" "main" 'False)
    ( M1
        C
        ('MetaCons "Case1" 'PrefixI 'True)
        ( M1
            S
            ( 'MetaSel
                ('Just "fieldA")
                'NoSourceUnpackedness
                'NoSourceStrictness
                'DecidedLazy
            )
            (K1 R Int)
            :*: ( M1
                    S
                    ( 'MetaSel
                        ('Just "fieldB")
                        'NoSourceUnpackedness
                        'NoSourceStrictness
                        'DecidedLazy
                    )
                    (K1 R SampleType2)
                    :*: M1
                          S
                          ( 'MetaSel
                              ('Just "fieldC")
                              'NoSourceUnpackedness
                              'NoSourceStrictness
                              'DecidedLazy
                          )
                          (K1 R [Float])
                )
        )
        :+: ( M1
                C
                ('MetaCons "Case2" 'PrefixI 'False)
                ( M1
                    S
                    ( 'MetaSel
                        'Nothing
                        'NoSourceUnpackedness
                        'NoSourceStrictness
                        'DecidedLazy
                    )
                    (K1 R Int)
                    :*: ( M1
                            S
                            ( 'MetaSel
                                'Nothing
                                'NoSourceUnpackedness
                                'NoSourceStrictness
                                'DecidedLazy
                            )
                            (K1 R Text)
                            :*: M1
                                  S
                                  ( 'MetaSel
                                      'Nothing
                                      'NoSourceUnpackedness
                                      'NoSourceStrictness
                                      'DecidedLazy
                                  )
                                  (K1 R Float)
                        )
                )
                :+: M1 C ('MetaCons "Case3" 'PrefixI 'False) U1
            )
    )

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "..."
        $ ( FnC.toTypeDef @SampleType @Code
              @?= FnC.TypeDef
                { qualifiedName =
                    FnC.QualifiedName
                      { moduleName = "Spec",
                        typeName = "SampleType"
                      },
                  cases =
                    [ FnC.Case
                        { tagName = "Case1",
                          caseArgs =
                            Just
                              ( FnC.CaseFields
                                  [ FnC.Field {fieldName = "fieldA", fieldType = Code "Int"},
                                    FnC.Field {fieldName = "fieldB", fieldType = Code "SampleType2"},
                                    FnC.Field {fieldName = "fieldC", fieldType = Code "Vec<Float>"}
                                  ]
                              )
                        },
                      FnC.Case
                        { tagName = "Case2",
                          caseArgs =
                            Just
                              ( FnC.CasePositionalArgs
                                  [ FnC.PositionalArg {fieldType = Code "Int"},
                                    FnC.PositionalArg {fieldType = Code "Text"},
                                    FnC.PositionalArg {fieldType = Code "Float"}
                                  ]
                              )
                        },
                      FnC.Case {tagName = "Case3", caseArgs = Nothing}
                    ]
                }
          )
    ]