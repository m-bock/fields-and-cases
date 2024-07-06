module Spec where

import Data.String.Conversions (cs)
import Data.Text (replace)
import FieldsAndCases (Case (..), IsLang (..), PositionalField (..), PositionalFields (..), QualName (..), Ref (..), ToDef, ToRef (toRef), TypeDef (..), toRef, LabeledField)
import qualified FieldsAndCases as FnC
import GHC.Generics
import qualified GHC.Generics as GHC
import Lima.Converter (Format (..), convertTo, def)
import Relude
import Test.Tasty
import Test.Tasty.HUnit
import FieldsAndCases (LabeledField(..))
import FieldsAndCases (CaseFields(..))

newtype Code = Code Text
  deriving (Show, Eq)
  deriving newtype (IsString, Semigroup, IsLang, ToText)

instance ToRef Code Int where
  toRef = "Int"

instance ToRef Code Float where
  toRef = "Float"

instance ToRef Code Bool where
  toRef = "Bool"

instance ToRef Code Text where
  toRef = "Text"

instance (ToRef Code a) => ToRef Code [a] where
  toRef = "Vec<" <> ref @a <> ">"

data SampleType2 = SampleType2 Int Bool
  deriving (Eq, Show, Generic, ToRef lang)

data SampleType
  = Case1 {fieldA :: Int, fieldB :: SampleType2, fieldC :: [Float]}
  | Case2 Int Text Float
  | Case3
  deriving (Eq, Show, Generic, ToRef lang)

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
        $ ( FnC.toDef @SampleType @Code
              @?= TypeDef
                { typeName =
                    QualName
                      { moduleName = "Spec",
                        typeName = "SampleType"
                      },
                  cases =
                    [ Case
                        { tagName = "Case1",
                          caseFields =
                            Just
                              ( CaseLabeledFields
                                  [ LabeledField {fieldName = "fieldA", fieldType = Code "Int"},
                                    LabeledField {fieldName = "fieldB", fieldType = Code "SampleType2"},
                                    LabeledField {fieldName = "fieldC", fieldType = Code "Vec<Float>"}
                                  ]
                              )
                        },
                      Case
                        { tagName = "Case2",
                          caseFields =
                            Just
                              ( CasePositionalFields
                                  [ PositionalField {fieldType = Code "Int"},
                                    PositionalField {fieldType = Code "Text"},
                                    PositionalField {fieldType = Code "Float"}
                                  ]
                              )
                        },
                      Case {tagName = "Case3", caseFields = Nothing}
                    ]
                }
          )
    ]