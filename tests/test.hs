module Main where

import Lima.Converter (Format (..), convertTo, def)
import FieldsAndCases (Case (..), Cases (..), Field (..), Fields (..), PositionalField (..), PositionalFields (..), ToDef, ToRef, TypeDef (..), toRef)
import qualified FieldsAndCases as FnC
import GHC.Generics
import qualified GHC.Generics as GHC
import Relude
import Test.Tasty
import Test.Tasty.HUnit
import FieldsAndCases (QualName(..))
import FieldsAndCases (TextLang)
import FieldsAndCases (IsLang(..))
import Data.String.Conversions (cs)

instance ToRef TextLang Int where
  toRef = "Int"

instance ToRef TextLang Float where
  toRef = "Float"

instance ToRef TextLang Bool where
  toRef = "Bool"

instance ToRef TextLang Text where
  toRef = "Text"

instance (ToRef TextLang a) => ToRef TextLang [a] where
  toRef = "Vec<" <> toRef @_ @a <> ">"

data SampleType2 = SampleType2 Int Bool
  deriving (Eq, Show, Generic, ToRef TextLang, ToDef TextLang)

data SampleType
  = Case1 {fieldA :: Int, fieldB :: SampleType2, fieldC :: [Float]}
  | Case2 Int Text Float
  | Case3
  deriving (Eq, Show, Generic, ToRef TextLang, ToDef TextLang)

type Res =
  M1
    D
    ('MetaData "SampleType" "Test.Main" "main" 'False)
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
                            (K1 R [Char])
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

---

main :: IO ()
main = do
  readmeHs <- readFileBS "tests/Readme.hs"
  let readmeMd = convertTo Hs Md def (cs readmeHs)
  writeFileBS "README.md" (cs readmeMd)
  
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "..."
        $ ( FnC.toDef @TextLang @SampleType
              @?= TypeDef
                { typeName = QualName "Main" "SampleType",
                  cases =
                    Cases
                      [ CaseWithFields
                          { tagName = "Case1",
                            fields =
                              Fields
                                [ Field {fieldName = "fieldA", fieldType = "Int"},
                                  Field {fieldName = "fieldB", fieldType = typeRef (QualName "Main" "SampleType2")},
                                  Field {fieldName = "fieldC", fieldType = "Vec<Float>"}
                                ]
                          },
                        CaseWithPositionalFields
                          { tagName = "Case2",
                            positionalFields =
                              PositionalFields
                                [ PositionalField {fieldType = "Int"},
                                  PositionalField {fieldType = "Text"},
                                  PositionalField {fieldType = "Float"}
                                ]
                          },
                        CaseNoFields
                          { tagName = "Case3"
                          }
                      ]
                }
          )
    ]