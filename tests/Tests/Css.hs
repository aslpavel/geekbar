module Tests.Css (tests) where

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck
import GeekBar.Css

import Data.Foldable (foldlM)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T


-- | Aribtrary type class instance for Selector
-- TODO: Add attrribute support
--
instance Arbitrary Selector where
    arbitrary = do selectors <- resize 20 (listOf1 primitive)
                   case selectors of
                     [s]    -> return s
                     s : ss -> foldlM (\l r -> (\op -> l `op` r) <$> combinator) s ss
                     []     -> error "Selectors list must not be empty"
        where
          -- identifiers
          identChars = '-' : '_' : ['a'..'z'] ++ ['A'..'Z']
          nameChars  = identChars ++ ['0'..'9']
          ident = (\h f r -> T.pack (h ++ f ++ r))
                  <$> frequency [(1, return "-"), (10, return "")]
                  <*> fmap (:[]) (elements . tail $ identChars)
                  <*> (resize 6 . listOf . elements $ nameChars)
          name  = T.pack <$> (resize 6 . listOf1 . elements $ nameChars)
          -- selectors
          universal = return universalS
          tupe      = typeS  <$> ident
          klass     = classS <$> ident
          hash      = uidS   <$> name
          pseudo    = oneof [ return hoverS
                            , return firstChildS
                            , return lastChildS
                            , nthChildS <$> arbitrary
                            , nthLastChildS <$> arbitrary
                            ]
          negation  = notS <$> oneof [tupe, universal, hash, klass, pseudo]
          primitive = do
                s  <- oneof [tupe, universal, hash, klass, pseudo, negation]
                ss <- listOf . oneof $ [hash, klass, pseudo, negation]
                return . mconcat $ s : ss
          combinator = elements [ mappend . childS
                                , mappend . directChildS
                                , mappend . adjacentSiblingS
                                , mappend . generalSiblingS
                                , orS
                                ]


-- | Selector representation must be parseble and be equal to parseds selector representation
prop_Parsable :: Selector -> Property
prop_Parsable s = case A.parseOnly selectorP (T.pack . show $ s) of
                    Right s' -> show s' === show s
                    Left  _  -> label "Parser failed" False


-- | All tests in this suite
tests :: IO [Test]
tests = return [testProperty "show (parse <css>) == show <css>" prop_Parsable]
