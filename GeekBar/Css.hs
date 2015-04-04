{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module GeekBar.Css ( Selector
                   -- Parser
                   , selectorP
                   ) where

import GeekBar.Node
import GeekBar.Props

import Control.Lens
import Control.Applicative  (Alternative(..), (<|>), many)
import Control.Monad        ((<=<))
import Data.Char            (isSpace)
import Data.Functor         (($>))
import Data.List            (foldl1', foldl')
import Data.Monoid          ((<>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Set        as S
import qualified Data.Text       as T


-- | Css selector
data Selector = Selector { _select :: NodeZ -> Maybe NodeZ
                         , _repr   :: String
                         --, _sepcifity :: (Int, Int)
                         }
makeLenses ''Selector

instance Show Selector where
    show s = case s ^. repr of
               "" -> "s\"*\""
               r  -> "s\"" ++ r ++ "\""

instance Monoid Selector where
    mempty        = Selector Just ""
    a `mappend` b = Selector (a ^. select <=< b ^. select)
                             (a ^. repr <> b ^. repr)


--------------------------------------------------------------------------------
-- Selector combinators
--
-- | Predicate selector
condS :: (NodeZ -> Bool) -> Selector
condS p = Selector (\z -> if p z then Just z else Nothing) "<pred>"

-- | Class selector = ".class"
classS :: T.Text -> Selector
classS k = condS (S.member k . (^. zfocus.props.classes)) & repr .~ ('.' : T.unpack k)

-- | Type selector = "type"
typeS :: T.Text -> Selector
typeS = set repr . T.unpack <*> classS

-- | Uid selector = "#uid"
uidS :: T.Text -> Selector
uidS u = condS ((u ==) . (^. zfocus.props.uid)) & repr .~ ('#' : T.unpack u)

-- | Compose with ansestor selector = "selector selector"
childS :: Selector -> Selector
childS s = Selector (\z -> fmap (s ^. select) (zups z) ^? traverse . _Just)
                    (s ^. repr ++ " ")

-- | Compose with dirct parent selector = "selector > selector"
directChildS :: Selector -> Selector
directChildS s = Selector ((s ^. select =<<) . zup) (s ^. repr ++ " > ")

-- | Compose with alternative selector = "selector, selector"
orS :: Selector -> Selector -> Selector
orS a b = Selector (\z -> (b ^. select) z <|> (a ^. select) z)
                   (a ^. repr ++ ", " ++ b ^. repr)

-- | Compose with left sibling selctor = "selector ~ selector"
generalSiblingS :: Selector -> Selector
generalSiblingS s = Selector (\z -> fmap (s ^. select) (zrights z) ^? traverse . _Just)
                             (s ^. repr ++ " ~ ")

-- | Conpose with immedieate left sibling selector = "selector + selector"
adjacentSiblingS :: Selector -> Selector
adjacentSiblingS s = Selector ((s ^. select =<<) . zleft)
                              (s ^. repr ++ " + ")

-- | First child selector
firstChildS :: Selector
firstChildS = Selector (\z -> maybe (Just z) (const Nothing) (zleft z)) ":first-child"

-- | Last child selector
lastChildS :: Selector
lastChildS = Selector (\z -> maybe (Just z) (const Nothing) (zright z)) ":last-child"

-- | Predicate used in nth- family of selectors
indexPredicate :: Int -> Int -> Int -> Bool
indexPredicate a b i
    | a == 0       = i == b
    | (i-b)*a >= 0 = mod (i-b) a == 0
    | otherwise   = False

-- | Index predicate prepresentatin
indexPredicateRepr :: Int -> Int -> String
indexPredicateRepr a b
    | a == 0     = show b
    | a == 1     = "n"  ++ showSigned b
    | a == -1    = "-n" ++ showSigned b
    | otherwise = show a ++ "n" ++ showSigned b
    where showSigned v
              | v > 0 = "+" ++ show v
              | v < 0 = "-" ++ show (negate v)
              | v == 0 = ""

-- | Nth child
nthChildS :: (Int,Int) -> Selector
nthChildS (a,b) = condS (indexPredicate a b . (+1) . length . zlefts)
                & repr .~ ":nth-child(" ++ indexPredicateRepr a b ++ ")"

-- | Nth last child
nthLastChildS :: (Int,Int) -> Selector
nthLastChildS (a,b) = condS (indexPredicate a b . (+1) . length . zrights)
                    & repr .~ ":nth-last-child(" ++ indexPredicateRepr a b ++ ")"

-- | Hover selector
hoverS :: Selector
hoverS = condS (const False) & repr .~ ":hover"

--------------------------------------------------------------------------------
-- Selector parser
--
selectorP :: A.Parser Selector
selectorP = group
    where
      -- Based on "http://www.w3.org/TR/css3-selectors/#grammar"
      -- name = [_a-zA-Z0-9-]+
      name :: A.Parser T.Text
      name  = A.takeWhile1 (A.inClass "_a-zA-Z0-9-")
      -- identifier = [-]?[_a-zA-Z][_a-zA-Z0-9-]*
      ident :: A.Parser T.Text
      ident = fmap fst $ A.match $ do
                A.skip (== '-') <|> return ()
                A.skip (A.inClass "_a-zA-Z")
                A.skipWhile (A.inClass "_a-zA-Z0-9-")
      -- spaces = [ \t\r\n\f]*
      spaces :: A.Parser ()
      spaces = A.skipWhile isSpace
      -- selector group
      group :: A.Parser Selector
      group = foldl1' orS <$> selector `A.sepBy1` (spaces *> A.char ',' <* spaces)
      -- selector
      selector :: A.Parser Selector
      selector = do s   <- primitive
                    ops <- many $ (,) <$> combinator <*> primitive
                    return $ foldl' (\l (o, r) -> l `o` r) s ops
      -- combinators (child, direct chilod, adjacent sibling, general sibling)
      combinator :: A.Parser (Selector -> Selector -> Selector)
      combinator = do o <- spaces *> A.satisfy (\c -> c == '+' || c == '>' || c == '~') <* spaces
                           <|> (A.space >> spaces $> ' ')
                      return $ mappend . case o of
                                           ' ' -> childS
                                           '>' -> directChildS
                                           '+' -> adjacentSiblingS
                                           '~' -> generalSiblingS
      -- simple selector sequence
      primitive :: A.Parser Selector
      primitive = do s  <- tupe <|> universal <|> chain
                     ss <- many chain
                     return . mconcat $ s : ss
          where chain = hash <|> klass <|> attrib <|> pseudo <|> negation
      -- type selector
      tupe :: A.Parser Selector
      tupe = typeS <$> ident
      -- universal selector
      universal :: A.Parser Selector
      universal = A.char '*' $> mempty
      -- hash selector
      hash :: A.Parser Selector
      hash = uidS <$> (A.char '#' *> name)
      -- class selector
      klass :: A.Parser Selector
      klass = classS <$> (A.char '.' *> ident)
      -- attribute selector
      attrib :: A.Parser Selector
      attrib = empty
      -- pseudo selector
      pseudo :: A.Parser Selector
      pseudo = do A.char ':'
                  sel <- ident <|> (T.cons <$> A.char ':' <*> ident)
                  case sel of
                    "hover"          -> return hoverS
                    "first-chilld"   -> return firstChildS
                    "last-child"     -> return lastChildS
                    "nth-child"      -> nthChildS     <$> nthPred
                    "nth-last-child" -> nthLastChildS <$> nthPred
                    _                -> empty
      nthPred :: A.Parser (Int,Int)
      nthPred = do A.char '(' >> spaces
                   ab <-     A.string "odd"  $> (2,1)
                      <|> A.string "even" $> (2,0)
                      <|> (,) <$> (   A.signed A.decimal
                                  <|> A.char '-' *> return (-1)
                                  <|> return 1
                                  ) <* A.char 'n' <* spaces
                              <*> (   A.char '+' *> spaces *> A.decimal
                                  <|> A.char '-' *> spaces *> fmap negate A.decimal
                                  <|> return 0)
                      <|> (,) 0 <$> A.signed A.decimal
                   spaces >> A.char ')'
                   return ab
      -- negation selector
      negation :: A.Parser Selector
      negation = do A.string ":not(" >> spaces
                    sel <- tupe <|> universal <|> hash <|> klass <|> attrib <|> pseudo
                    spaces >> A.char ')'
                    return sel


--------------------------------------------------------------------------------
-- Selectors
--
-- Supported selectors:
--   *       - any
--   .class  - class matches
--   #id     - id matches
--   a, b    - a or b
--   a b     - a parent of b
--   a > b   - a direct parent of b
--   a ~ b   - a,b are siblings, any b that follows a
--   a + b   - a,b are siblings, b immediatly follows a
--   a:hover - a hovered by mouse
--

{-
-- | "*" selector
-- Matches any node
starSelector :: Selector
starSelector = Selector . const $ Right True

-- | Class selector
-- Matches only specific classs
classSelector :: T.Text -> Selector
classSelector k = Selector $ \n -> Right (k `S.member` n ^. props.classes)


data Rule = Rule { _selector :: Selector
                 , _action   :: Node -> Node
                 }


newtype S a b = S {_sel :: a -> Either (S a b) (Maybe b)}

instance Category S where
    id        = S . const . Right . Just
    S f . S g = S $ \a ->
                case g a of
                  Right Nothing   -> Right Nothing
                  Right (Just a') -> f a'
                  Left  g'        -> _sel (S f . g') a

instance Arrow S where
    arr f       = S . const . Right . Just . f
    first (S f) = S $ \(a, b) ->
                  case f a of
                    Right Nothing   -> Right Nothing
                    Right (Just a') -> Right (Just (a', b))
                    Left f'         -> _sel (first f') (a, b)

instance Functor (S a) where
    f `fmap` a = arr f . a

instance Applicative (S a) where
    pure    = id
    f <*> a = (\(f', a') -> f' a') `fmap` f &&& a
-}
