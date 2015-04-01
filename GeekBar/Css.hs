{-# LANGUAGE TemplateHaskell #-}
module GeekBar.Css ( Selector
                   -- Combinators
                   , (!.), (!#), (!^), (!>), (!|), (!~), (!+)
                   -- Parser
                   , selectorP
                   ) where

import GeekBar.Node
import GeekBar.Props

import Control.Lens
import Control.Applicative ((<|>))
import Control.Monad       ((<=<))
import Data.Monoid         (Monoid(..), (<>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Set        as S
import qualified Data.Text       as T


-- | Css selector
newtype Selector = Selector {_select :: NodeZ -> Maybe NodeZ}
makeLenses ''Selector

instance Monoid Selector where
    mempty        = Selector Just
    a `mappend` b = Selector $ a ^. select <=< b ^. select


--------------------------------------------------------------------------------
-- Selector combinators
--
-- | Predicate selector
cond :: (NodeZ -> Bool) -> Selector
cond p = Selector $ \z -> if p z then Just z else Nothing

-- | Class selector = ".class"
(!.) :: Selector -> T.Text -> Selector
a !. k = a <> cond (S.member k . (^. zfocus.props.classes))

-- | Uid selector = "#uid"
(!#) :: Selector -> T.Text -> Selector
a !# u = a <> cond ((u ==) . (^. zfocus.props.uid))

-- | Compose with ansestor selector = "selector selector"
(!^) :: Selector -> Selector -> Selector
a !^ b = parent a <> b
    where parent s = Selector $ \z -> fmap (s ^. select) (zups z) ^? traverse . _Just

-- | Compose with dirct parent selector = "selector > selector"
(!>) :: Selector -> Selector -> Selector
a !> b = Selector ((a ^. select =<<) . zup) <> b

-- | Compose with alternative selector = "selector, selector"
(!|) :: Selector -> Selector -> Selector
a !| b = Selector $ \z -> (b ^. select) z <|> (a ^. select) z

-- | Compose with left sibling selctor = "selector ~ selector"
(!~) :: Selector -> Selector -> Selector
a !~ b = sibs a <> b
    where sibs s = Selector $ \z -> fmap (s ^. select) (zrights z) ^? traverse . _Just

-- | Conpose with immedieate left sibling selector = "selector + selector"
(!+) :: Selector -> Selector -> Selector
a !+ b = Selector ((a ^. select =<<) . zleft) <> b

-- | First child selector
firstChild :: Selector -> Selector
firstChild s = s <> Selector (\z -> maybe (Just z) (const Nothing) (zleft z))

-- | Last child selector
lastChild :: Selector -> Selector
lastChild s = s <> Selector (\z -> maybe (Just z) (const Nothing) (zright z))

--------------------------------------------------------------------------------
-- Selector parser
--
selectorP :: A.Parser Selector
selectorP = undefined

-- | Compose with
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
