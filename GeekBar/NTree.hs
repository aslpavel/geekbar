{-# LANGUAGE TemplateHaskell #-}
module GeekBar.NTree ( NTree(..), value
                     -- Zipper
                     , zipper, NTreeZ, zcontext, zfocus
                     , zdown
                     , ztop, zup, zups
                     , zleft, zlefts
                     , zright, zrights
                     , zrewrite
                     ) where

import Control.Lens
import Control.Applicative (pure)
import Data.Maybe          (fromMaybe)
import Data.Traversable    (sequenceA)

-- | Node tree representation
data NTree a = NLeaf a | NBranch a [NTree a]

-- | Node properties
value :: Lens' (NTree a) a
value f (NLeaf a)      = NLeaf `fmap` f a
value f (NBranch a cs) = flip NBranch cs `fmap` f a

-- | Plated isntance of node, can be used to access children
-- TODO: Maybe switch to Data's uniplate instance?
instance Plated (NTree a) where
    plate _ n@(NLeaf _)    = pure n
    plate f (NBranch a cs) = NBranch a `fmap` sequenceA (fmap f cs)


-- | Node tree zipper
data NTreeZ a = NTreeZ { _zcontext :: [(a, [NTree a], [NTree a])]
                       , _zfocus   :: NTree a
                       }
makeLenses ''NTreeZ

-- | Create zipper from node
zipper :: NTree a -> NTreeZ a
zipper = NTreeZ []

-- | Move zipper to the first chidl
zdown :: NTreeZ a -> Maybe (NTreeZ a)
zdown z = case z ^. zfocus of
             NBranch a (c:cs) -> Just $ z & zcontext %~ ((a, [], cs):)
                                         & zfocus   .~ c
             _ -> Nothing

-- | Move zipper to closest same parent right sibling
zright :: NTreeZ a -> Maybe (NTreeZ a)
zright z = case z ^. zcontext of
              (a, ls, r:rs) : ps -> Just $ z & zcontext .~ (a, z ^. zfocus : ls, rs) : ps
                                            & zfocus   .~ r
              _ -> Nothing

-- | Move zipper to closest same parent left sibling
zleft :: NTreeZ a -> Maybe (NTreeZ a)
zleft z = case z ^. zcontext of
             (a, l:ls, rs) : ps -> Just $ z & zcontext .~ (a, ls, z ^. zfocus : rs) : ps
                                           & zfocus   .~ l
             _ -> Nothing

-- | Move zipper to parent
zup :: NTreeZ a -> Maybe (NTreeZ a)
zup z = case z ^. zcontext of
           (a, ls, rs) : ps -> Just $ z & zcontext .~ ps
                                       & zfocus   .~ NBranch a (foldl (flip (:)) rs ls)
           _ -> Nothing

-- | Zip-up till top is reached
ztop :: NTreeZ a -> NTree a
ztop z = maybe (z ^. zfocus) ztop (zup z)

-- | Collect zippers with move function
zs :: (NTreeZ a -> Maybe (NTreeZ a)) -> NTreeZ a -> [NTreeZ a]
zs f = drop 1 . s
    where s z = z : maybe [] s (f z)

-- | Left siblings in reverse order
zlefts :: NTreeZ a -> [NTreeZ a]
zlefts  = zs zleft

-- | Righ siblings in order
zrights :: NTreeZ a -> [NTreeZ a]
zrights = zs zright

-- | Ansestors from closest
zups :: NTreeZ a -> [NTreeZ a]
zups = drop 1 . ups
    where ups z = z : maybe [] ups (zup z)

-- | Rewrite tree with provided function in depth first order
zrewrite :: (NTreeZ a -> NTree a) -> NTreeZ a -> NTreeZ a
zrewrite f = walkd
    where -- walk down
          walkd z = let z' = z & zfocus .~ f z
                    in maybe z' (fromMaybe z' . zup . walkr) (zdown z')
          -- walk right
          walkr z = let z' = walkd z
                    in maybe z' walkr (zright z')

