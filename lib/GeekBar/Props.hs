{-# LANGUAGE TemplateHaskell #-}
module GeekBar.Props ( Color
                     , Align(..)
                     , Rect(Rect), x, y, width, height
                     , Edge(Edge), left, right, top, bottom
                     , Props, classes, uid, content, padding, border, margin, align
                     ) where
import Control.Lens (makeLenses)
import qualified Data.Set  as S
import qualified Data.Text as T


-- | Rectangular area
data Rect  = Rect { _x      :: !Float
                  , _y      :: !Float
                  , _width  :: !Float
                  , _height :: !Float
                  } deriving Show
makeLenses ''Rect

-- | Edge sizes (padding, border, margin)
data Edge  = Edge { _left   :: !Float
                  , _right  :: !Float
                  , _bottom :: !Float
                  , _top    :: !Float
                  } deriving Show
makeLenses ''Edge

-- | 
data Color = Color deriving Show

data Align = AlignHor | AlignVer | AlignAbs deriving Show

-- | Node properties (sizes colors animation etc..)
data Props = Props { _classes :: S.Set T.Text
                   , _uid     :: !T.Text
                   {-
                   , _fgColor :: !Color
                   , _bgColor :: !Color
                   , _opacity :: !Float
                   -}
                   -- layout
                   , _content :: !Rect
                   , _padding :: !Edge
                   , _border  :: !Edge
                   , _margin  :: !Edge
                   , _align   :: !Align
                   } deriving Show
makeLenses ''Props

{- Alternative way wich allows to use same name (HasX, HasY typeclasses)
L.declareFields [d|
  -- | Rectangle Area
  data Rect = Rect { rectX
                   , rectY
                   , rectWidth
                   , rectHeight :: !Float
                   }
  -- | Edge Sizes
  data Edge = Edge { edgeLeft
                   , edgeRight
                   , edgeBottom
                   , edgeTop :: !Float
                   }
  -- | Elemnt Dimenstions
  data Dim  = Dim  { dimContent :: !Rect
                   , dimPadding :: !Edge
                   , dimBorder  :: !Edge
                   , dimMargin  :: !Edge
                   }
  |]
-}
