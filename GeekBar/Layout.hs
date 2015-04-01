module GeekBar.Layout (layout) where

import GeekBar.Node
import GeekBar.Props

import Control.Lens
import qualified Control.Monad.State.Strict as ST


-- | Layout nodes
-- Updates content{x,y} to its correct values based on position of upper-left
-- corner and properites of the node.
layout :: (Float, Float) -> Node -> Node
layout (x', y') (NLeaf p) = NLeaf (p & content . x .~ x'
                                     & content . y .~ y')
layout (x', y') (NBranch p cs) =
    alignWith $ case p ^. align of
                  AlignHor -> alignHor
                  AlignVer -> alignVer
                  AlignAbs -> alignAbs
    where
      -- | Align children with align function
      alignWith :: (Node -> ST.State (Rect, Edge) Node) -> Node
      alignWith a = let (cs', (r, _)) = ST.runState (mapM a cs) (Rect x' y' 0 0, Edge 0 0 0 0)
                    in NBranch (p & content .~ r) cs'
      -- | Horiziontal alignment
      -- State is (<all_content_rect>, <margin_edge>)
      alignHor :: Node -> ST.State (Rect, Edge) Node
      alignHor n = do
        (r, m) <- ST.get
        let -- x coordinate for contents of current node
            x'' = r ^. x
               + r ^. width
               + max (m ^. right) (n ^. props.margin.left)
               + n ^. props.border.left
               + n ^. props.padding.left
            -- y cooridnate for contents of current nede
            y'' = r ^. y
               + n ^. props.margin.top
               + n ^. props.border.top
               + n ^. props.padding.top
            -- layout current node with regards to colclulated coordiantes
            n'' = layout (x'', y'') n
            -- new width for contnet area
            w  = x''
               - r ^. x
               + n'' ^. props.content.width
               + n'' ^. props.padding.right
               + n'' ^. props.border.right
            -- new neight for content area
            h  = y''
               - r  ^. y
               + n'' ^. props.content.height
               + n'' ^. props.padding.bottom
               + n'' ^. props.border.bottom
               + n'' ^. props.margin.bottom
        ST.put (r & width .~ w & height .~ max h (r ^. height)
               ,m & right .~ n'' ^. props.margin.right)
        return n''
      -- | vertical alignment
      alignVer :: Node -> ST.State (Rect, Edge) Node
      alignVer n = do
        (r, m) <- ST.get
        let x'' = r ^. x
               + n ^. props.margin.left
               + n ^. props.border.left
               + n ^. props.padding.left
            y'' = r ^. y
               + r ^. height
               + max (m ^. bottom) (n ^. props.margin.top)
               + n ^. props.border.top
               + n ^. props.padding.top
            n'' = layout (x'', y'') n
            w  = x''
               - r  ^. x
               + n'' ^. props.content.width
               + n'' ^. props.padding.right
               + n'' ^. props.border.right
               + n'' ^. props.margin.right
            h  = y''
               - r ^. y
               + n'' ^. props.content.height
               + n'' ^. props.padding.bottom
               + n'' ^. props.border.bottom
        ST.put (r & width .~ max w (r ^. width) & height .~ h
               ,m & bottom .~ n'' ^. props.margin.bottom)
        return n''
      -- | absolute alignment
      alignAbs :: Node -> ST.State (Rect, Edge) Node
      alignAbs n = do
        (r, m) <- ST.get
        let x'' = r ^. x
               + n ^. props.margin.left
               + n ^. props.border.left
               + n ^. props.padding.left
            y'' = r ^. y
               + n ^. props.margin.top
               + n ^. props.border.top
               + n ^. props.padding.top
            n'' = layout (x'', y'') n
            w  = x''
               - r  ^. x
               + n'' ^. props.content.width
               + n'' ^. props.padding.right
               + n'' ^. props.border.right
               + n'' ^. props.margin.right
            h  = y''
               - r  ^. y
               + n'' ^. props.content.height
               + n'' ^. props.padding.bottom
               + n'' ^. props.border.bottom
               + n'' ^. props.margin.bottom
        ST.put (r & width .~ max w (r ^. width) & height .~ max h (r ^. height), m)
        return n''
