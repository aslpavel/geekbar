module GeekBar.Node ( Node
                    , props
                    , NodeZ
                    , module NT) where

import GeekBar.NTree as NT
import GeekBar.Props (Props)
import Control.Lens  (Lens')

type Node  = NTree Props
type NodeZ = NTreeZ Props

props :: Lens' Node Props
props = value
