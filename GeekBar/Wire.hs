module GeekBar.Wire (render) where

import Control.Monad.Identity (Identity)
import qualified Control.Wire as W

-- |
-- Steps:
--  * Apply static css rules
--  * Apply animation/transform properties
--  * Layout nodes (to be able to find hovered elements)
--  if there is mouse signal
--  * Calculate hovered/clicked elements based on mouse coordinates
--  * Apply hover css rules
--  * Layout nodes (new :hover rules might have changed layout)
--  * Return final tree and tompost subscribed click event
--
render :: W.Wire s () Identity a b
render = undefined

