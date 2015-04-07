{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Wire
import Control.Wire.Unsafe.Event
import Prelude hiding ((.), id)
import qualified Control.Monad as M


keySource :: IO (IO Char)
keySource = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  queue <- atomically newTQueue
  forkIO . M.forever $ getChar >>= atomically . writeTQueue queue
  return . atomically $ readTQueue queue


main :: IO ()
main = do
  get <- keySource
  let run s w = do
         k <- get
         M.unless (k == 'q') $ do
              (st, s') <- stepSession s
              (o,  w') <- stepWire w st (Right k)
              case o of
                Left  _ -> return ()
                Right t -> putStrLn $  "key:"   <> show k
                                   <> " time:" <> show (t :: NominalDiffTime)
                                   <> " delta:"    <> show (dtime st :: NominalDiffTime)
              run s' w'
  run clockSession_ (modes '_' (const time) . (id &&& fmap Event id))
  putStrLn "'q' was entered, quiting ..."
