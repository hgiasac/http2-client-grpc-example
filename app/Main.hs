{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad         (replicateM_, void)
import           Data.Monoid           ((<>))
import           Lib
import           Network.HTTP2.Client
import           Options.Applicative
import           System.Environment    (getArgs)

import qualified Data.ByteString.Char8 as BS

logBs :: IO BS.ByteString
logBs = BS.pack <$> readFile "./log.json"

-- main :: IO ()
-- main =
--   replicateM_ 50 $
--     runClientIO runUnary >>= \case
--       Left e -> print e
--       Right _ -> return ()

main :: IO ()
main = do
  bs <- logBs
  replicateM_ 1000 $
    runClientIO (runBiDirectionalStream bs) >>= \case
      Left e -> print e
      Right _ -> return ()
