{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Exception

import qualified Data.ByteString.Char8 as BL
import qualified Data.ProtoLens.Message          as PLM
import qualified Data.Text.Lazy                  as TL
import qualified GRPC                            as R
import qualified Network.GRPC.Client             as GC
import qualified Network.GRPC.Client.Helpers     as GCH
import qualified Network.HTTP2.Client            as HC

import           Control.Concurrent.Async.Lifted (async, wait)
import           Control.Concurrent.Lifted       (threadDelay)

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Network.GRPC.HTTP2.Encoding
import           Network.GRPC.HTTP2.Types        (IsRPC (..))
import           Network.HTTP2.Client.Exceptions (ClientIO)

import           Proto.Protos.Grpcbin
import           Proto.Protos.Grpcbin_Fields

printDone :: Show a => String -> a -> ClientIO ()
printDone h v = printIO ("Done " ++ h ++ ": " ++ show v)

printIO :: (MonadIO m, Show a) => a -> m ()
printIO = liftIO . print

mkClient :: ClientIO GCH.GrpcClient
mkClient = GCH.setupGrpcClient $ GCH.GrpcClientConfig
    "logs-grpc.lux-dev.hasura.me"
    80
    [("authorization", "some-secret-token")]
    (GC.Timeout 3000)
    GC.uncompressed
    Nothing
    (liftIO . throwIO)
    HC.ignoreFallbackHandler
    5000000
    1000000

runUnary :: BL.ByteString -> ClientIO ()
runUnary bs = do
  client <- mkClient
  ret <- GCH.rawUnary sayHelloRPC client request :: ClientIO (Either HC.TooMuchConcurrency (GC.RawReply LogResponse))
  liftIO $ print ret
  return ()
  where
    sayHelloRPC = R.createRPC "SendLog"
    request :: LogRequest
    request = R.createLogRequest "inputPayload"

runBiDirectionalStream :: BL.ByteString -> ClientIO ()
runBiDirectionalStream bs = do
  client <- mkClient
  bidirStreamThread <- async $
    printDone "bidir-client" =<< GCH.rawSteppedBidirectional
      rpc
      client
      (10::Int)
      bidiloop
  wait bidirStreamThread

  where
    rpc = R.createRPC "SendLog"

    bidiloop :: GC.RunBiDiStep LogRequest LogResponse Int
    bidiloop n = do
      printIO "bidi-loop"
      threadDelay 1000
      if n > 0
      then do
          if n `mod` 2 == 0
          then do
            printIO "bidi-loop-send"
            return (n - 1, GC.SendInput GC.Uncompressed $ R.createLogRequest bs)
          else do
              printIO "bidi-loop-rcv"
              return (n, GC.WaitOutput
                  (\hdrs m msg -> do
                      printIO ("bidi-out", hdrs, m, msg)
                      return (m - 1))
                  (\hdrs m trls -> do
                      printIO ("bidi-closed", m, trls)
                      return m))
      else do
          printIO ("stop bidiloop", n)
          return (n, GC.Abort)
