{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GRPC where

import Data.Proxy
import GHC.TypeLits
import Network.GRPC.HTTP2.Types                 (IsRPC(..))
import Network.GRPC.HTTP2.Encoding              ( GRPCInput(..)
                                                , GRPCOutput(..)
                                                , Compression(..)
                                                , Decoder
                                                ) 
import           Lens.Micro                     ( (^.)
                                                , (&)
                                                , (.~)
                                                )
import Network.GRPC.HTTP2.Proto3Wire
import Data.ProtoLens.Service.Types
import Data.ProtoLens.Labels ()

import qualified Data.Text.Lazy                 as TL

import qualified Data.Monoid                    as MND
import qualified Data.ByteString.Char8          as BS
import           Proto3.Wire
import qualified Proto3.Wire.Encode             as Enc
import qualified Proto3.Wire.Decode             as Dec
import qualified Data.ProtoLens.Message         as PLM
import Proto.Protos.Grpcbin
import Proto.Protos.Grpcbin_Fields

createRPC :: String -> RPC
createRPC = RPC packageName serviceName . BS.pack 
  where
    serviceName = BS.pack $ symbolVal (Proxy :: Proxy (ServiceName LuxLogger))
    packageName = BS.pack $ symbolVal (Proxy :: Proxy (ServicePackage LuxLogger))


createLogRequest :: BS.ByteString -> LogRequest
createLogRequest c =
  PLM.defMessage
    & #logs .~ c


createLogResponse :: TL.Text -> LogResponse
createLogResponse c =
  PLM.defMessage
    & #message .~ TL.toStrict "success"

-- Making concrete encode and decode methods.
instance Proto3WireEncoder LogRequest where
  proto3WireEncode r = Enc.byteString 1 (r ^. #logs)
  proto3WireDecode = createLogRequest <$> (Dec.one Dec.byteString MND.mempty `Dec.at` 1)


-- Making concrete encode and decode methods.
instance Proto3WireEncoder LogResponse where
  proto3WireEncode r = Enc.text 1 . TL.fromStrict $ r ^. #message
  proto3WireDecode = createLogResponse <$> (Dec.one Dec.text MND.mempty `Dec.at` 1)
