{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module GRPC where

import           Data.ProtoLens.Labels         ()
import           Data.ProtoLens.Service.Types
import           Data.Proxy
import           GHC.TypeLits
import           Lens.Micro                    ((&), (.~), (^.))
import           Network.GRPC.HTTP2.Encoding   (Compression (..), Decoder,
                                                GRPCInput (..), GRPCOutput (..))
import           Network.GRPC.HTTP2.Proto3Wire
import           Network.GRPC.HTTP2.Types      (IsRPC (..))
import           Proto3.Wire

import qualified Data.ByteString.Char8         as BS
import qualified Data.Monoid                   as MND
import qualified Data.ProtoLens.Message        as PLM
import qualified Data.Text.Lazy                as TL
import qualified Proto3.Wire.Decode            as Dec
import qualified Proto3.Wire.Encode            as Enc

import           Proto.Protos.Grpcbin
import           Proto.Protos.Grpcbin_Fields

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
