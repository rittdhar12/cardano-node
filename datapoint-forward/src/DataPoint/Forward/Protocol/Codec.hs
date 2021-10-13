{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataPoint.Forward.Protocol.Codec
  ( codecDataPointForward
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import           Text.Printf (printf)
import           Ouroboros.Network.Codec (Codec, PeerHasAgency (..),
                                          PeerRole (..), SomeMessage (..),
                                          mkCodecCborLazyBS)

import           DataPoint.Forward.Protocol.Type

codecDataPointForward
  :: forall lo m.
     MonadST m
  => ([Text] -> CBOR.Encoding)          -- ^ Encoder for 'Request'.
  -> (forall s . CBOR.Decoder s [Text]) -- ^ Decoder for 'Request'.
  -> ([lo] -> CBOR.Encoding)            -- ^ Encoder for reply with list of 'TraceObject's.
  -> (forall s . CBOR.Decoder s [lo])   -- ^ Decoder for reply with list of 'TraceObject's.
  -> Codec (DataPointForward lo)
           DeserialiseFailure m LBS.ByteString
codecDataPointForward encodeRequest   decodeRequest
                      encodeReplyList decodeReplyList =
  mkCodecCborLazyBS encode decode
 where
  -- Encode messages.
  encode
    :: forall (pr  :: PeerRole)
              (st  :: DataPointForward lo)
              (st' :: DataPointForward lo).
       PeerHasAgency pr st
    -> Message (DataPointForward lo) st st'
    -> CBOR.Encoding

  encode (ClientAgency TokIdle) (MsgTraceObjectsRequest request) =
         CBOR.encodeListLen 3
      <> CBOR.encodeWord 1
      <> encodeRequest request

  encode (ClientAgency TokIdle) MsgDone =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 2

  encode (ServerAgency TokBusy) (MsgTraceObjectsReply reply) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 4
      <> encodeReplyList reply

  -- Decode messages
  decode
    :: forall (pr :: PeerRole)
              (st :: DataPointForward lo) s.
       PeerHasAgency pr st
    -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stok) of
      (1, 3, ClientAgency TokIdle) -> do
        request <- decodeRequest
        return $! SomeMessage $ MsgTraceObjectsRequest request

      (2, 1, ClientAgency TokIdle) ->
        return $ SomeMessage MsgDone

      (4, 2, ServerAgency TokBusy) -> do
        replyList <- decodeReplyList
        return $ SomeMessage (MsgTraceObjectsReply replyList)

      -- Failures per protocol state
      (_, _, ClientAgency TokIdle) ->
        fail (printf "codecDataPointForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency TokBusy) ->
        fail (printf "codecDataPointForward (%s) unexpected key (%d, %d)" (show stok) key len)
