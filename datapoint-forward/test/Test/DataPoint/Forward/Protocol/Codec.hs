{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.DataPoint.Forward.Protocol.Codec () where

import           Test.QuickCheck

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec

import           DataPoint.Forward.Protocol.Type

import           Test.DataPoint.Forward.Protocol.TraceItem

instance Arbitrary (AnyMessageAndAgency (DataPointForward TraceItem)) where
  arbitrary = oneof
    [ pure $ AnyMessageAndAgency (ClientAgency TokIdle) (MsgTraceObjectsRequest [""])
    , AnyMessageAndAgency (ServerAgency TokBusy) . MsgTraceObjectsReply <$> arbitrary
    , pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance Eq (AnyMessage (DataPointForward TraceItem)) where
  AnyMessage (MsgTraceObjectsRequest r1)
    == AnyMessage (MsgTraceObjectsRequest r2) = r1 == r2
  AnyMessage (MsgTraceObjectsReply r1)
    == AnyMessage (MsgTraceObjectsReply r2) = r1 == r2
  AnyMessage MsgDone
    == AnyMessage MsgDone = True
  _ == _ = False
