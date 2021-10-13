{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.DataPoint.Forward.Protocol.Codec () where

import           Test.QuickCheck

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec

import           DataPoint.Forward.Protocol.Type

import           Test.DataPoint.Forward.Protocol.TraceItem

instance Arbitrary NumberOfTraceObjects where
  arbitrary = NumberOfTraceObjects <$> arbitrary

instance Arbitrary (AnyMessageAndAgency (DataPointForward TraceItem)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) . MsgTraceObjectsRequest TokBlocking <$> arbitrary
    , AnyMessageAndAgency (ClientAgency TokIdle) . MsgTraceObjectsRequest TokNonBlocking <$> arbitrary
    , AnyMessageAndAgency (ServerAgency (TokBusy TokBlocking)) . MsgTraceObjectsReply . BlockingReply <$> arbitrary
    , AnyMessageAndAgency (ServerAgency (TokBusy TokNonBlocking)) . MsgTraceObjectsReply . NonBlockingReply <$> arbitrary
    , pure  $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance Eq (AnyMessage (DataPointForward TraceItem)) where
  AnyMessage (MsgTraceObjectsRequest TokBlocking r1)
    == AnyMessage (MsgTraceObjectsRequest TokBlocking r2) = r1 == r2
  AnyMessage (MsgTraceObjectsRequest TokNonBlocking r1)
    == AnyMessage (MsgTraceObjectsRequest TokNonBlocking r2) = r1 == r2
  AnyMessage (MsgTraceObjectsReply (BlockingReply r1))
    == AnyMessage (MsgTraceObjectsReply (BlockingReply r2)) = r1 == r2
  AnyMessage (MsgTraceObjectsReply (NonBlockingReply r1))
    == AnyMessage (MsgTraceObjectsReply (NonBlockingReply r2)) = r1 == r2
  AnyMessage MsgDone
    == AnyMessage MsgDone = True
  _ == _ = False
