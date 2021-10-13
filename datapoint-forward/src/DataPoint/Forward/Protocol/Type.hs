{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the trace forwarding/accepting protocol.
--
-- Since we are using a typed protocol framework this is in some sense /the/
-- definition of the protocol: what is allowed and what is not allowed.

module DataPoint.Forward.Protocol.Type
  ( DataPointForward (..)
  , Message (..)
  , ClientHasAgency (..)
  , ServerHasAgency (..)
  , NobodyHasAgency (..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import           Network.TypedProtocol.Core (Protocol (..))
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
-- IMPORTANT NOTE: the following terminology is used:
--
-- 1. From the protocol's point of view, two peers talk to each other:
--    the forwarder and the acceptor.
-- 2. The forwarder is an application that collects 'TraceObject's and sends
--    them to the acceptor by request (with 'MsgTraceObjectsReply').
-- 3. The acceptor is an application that receives 'TraceObject's from the
--    forwarder.
-- 4. You can think of the acceptor as a client, and the forwarder as a server.
--    After the connection is established, the acceptor asks for 'TraceObject's,
--    the forwarder replies to it.

data DataPointForward lo where

  -- | Both acceptor and forwarder are in idle state. The acceptor can send a
  -- request  for a list of 'TraceObject's ('MsgTraceObjectsRequest');
  -- the forwarder is waiting for a request, it will replay with 'MsgTraceObjectsReply'.
  StIdle :: DataPointForward lo

  -- | The acceptor has sent a next request for 'TraceObject's. The acceptor is
  -- now waiting for a reply, and the forwarder is busy getting ready to send a
  -- reply with new list of 'TraceObject's.
  StBusy :: DataPointForward lo

  -- | Both the acceptor and forwarder are in the terminal state. They're done.
  StDone :: DataPointForward lo

instance (ShowProxy lo)
      => ShowProxy (DataPointForward lo) where
  showProxy _ = concat
    [ "DataPointForward ("
    , showProxy (Proxy :: Proxy lo)
    , ")"
    ]

instance Protocol (DataPointForward lo) where

  -- | The messages in the trace forwarding/accepting protocol.
  --
  data Message (DataPointForward lo) from to where
    -- | Request the list of 'TraceObject's from the forwarder.
    --   State: Idle -> Busy.
    MsgTraceObjectsRequest
      :: [Text]
      -> Message (DataPointForward lo) 'StIdle 'StBusy

    -- | Reply with a list of 'TraceObject's for the acceptor.
    -- State: Busy -> Idle.
    MsgTraceObjectsReply
      :: [lo]
      -> Message (DataPointForward lo) 'StBusy 'StIdle

    -- | Terminating message. State: Idle -> Done.
    MsgDone
      :: Message (DataPointForward lo) 'StIdle 'StDone

  -- | This is an explanation of our states, in terms of which party has agency
  -- in each state.
  --
  -- 1. When both peers are in Idle state, the acceptor can send a message
  --    to the forwarder (request for new 'TraceObject's),
  -- 2. When both peers are in Busy state, the forwarder is expected to send
  --    a reply to the acceptor (list of new 'TraceObject's).
  --
  -- So we assume that, from __interaction__ point of view:
  -- 1. ClientHasAgency (from 'Network.TypedProtocol.Core') corresponds to acceptor's agency.
  -- 3. ServerHasAgency (from 'Network.TypedProtocol.Core') corresponds to forwarder's agency.
  --
  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle

  data ServerHasAgency st where
    TokBusy :: ServerHasAgency 'StBusy

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  -- | Impossible cases.
  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

instance Show lo
      => Show (Message (DataPointForward lo) from to) where
  show MsgTraceObjectsRequest{} = "MsgTraceObjectsRequest"
  show MsgTraceObjectsReply{}   = "MsgTraceObjectsReply"
  show MsgDone{}                = "MsgDone"

instance Show (ClientHasAgency (st :: DataPointForward lo)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: DataPointForward lo)) where
  show TokBusy{} = "TokBusy"
