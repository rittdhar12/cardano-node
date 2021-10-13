{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module will be used by the acceptor application.
--   Acceptor application asks 'TraceObject's from the forwarder application.
module DataPoint.Forward.Acceptor
  ( runTraceAcceptor
  ) where

import qualified Codec.Serialise as CBOR
import           Data.Typeable (Typeable)

import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           DataPoint.Forward.Network.Acceptor (listenToForwarder)
import           DataPoint.Forward.Configuration (AcceptorConfiguration (..))
import           DataPoint.Forward.Utils (runActionInLoop)

runTraceAcceptor
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => IOManager                -- ^ 'IOManager' from the external application.
  -> AcceptorConfiguration lo -- ^ Acceptor configuration.
  -> ([lo] -> IO ())          -- ^ The handler for 'TraceObject's received from the node.
  -> IO ()
runTraceAcceptor iomgr config@AcceptorConfiguration{forwarderEndpoint} loHandler =
  runActionInLoop (listenToForwarder iomgr config loHandler) forwarderEndpoint 1
