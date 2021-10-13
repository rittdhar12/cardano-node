module Test.DataPoint.Forward.Demo.Configs
  ( mkAcceptorConfig
  , mkForwarderConfig
  ) where

import           Control.Tracer (nullTracer)
import           GHC.Conc (TVar)

import           DataPoint.Forward.Configuration
import           DataPoint.Forward.Protocol.Type

import           Test.DataPoint.Forward.Protocol.TraceItem

mkAcceptorConfig
  :: HowToConnect
  -> TVar Bool
  -> AcceptorConfiguration TraceItem
mkAcceptorConfig ep weAreDone =
  AcceptorConfiguration
    { acceptorTracer    = nullTracer
    , forwarderEndpoint = ep
    , shouldWeStop      = weAreDone
    }

mkForwarderConfig
  :: HowToConnect
  -> Word
  -> Word
  -> ForwarderConfiguration TraceItem
mkForwarderConfig ep disconnectedSize connectedSize =
  ForwarderConfiguration
    { forwarderTracer       = nullTracer
    , acceptorEndpoint      = ep
    , disconnectedQueueSize = disconnectedSize
    , connectedQueueSize    = connectedSize
    }
