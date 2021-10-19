{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Plutus.Direct.CertifyingPlutus
  ( hprop_plutus_certifying
  ) where


import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley

import           Control.Monad (void)
import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import           Data.Monoid (Last (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import qualified Data.Text as T
import qualified System.Directory as IO
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))

import           Cardano.CLI.Shelley.Run.Query

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Test.Base as H
import qualified Test.Process as H
import           Testnet.Cardano (defaultTestnetOptions, testnet)
import qualified Testnet.Cardano as TC
import qualified Testnet.Conf as H


{-
The aim is to test a Plutus certifying and rewarding script. Certifying in the sense of validating a certifiate
e.g in this case a delegating certificate and rewarding in the sense of validating a rewards withdrawal.
In this test, we delegate a Plutus script staking address to our stake pool. We must:
  1. Create a stake pool
  2. Delegate our Plutus script address to said staking pool
  3. Withdraw our rewards from our Plutus script staking address.
-}

hprop_plutus_certifying :: Property
hprop_plutus_certifying = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  projectBase <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $ H.mkConf tempAbsBasePath' Nothing

  TC.TestnetRuntime { bftSprockets, testnetMagic } <- testnet defaultTestnetOptions conf

  env <- H.evalIO getEnvironment

  execConfig <- H.noteShow H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName (head bftSprockets))
          ]
          -- The environment must be passed onto child process on Windows in order to
          -- successfully start that process.
          <> env
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  -- First we note all the relevant files
  base <- H.note projectBase
  work <- H.note tempAbsPath

  -- We get our UTxOs from here
  utxoVKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
  utxoVKeyFile2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2.vkey"
  utxoSKeyFile2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2.skey"

  utxoAddr <- H.execCli
                [ "address", "build"
                , "--testnet-magic", show @Int testnetMagic
                , "--payment-verification-key-file", utxoVKeyFile
                ]

  void $ H.execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-1.json"
      ]

  H.cat $ work </> "utxo-1.json"

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo1Json
  txin <- H.noteShow $ head $ Map.keys utxo1

  -- Staking keys
  utxoStakingVkey1 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo-stake.vkey"
  utxoStakingSkey1 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo-stake.skey"
  utxoStakingVkey2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2-stake.vkey"
  utxoStakingSkey2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2-stake.skey"

  utxoaddrwithstaking <- H.execCli [ "address", "build"
                                   , "--payment-verification-key-file", utxoVKeyFile2
                                   , "--stake-verification-key-file", utxoStakingVkey2
                                   , "--testnet-magic", show @Int testnetMagic
                                   ]

  utxostakingaddr <- filter (/= '\n')
                       <$> H.execCli
                             [ "stake-address", "build"
                             , "--stake-verification-key-file", utxoStakingVkey2
                             , "--testnet-magic", show @Int testnetMagic
                             ]

  scriptDummyRedeemer <- H.note $ work </> "mint-script-context-dummy.redeemer"
  scriptContextRedeemer <- H.note $ work </> "mint-script-context.redeemer"

  -- Plutus related
  plutusStakingScript <- H.note $ base </> "scripts/plutus/scripts/guess-42-stake.plutus"
  plutusStakingScriptRedeemer <- H.note $ base </> "scripts/plutus/data/42.redeemer"
  scriptPaymentAddressWithStaking <- H.execCli [ "address", "build"
                                               , "--payment-verification-key-file", utxoVKeyFile
                                               , "--stake-script-file",  plutusStakingScript
                                               , "--testnet-magic", show @Int testnetMagic
                                               ]
  plutusStakingAddr <- filter (/= '\n') <$>
                         H.execCli [ "stake-address", "build"
                                   , "--testnet-magic", show @Int testnetMagic
                                   , "--stake-script-file",  plutusStakingScript
                                   ]
  -- Stake pool related
  poolownerstakekey <- H.note $ tempAbsPath </> "addresses/pool-owner1-stake.vkey"
  poolownerverkey <- H.note $ tempAbsPath </> "addresses/pool-owner1.vkey"
  poolownerstakeaddr <- filter (/= '\n')
                          <$> H.execCli
                                [ "stake-address", "build"
                                , "--stake-verification-key-file", poolownerstakekey
                                , "--testnet-magic", show @Int testnetMagic
                                ]

  poolowneraddresswstakecred <- H.execCli [ "address", "build"
                                          , "--payment-verification-key-file", poolownerverkey
                                          , "--stake-verification-key-file",  poolownerstakekey
                                          , "--testnet-magic", show @Int testnetMagic
                                          ]
  poolcoldVkey <- H.note $ tempAbsPath </> "node-pool1/shelley/operator.vkey"
  poolcoldSkey <- H.note $ tempAbsPath </> "node-pool1/shelley/operator.skey"

  stakePoolId <- filter ( /= '\n') <$>
                   H.execCli [ "stake-pool", "id"
                             , "--cold-verification-key-file", poolcoldVkey
                             ]

  -- REGISTER PLEDGER POOL

  -- Create pledger registration certificate
  void $ H.execCli
            [ "stake-address", "registration-certificate"
            , "--stake-verification-key-file", poolownerstakekey
            , "--out-file", work </> "pledger.regcert"
            ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address",  utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Int 5000000
    , "--tx-out", poolowneraddresswstakecred <> "+" <> show @Int 5000000
    , "--tx-out", utxoaddrwithstaking <> "+" <> show @Int 5000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "pledger.regcert"
    , "--out-file", work </> "pledge-registration-cert.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "pledge-registration-cert.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "pledge-registration-cert.tx"
    ]

  H.note_ "Submitting pool owner/pledge stake registration cert and funding stake pool owner address..."

  void $ H.execCli' execConfig
               [ "transaction", "submit"
               , "--tx-file", work </> "pledge-registration-cert.tx"
               , "--testnet-magic", show @Int testnetMagic
               ]

  -- Wait 5 seconds
  H.threadDelay 5000000

  -- Check to see if pledge's stake address was registered

  void $ H.execCli' execConfig
    [ "query",  "stake-address-info"
    , "--address", poolownerstakeaddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pledgeownerregistration.json"
    ]

  pledgerStakeInfo <- H.leftFailM . H.readJsonFile $ work </> "pledgeownerregistration.json"
  delegsAndRewardsMap <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards pledgerStakeInfo
  let delegsAndRewards = mergeDelegsAndRewards delegsAndRewardsMap

  length delegsAndRewards === 1

  let (pledgerSAddr, _rewards, _poolId) = head delegsAndRewards

  -- Pledger and owner are and can be the same
  T.unpack (serialiseAddress pledgerSAddr) === poolownerstakeaddr

  H.note_ $ "Register staking key: " <> show utxoStakingVkey2

  void $ H.execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoaddrwithstaking
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-addr-with-staking-1.json"
      ]

  H.cat $ work </> "utxo-addr-with-staking-1.json"

  utxoWithStaking1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-addr-with-staking-1.json"
  UTxO utxoWithStaking1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxoWithStaking1Json
  txinForStakeReg <- H.noteShow $ head $ Map.keys utxoWithStaking1

  void $ H.execCli [ "stake-address", "registration-certificate"
                   , "--stake-verification-key-file", utxoStakingVkey2
                   , "--out-file", work </> "stakekey.regcert"
                   ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoaddrwithstaking
    , "--tx-in", T.unpack (renderTxIn txinForStakeReg)
    , "--tx-out", utxoaddrwithstaking <> "+" <> show @Int 1000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "stakekey.regcert"
    , "--out-file", work </> "key-registration-cert.txbody"
    ]

  void $ H.execCli [ "transaction", "sign"
                   , "--tx-body-file", work </> "key-registration-cert.txbody"
                   , "--testnet-magic", show @Int testnetMagic
                   , "--signing-key-file", utxoStakingSkey2
                   , "--signing-key-file", utxoSKeyFile2
                   , "--out-file", work </> "key-registration-cert.tx"
                   ]


  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "key-registration-cert.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.note_ $ "Check to see if " <> utxoStakingVkey2 <> " was registered..."
  H.threadDelay 10000000

  void $ H.execCli' execConfig
    [ "query", "stake-address-info"
    , "--address", utxostakingaddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "stake-address-info-utxo-staking-vkey-2.json"
    ]

  userStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "stake-address-info-utxo-staking-vkey-2.json"
  delegsAndRewardsMapUser <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards userStakeAddrInfoJSON
  let delegsAndRewardsUser = mergeDelegsAndRewards delegsAndRewardsMapUser
      userStakeAddrInfo = filter (\(sAddr,_,_) -> utxostakingaddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsUser
      (userSAddr, _rewards, _poolId) = head userStakeAddrInfo


  H.note_ $ "Check staking key: " <> show utxoStakingVkey2 <> " was registered"
  T.unpack (serialiseAddress userSAddr) === utxostakingaddr

  H.note_  "Get updated UTxO"

  void $ H.execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-2.json"
      ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo2Json
  txin2 <- H.noteShow $ head $ Map.keys utxo2

  H.note_ "Create delegation certificate of pledger"

  void $ H.execCli
    [ "stake-address", "delegation-certificate"
    , "--stake-verification-key-file", poolownerstakekey
    , "--cold-verification-key-file", poolcoldVkey
    , "--out-file", work </> "pledger.delegcert"
    ]

  H.note_ "Register stake pool and delegate pledger to stake pool in a single tx"

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address",  utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin2
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Int 5000000
    , "--tx-out", utxoAddr <> "+" <> show @Int 10000000
    , "--witness-override", show @Int 3
    , "--certificate-file", tempAbsPath </> "node-pool1/registration.cert"
    , "--certificate-file", work </> "pledger.delegcert"
    , "--out-file", work </> "register-stake-pool.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "register-stake-pool.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", poolcoldSkey
    , "--signing-key-file", tempAbsPath </> "node-pool1/owner.skey"
    , "--out-file", work </> "register-stake-pool.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "register-stake-pool.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 10000000

  void $ H.execCli' execConfig
    [ "query", "stake-pools"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "current-registered.pools.json"
    ]

  currRegPools <- H.leftFailM . H.readJsonFile $ work </> "current-registered.pools.json"
  poolIds <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(Set PoolId) currRegPools
  poolId <- H.noteShow $ head $ Set.toList poolIds

  H.note_ "Check stake pool was successfully registered"
  T.unpack (serialiseToBech32 poolId) === stakePoolId

  H.note_ "Check pledge was successfully delegated"
  void $ H.execCli' execConfig
      [ "query", "stake-address-info"
      , "--address", poolownerstakeaddr
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "pledge-stake-address-info.json"
      ]

  pledgeStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "pledge-stake-address-info.json"
  delegsAndRewardsMapPledge <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards pledgeStakeAddrInfoJSON
  let delegsAndRewardsPledge = mergeDelegsAndRewards delegsAndRewardsMapPledge
      pledgeStakeAddrInfo = filter (\(sAddr,_,_) -> poolownerstakeaddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsPledge
      (pledgeSAddr, _rewards, _poolId) = head pledgeStakeAddrInfo

  H.note_ "Check pledge has been delegated to pool"
  T.unpack (serialiseAddress pledgeSAddr) === poolownerstakeaddr

  H.note_ "We have a fully functioning stake pool at this point. We now want to test Plutus staking script withdrawals."

  H.note_ "We now create the Plutus script staking registration certificate"

  H.note_ "Get updated UTxO"

  void $ H.execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-3.json"
      ]

  H.cat $ work </> "utxo-3.json"

  utxo3Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-3.json"
  UTxO utxo3 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo3Json
  txin3 <- H.noteShow $ Map.keys utxo3 !! 0
  txinCollateral <- H.noteShow $ Map.keys utxo3 !! 1

  void $ H.execCli
    [ "stake-address", "registration-certificate"
    , "--stake-script-file", plutusStakingScript
    , "--out-file", work </> "script.regcert"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin3
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Int 5000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "script.regcert"
    , "--out-file", work </> "register-plutus-staking-script.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "register-plutus-staking-script.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "register-plutus-staking-script.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "register-plutus-staking-script.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 10000000

  H.note_ "Check if Plutus staking script address was registered"

  void $ H.execCli' execConfig
      [ "query", "stake-address-info"
      , "--address", plutusStakingAddr
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "pledge-stake-address-info.json"
      ]

  plutusStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "pledge-stake-address-info.json"
  delegsAndRewardsMapPlutus <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards plutusStakeAddrInfoJSON
  let delegsAndRewardsPlutus = mergeDelegsAndRewards delegsAndRewardsMapPlutus
      plutusStakeAddrInfo = filter (\(sAddr,_,_) -> plutusStakingAddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsPlutus
      (plutusSAddr, _rewards, _poolId) = head plutusStakeAddrInfo

  H.note_ "Check if Plutus staking script has been registered"
  T.unpack (serialiseAddress plutusSAddr) === plutusStakingAddr

  H.note_ "Create delegation certificate for Plutus staking script to stake pool"

  void $ H.execCli
    [ "stake-address", "delegation-certificate"
    , "--stake-script-file", plutusStakingScript
    , "--cold-verification-key-file", poolcoldVkey
    , "--out-file", work </> "plutus-script.delegcert"
    ]

  H.note_ "Get updated UTxO"

  void $ H.execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-4.json"
      ]

  H.cat $ work </> "utxo-4.json"

  utxo4Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-4.json"
  UTxO utxo4 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo4Json
  txin4 <- H.noteShow $ Map.keys utxo4 !! 0
  txinCollateral1 <- H.noteShow $ Map.keys utxo4 !! 1

  H.note_ "Delegate Plutus staking script to stake pool"

  void $ H.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin4
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral1
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Int 5000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "plutus-script.delegcert"
    , "--certificate-script-file", plutusStakingScript
    , "--certificate-redeemer-file", plutusStakingScriptRedeemer
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "delegate-staking-script.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "delegate-staking-script.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "delegate-staking-script.tx"
    ]

  void $ H.execCli' execConfig
               [ "transaction", "submit"
               , "--tx-file", work </> "delegate-staking-script.tx"
               , "--testnet-magic", show @Int testnetMagic
               ]

  -- Wait 5 seconds
  H.threadDelay 5000000

  H.note_ "Check to see if staking script was delegated"

  void $ H.execCli' execConfig
    [ "query",  "stake-address-info"
    , "--address", plutusStakingAddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "plutus-staking-script-delegation.json"
    ]

  stakingScriptAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "plutus-staking-script-delegation.json"
  delegsAndRewardsMapStakingScript <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards stakingScriptAddrInfoJSON
  let delegsAndRewardsStakingScript = mergeDelegsAndRewards delegsAndRewardsMapStakingScript
      stakingScriptAddrInfo = filter (\(sAddr,_,_) -> plutusStakingAddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsStakingScript
      (stakingSAddr, _rewards, _poolId) = head stakingScriptAddrInfo

  H.note_ $ "Check plutus staking script: " <> (work </> "plutus-staking-script-delegation.json") <> " was delegated"
  T.unpack (serialiseAddress stakingSAddr) === plutusStakingAddr

  H.note "Wait for rewards to be paid out. This will be current epoch + 4"
  -- TODO: Get current epoch. Add 4 to it. Loop and check.
  -- when we hut n + 4 check stake address info to see
  -- if we have a non-zero value in there.

  -- TODO: Left off here
  tipJSON <- H.execCli' execConfig
    [ "query",  "tip"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> ""
{-

  policyId <- filter (/= '\n')
                <$> H.execCli
                      [ "transaction", "policyid"
                      , "--script-file", plutusContextEqualityMintScript
                      ]

  void . H.note $ "Policy ID: " <> policyId

  H.noteEachM_ . H.listDirectory $ base
  H.noteEachM_ . H.listDirectory $ base </> "scripts"
  H.noteEachM_ . H.listDirectory $ base </> "scripts/plutus"
  H.noteEachM_ . H.listDirectory $ base </> "scripts/plutus/scripts"



  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  H.cat $ work </> "utxo-1.json"

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo1Json
  txin <- H.noteShow $ head $ Map.keys utxo1
  TxOut _ txoutVal _ <- H.nothingFailM . H.noteShow $ Map.lookup txin utxo1
  let Lovelace lovelaceAtTxin = txOutValueToLovelace txoutVal
  lovelaceAtTxinDiv3 <- H.noteShow $ lovelaceAtTxin `div` 3

  void $ H.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  let dummyaddress = "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4"


  -- STEP 1 - Create collateral
  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin
    , "--tx-out", utxoAddr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "create-collateral-output.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "create-collateral-output.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "create-collateral-output.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "create-collateral-output.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 5000000

  -- STEP 2
  -- We need to create a dummy tx in order to create the script context redeemer
  -- that we want to use when attempting to spend the spending script locked UTxO

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json :: Aeson.Value <- H.leftFailM $ H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo2Json
  txinFunding <- H.noteShow . head $ Map.keys utxo2
  txinCollateral <- H.noteShow $ Map.keys utxo2 !! 1

  void $ execCreateScriptContext ["--out-file", scriptDummyRedeemer]

  H.cat $ work </> scriptDummyRedeemer

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--script-invalid"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--invalid-before", "1"
    , "--invalid-hereafter", "3000"
    , "--required-signer", requiredSignerSKey
    , "--tx-in", T.unpack $ renderTxIn txinFunding
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral
    , "--mint-script-file", plutusContextEqualityMintScript
    , "--mint-redeemer-file", scriptDummyRedeemer
    , "--tx-out", dummyaddress <> "+" <> show @Integer 10000000 <> "+ 5 " <> (policyId <> ".MillarCoin")
    , "--mint", "5 " <> (policyId <> ".MillarCoin")
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "mint-dummy.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "mint-dummy.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "mint-dummy.tx"
    ]

  -- Generate the redeeemer we will use in the tx!
  void $ execCreateScriptContext' execConfig
           [ "--generate-tx" , work </> "mint-dummy.tx"
           , "--cardano-mode"
           , "--testnet-magic", show @Int testnetMagic
           , "--out-file", scriptContextRedeemer
           ]

  H.cat $ work </> scriptContextRedeemer

  H.threadDelay 5000000

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--script-valid"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--invalid-before", "1"
    , "--invalid-hereafter", "3000"
    , "--required-signer", requiredSignerSKey
    , "--tx-in", T.unpack $ renderTxIn txinFunding
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral
    , "--mint-script-file", plutusContextEqualityMintScript
    , "--mint-redeemer-file", scriptContextRedeemer
    , "--tx-out", dummyaddress <> "+" <> show @Integer 10000000 <> "+ 5 " <> (policyId <> ".MillarCoin")
    , "--mint", "5 " <> (policyId <> ".MillarCoin")
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "mint-final.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "mint-final.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", requiredSignerSKey
    , "--out-file", work </> "mint-final.tx"
    ]


  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "mint-final.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 5000000

  -- Query UTxO at dummyAddress.

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", dummyaddress
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "dummyaddress.json"
    ]

  H.cat $ work </> "dummyaddress.json"

  dummyUtxoJson <- H.leftFailM . H.readJsonFile $ work </> "dummyaddress.json"
  UTxO dummyUtxo <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) dummyUtxoJson

  let allValues = mconcat . map (\(TxOut _ val _) -> txOutValueToValue val) $ Map.elems dummyUtxo
      millarAssetId = AssetId (fromString policyId) $ fromString "MillarCoin"

  -- There should be a multi asset value at the dummy address
  1 === length (valueToList $ filterValue (== millarAssetId) allValues)
  -}
