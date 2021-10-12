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

import           Control.Monad
import           Data.Monoid (Last (..))
import           Data.String
import           Hedgehog (Property, (===))
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))

import           Cardano.CLI.Shelley.Run.Query
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified Test.Base as H
import           Test.Process (execCreateScriptContext, execCreateScriptContext')
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


  scriptDummyRedeemer <- H.note $ work </> "mint-script-context-dummy.redeemer"
  scriptContextRedeemer <- H.note $ work </> "mint-script-context.redeemer"

  -- Plutus related
  plutusStakingScript <- H.note $ base </> "scripts/plutus/scripts/guess-42-stake.plutus"
  scriptPaymentAddressWithStaking <- H.execCli [ "address", "build"
                                               , "--payment-verification-key-file", utxoVKeyFile
                                               , "--stake-script-file",  plutusStakingScript
                                               , "--testnet-magic", show @Int testnetMagic
                                               ]
  -- Stake pool related
  poolownerstakekey <- H.note $ tempAbsPath "addresses/pool-owner1-stake.vkey"
  poolownerverkey <- H.note $ tempAbsPath "addresses/pool-owner1.vkey"
  poolownerstakeaddr <- H.execCli [ "stake-address", "build"
                                  , "--stake-verification-key-file", poolownerstakekey
                                  , "--testnet-magic", show @Int testnetMagic
                                  ]

  poolowneraddresswstakecred <- H.execCli [ "address", "build"
                                          , "--payment-verification-key-file", poolownerverkey
                                          , "--stake-verification-key-file",  poolownerstakekey
                                          , "--testnet-magic", show @Int testnetMagic
                                          ]
  poolcoldkey <- H.note $ tempAbsPath "node-pool1/shelley/operator.vkey"

  -- REGISTER STAKE POOL

  -- Create stake pool registration certificate
  H.execCli [ "stake-address", "registration-certificate"
            , "--stake-verification-key-file", poolownerstakekey
            , "--out-file", work </> "pledger.regcert"
            ]

  H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address",  utxoAddr
    , "--tx-in", "$txin"
    , "--tx-out", scriptPaymentAddressWithStaking </> "+" </> show @Int 5000000
    , "--tx-out", poolowneraddresswstakecred </> "+" </> show @Int 5000000
    , "--tx-out", utxoaddrwithstaking </> "+" </> show @Int 5000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "pledger.regcert"
    , "--out-file", work </> "pledge-registration-cert.txbody"
    ]

  H.execCli [ "transaction", "sign"
            , "--tx-body-file", work </> "pledge-registration-cert.txbody"
            , "--testnet-magic", show @Int testnetMagic
            , "--signing-key-file", utxoSKeyFile
            , "--out-file" work </> "pledge-registration-cert.tx"
            ]

  noteM_ "Submitting pool owner/pledge stake registration cert and funding stake pool owner address..."

  H.execCli' execConfig
               [ "transaction", "submit"
               , "--tx-file", work </> "pledge-registration-cert.tx"
               , "--testnet-magic", show @Int testnetMagic
               ]

  -- Wait 10 seconds
  H.threadDelay 1000000

  -- Check to see if stake pool was registered

  H.execCli' execConfig [ "query",  "stake-address-info"
                        , "--address", poolownerstakeaddr
                        , "--testnet-magic", show @Int testnetMagic
                        , "--out-file", work </> "pledgeownerregistration.json"
                        ]

  stakePoolInfo <- H.leftFailM . H.readJsonFile $ work </> "pledgeownerregistration.json"
  delegsAndRewardsMap <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards stakePoolInfo
  let delegsAndRewards = mergeDelegsAndRewards delegsAndRewardsMap

  length delegsAndRewards == 1

  let (poolSAddr, rewards, poolId) = head delegsAndRewards

  poolSAddr === T.unpack $ serialiseAddress poolownerstakeaddr

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
