{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module AuthNFTIssuer(issuerCS, issuerEndpoints, AuthNFTIssuerSchema) where

import           Control.Lens         (view)
import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map hiding (empty)
import           Data.Maybe                  (fromJust)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Ledger                 hiding (getAccessToken, singleton)
import Ledger.Ada (lovelaceValueOf)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.Contract
import           Plutus.Contract        as Contract
import           Plutus.Contract.Request     as Request
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Text.Printf            (printf)
import           Wallet.Emulator.Types (Wallet (..))
import           Utils

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pubKH () ctx = txSignedBy (scriptContextTxInfo ctx) pubKH

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pubKH = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pubKH

issuerCS :: PubKeyHash -> CurrencySymbol
issuerCS = scriptCurrencySymbol . policy

type AuthNFTIssuerSchema =
                  Endpoint "getAccessToken" PaymentPubKeyHash
              .\/ Endpoint "inspect" String  {-Argument can be just () instead of String. Not done due to want of time-}
              .\/ Endpoint "logWalletNftTokenName" ()

getAccessToken :: forall w s e. AsContractError e => PaymentPubKeyHash -> Contract w s e ()
getAccessToken reqPkh = do
    pkh <- Request.ownPaymentPubKeyHash
    oref <- getUnspentOutput
    Contract.logInfo @String $ printf "picked UTxO %s" (show oref)
    o    <- fromJust <$> Contract.unspentTxOutFromRef oref
    Contract.logDebug @String $ printf "picked UTxO at %s with value %s" (show oref) (show $ _ciTxOutValue o)
    let val     = Value.singleton (issuerCS $ unPaymentPubKeyHash pkh) (TokenName $ (getPubKeyHash . unPaymentPubKeyHash) reqPkh) 1
        lookups = Constraints.mintingPolicy (policy (unPaymentPubKeyHash pkh))
                  <> Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustMintValue val
                  <> Constraints.mustSpendPubKeyOutput oref
                  <> Constraints.mustPayToPubKey reqPkh (val <> lovelaceValueOf 2000000)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ adjustAndSubmitWith @Void lookups tx
    logInfo @String $ printf "minted %s" (show val)

inspect :: forall w s e. AsContractError e => String -> Contract w s e ()
inspect _ = do
    logInfo @String $ "Inspecting own utxos."
    pkh  <- Request.ownPaymentPubKeyHash
    os  <- map snd . Map.toList <$> utxosAt (pubKeyHashAddress pkh Nothing)
    let totalVal = mconcat [view ciTxOutValue o | o <- os]
    logInfo @String
            $ "Logging total Value : " <> show totalVal
    logInfo @String $ "Inspect complete"

logWalletNftTokenName :: forall w s e. AsContractError e => () -> Contract w s e ()
logWalletNftTokenName _ = do
    pkh <- Request.ownPaymentPubKeyHash
    let tn = TokenName $ (getPubKeyHash . unPaymentPubKeyHash) pkh
    logInfo @String
            $ "Logging own nft token name : " <> show tn
    logInfo @String $ "logWalletNftTokenName complete"

getAccessToken' :: Promise () AuthNFTIssuerSchema Text ()
getAccessToken' = endpoint @"getAccessToken" getAccessToken

inspect' :: Promise () AuthNFTIssuerSchema Text ()
inspect' = endpoint @"inspect" inspect

logWalletNftTokenName' :: Promise () AuthNFTIssuerSchema Text ()
logWalletNftTokenName' = endpoint @"logWalletNftTokenName" logWalletNftTokenName

issuerEndpoints :: Contract () AuthNFTIssuerSchema Text ()
issuerEndpoints = do
    logInfo @String "Waiting for request."
    selectList [getAccessToken', inspect', logWalletNftTokenName'] >>  issuerEndpoints

mkSchemaDefinitions ''AuthNFTIssuerSchema
mkKnownCurrencies []
