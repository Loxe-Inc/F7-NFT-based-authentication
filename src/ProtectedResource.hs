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

module ProtectedResource(checkEndpoints, ProtectedResourceSchema, CheckArg(..)) where

import           Control.Lens           (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           AuthNFTIssuer      as I hiding (issuerEndpoints)
import           Plutus.Contract
import           Plutus.Contract.Request     as Request
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Wallet.Emulator.Wallet (Wallet)

data CheckArg = CheckArg
    { issuerWalletPkh :: !PaymentPubKeyHash
    , clientWalletPkh :: !PaymentPubKeyHash
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
    
type ProtectedResourceSchema =
                  Endpoint "checkAccess" CheckArg

checkAccess :: forall w s e. AsContractError e => CheckArg -> Contract w s e ()
checkAccess arg = do
    pkh <- Request.ownPaymentPubKeyHash
    let iPkh = issuerWalletPkh arg
        cPkh = clientWalletPkh arg
    os  <- map snd . Map.toList <$> utxosAt (pubKeyHashAddress cPkh Nothing)
    let accessToken = mconcat [view ciTxOutValue o | o <- os, nf (view ciTxOutValue o) iPkh cPkh]
        qty = assetClassValueOf
                (accessToken)
                (AssetClass ( I.issuerCS (unPaymentPubKeyHash iPkh)
                            , TokenName $ (getPubKeyHash . unPaymentPubKeyHash) cPkh)
                )
    logInfo @String $ "Total value at client wallet" <> (show accessToken)
    logInfo @String $ "RESULT - ACCESS " ++ (if qty == 0 then "DENIED" else "GRANTED")
    where
      nf val iPkh cPkh = assetClassValueOf
                          (val)
                          (AssetClass ( I.issuerCS (unPaymentPubKeyHash iPkh)
                                      , TokenName $ (getPubKeyHash . unPaymentPubKeyHash) cPkh))
                         >= 1

checkAccess' :: Promise () ProtectedResourceSchema Text ()
checkAccess' = endpoint @"checkAccess" checkAccess

checkEndpoints :: Contract () ProtectedResourceSchema Text ()
checkEndpoints = do
    logInfo @String "Waiting for request."
    selectList [checkAccess'] >>  checkEndpoints

mkSchemaDefinitions ''ProtectedResourceSchema
mkKnownCurrencies []
