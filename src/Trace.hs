{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Trace where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras                   as Extras
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, String, show)
import           Wallet.Emulator.Wallet
import           Test.Tasty
import qualified Test.Tasty.HUnit                             as HUnit
import           Data.Monoid                                  (Last (..))
import           Plutus.Contract.Trace      as X

import           AuthNFTIssuer
import           ProtectedResource
import           Data.Text              (Text)

testContract :: IO ()
testContract = runEmulatorTraceIO nftAuthTrace

issuer, alice, checker :: Wallet
issuer = X.knownWallet 1
checker = X.knownWallet 2
alice = X.knownWallet 3
bob = X.knownWallet 4

nftAuthTrace :: EmulatorTrace ()
nftAuthTrace = do
    h1 <- activateContractWallet issuer issuerEndpoints
    h2 <- activateContractWallet issuer checkEndpoints
    let pkhIssuer = mockWalletPaymentPubKeyHash issuer
    let pkhAlice = mockWalletPaymentPubKeyHash alice
    let pkhBob= mockWalletPaymentPubKeyHash bob
    callEndpoint @"getAccessToken" h1 pkhAlice
    void $ Emulator.waitNSlots 2
    let checkArgs = CheckArg
                      { issuerWalletPkh = pkhIssuer
                      , clientWalletPkh = pkhAlice
                      }
    callEndpoint @"checkAccess" h2 checkArgs
    void $ Emulator.waitNSlots 2
    let checkArgs2 = checkArgs { clientWalletPkh = pkhBob}
    callEndpoint @"checkAccess" h2 checkArgs2
    void $ Emulator.waitNSlots 2