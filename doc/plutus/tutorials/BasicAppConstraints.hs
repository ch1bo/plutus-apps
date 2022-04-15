{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module BasicAppConstraints where

-- BLOCK0

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ledger (Ada, PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (ScriptContext, scriptContextTxInfo),
               valuePaidTo)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, Promise, collectFromScript, endpoint, logInfo, selectList,
                        submitTxConstraints, submitTxConstraintsSpending, type (.\/), utxosAt)
import PlutusTx qualified
import PlutusTx.Prelude (Bool, Semigroup ((<>)), ($), (&&), (-), (.), (>=))
import Prelude qualified as Haskell
import Schema (ToSchema)
import Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash)

-- tag::BLOCK1[]

data SplitData =
    SplitData
        { recipient1 :: PaymentPubKeyHash -- ^ First recipient of the funds
        , recipient2 :: PaymentPubKeyHash -- ^ Second recipient of the funds
        , amount     :: Ada -- ^ How much Ada we want to lock
        }
    deriving stock (Haskell.Show, Generic)

-- For a 'real' application use 'makeIsDataIndexed' to ensure the output is stable over time
PlutusTx.unstableMakeIsData ''SplitData
PlutusTx.makeLift ''SplitData

-- end::BLOCK1[]
-- tag::BLOCK2[]

-- Create constraints that will be used to spend a locked transaction output
-- from the script address.
--
-- These constraints will be used in the validation script as well as in the
-- transaction creation step.
{-# INLINABLE splitDataConstraints #-}
splitDataConstraints :: SplitData -> TxConstraints i o
splitDataConstraints SplitData{recipient1, recipient2, amount} =
     Constraints.mustPayToPubKey recipient1 (Ada.toValue half)
  <> Constraints.mustPayToPubKey recipient2 (Ada.toValue $ amount - half)
 where
     half = Ada.divide amount 2

{-# INLINABLE validateSplit #-}
validateSplit :: SplitData -> () -> ScriptContext -> Bool
validateSplit splitData _ = checkScriptContext splitDataConstraints

-- end::BLOCK2[]
-- tag::BLOCK3[]

data Split
instance Scripts.ValidatorTypes Split where
    type instance RedeemerType Split = ()
    type instance DatumType Split = SplitData

{-# INLINABLE splitValidator #-}
splitValidator :: Scripts.TypedValidator Split
splitValidator = Scripts.mkTypedValidator @Split
    $$(PlutusTx.compile [|| validateSplit ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @SplitData @()

-- end::BLOCK3[]
-- tag::BLOCK4[]

data LockArgs =
        LockArgs
            { recipient1Wallet :: Wallet
            , recipient2Wallet :: Wallet
            , totalAda         :: Ada
            }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type SplitSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "unlock" LockArgs

-- end::BLOCK4[]
-- tag::BLOCK5[]

lock :: Promise () SplitSchema T.Text ()
lock = endpoint @"lock" (lockFunds . mkSplitData)

unlock :: Promise () SplitSchema T.Text ()
unlock = endpoint @"unlock" (unlockFunds . mkSplitData)

-- end::BLOCK5[]
-- tag::BLOCK6[]

mkSplitData :: LockArgs -> SplitData
mkSplitData LockArgs{recipient1Wallet, recipient2Wallet, totalAda} =
    SplitData
        { recipient1 = mockWalletPaymentPubKeyHash recipient1Wallet
        , recipient2 = mockWalletPaymentPubKeyHash recipient2Wallet
        , amount = totalAda
        }

-- end::BLOCK6[]
-- tag::BLOCK7[]

lockFunds :: SplitData -> Contract () SplitSchema T.Text ()
lockFunds s@SplitData{amount} = do
    logInfo $ "Locking " <> Haskell.show amount
    let tx = Constraints.mustPayToTheScript s (Ada.toValue amount)
    void $ submitTxConstraints splitValidator tx

-- end::BLOCK7[]
-- tag::BLOCK8[]

unlockFunds :: SplitData -> Contract () SplitSchema T.Text ()
unlockFunds splitData = do
    let contractAddress = Scripts.validatorAddress splitValidator
    utxos <- utxosAt contractAddress
    let tx = collectFromScript utxos () <> splitDataConstraints splitData
    void $ submitTxConstraintsSpending splitValidator utxos tx

-- end::BLOCK8[]
-- tag::BLOCK9[]

endpoints :: Contract () SplitSchema T.Text ()
-- end::BLOCK9[]
-- tag::BLOCK10[]

endpoints = selectList [lock, unlock]

-- end::BLOCK10[]
