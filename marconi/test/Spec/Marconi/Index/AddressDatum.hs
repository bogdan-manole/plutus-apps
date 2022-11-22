module Spec.Marconi.Index.AddressDatum (tests) where

import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Spec.Marconi.Index.AddressDatum.AddressDatumIndex qualified as AddressDatumIndex
import Spec.Marconi.Index.AddressDatum.AddressDatumIndexEvent qualified as AddressDatumIndexEvent
import Spec.Marconi.Index.AddressDatum.E2E qualified as AddressDatumE2E

tests :: TestTree
tests = localOption (HedgehogTestLimit $ Just 200) $
    testGroup "Spec.Marconi.Index.AddressDatum"
    [
      -- AddressDatumIndexEvent.tests
    -- , AddressDatumIndex.tests
      AddressDatumE2E.tests
    ]
