{-# LANGUAGE OverloadedStrings #-}

module Errata.TypesSpec
    ( spec
    ) where

import Errata.Styles
import Errata.Types
import Test.Hspec

spec :: Spec
spec = do
    describe "pointerColumns" $ do
        it "gets the columns" $
            pointerColumns (Pointer 1 2 3 False Nothing basicPointer) `shouldBe` (2, 3)
