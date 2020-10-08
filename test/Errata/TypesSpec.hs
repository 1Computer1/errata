{-# LANGUAGE OverloadedStrings #-}

module Errata.TypesSpec
    ( spec
    ) where

import Errata.Types
import Test.Hspec

spec :: Spec
spec = do
    describe "pointerColumns" $ do
        it "gets the columns" $
            pointerColumns (Pointer 1 2 3 False Nothing) `shouldBe` (2, 3)

    describe "highlight" $ do
        it "brackets parts of text" $
            highlight "(" ")" [(2, 4), (6, 7)] "12345678" `shouldBe` "1(23)45(6)78"

        it "brackets empty column spans" $
            highlight "(" ")" [(1, 1)] "1234" `shouldBe` "()1234"

        it "does nothing with no columns" $
            highlight "(" ")" [] "123" `shouldBe` "123"

        it "puts text for out of bounds columns at the end" $
            highlight "(" ")" [(3, 5), (6, 7)] "1234" `shouldBe` "12(34)()"
