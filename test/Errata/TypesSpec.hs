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

    describe "highlight" $ do
        it "brackets parts of text" $
            highlight [(bracketPointer, (2, 4)), (bracketPointer, (6, 7))] "12345678" `shouldBe` "1(23)45(6)78"

        it "brackets empty column spans" $
            highlight [(bracketPointer, (1, 1))] "1234" `shouldBe` "()1234"

        it "does nothing with no columns" $
            highlight [] "123" `shouldBe` "123"

        it "puts text for out of bounds columns at the end" $
            highlight [(bracketPointer, (3, 5)), (bracketPointer, (6, 7))] "1234" `shouldBe` "12(34)()"

bracketPointer :: PointerStyle
bracketPointer = basicPointer { styleHighlight = \x -> "(" <> x <> ")" }
