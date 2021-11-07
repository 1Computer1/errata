{-# LANGUAGE OverloadedStrings #-}

module ErrataSpec
    ( spec
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Errata
import           Test.Hspec
import           Test.Hspec.Golden

spec :: Spec
spec = do
    describe "blockMerged" $ do
        it "merges pointers on the same line" $
            let b = blockMerged basicStyle "here" Nothing (1, 1, 2, Just "a") (1, 3, 4, Just "b") (Just "c") Nothing
                pointers = [Pointer 1 1 4 False (Just "c")]
            in blockPointers b `shouldBe` pointers

        it "does not merge pointers on different line" $
            let b = blockMerged basicStyle "here" Nothing (1, 1, 2, Just "a") (2, 3, 4, Just "b") (Just "c") Nothing
                pointers = [Pointer 1 1 2 True (Just "a"), Pointer 2 3 4 True (Just "b")]
            in blockPointers b `shouldBe` pointers

    describe "prettyErrors" goldenTests

goldenTests :: Spec
goldenTests = do
    golden
        "T000"
        "hello world"
        [ Errata
            (Just "error")
            [ Block
                basicStyle
                ("simple", 1, 1)
                Nothing
                [Pointer 1 1 6 False Nothing]
                Nothing
            ]
            Nothing
        ]

    golden
        "T001"
        "foo = if 1 > 2\n    then 100\n    else \"uh oh\""
        [ Errata
            (Just "error[E001]: mismatching types in `if` expression")
            [ Block
                basicStyle
                ("file.hs", 3, 10)
                Nothing
                [ Pointer 2 10 13 False (Just "this has type `Int`")
                , Pointer 3 10 17 False (Just "but this has type `String`")
                ]
                Nothing
            , Block
                basicStyle
                ("file.hs", 1, 7)
                Nothing
                [ Pointer 1 7 9 True Nothing
                , Pointer 2 5 9 True Nothing
                , Pointer 3 5 9 True (Just "in this `if` expression")
                ]
                Nothing
            ]
            (Just "\nnote: use --explain E001 to learn more")
        ]

    golden
        "T002"
        "sum xs = fold (+) 0 xs"
        [ Errata
            (Just "─────── NAME UNKNOWN ───────\n\nThe name fold was not found.\n")
            [ Block
                basicStyle
                ("file.hs", 1, 10)
                Nothing
                [Pointer 1 10 14 False Nothing]
                Nothing
            ]
            (Just "\nDid you mean to use one of these?\n\n    foldl\n    foldr")
        ]

    golden
        "T003"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 False (Just "label")
            ]
        ]

    golden
        "T004"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 False (Just "x")
            , Pointer 1 6 8 False (Just "y")
            , Pointer 1 10 12 False (Just "z")
            , Pointer 2 5 8 False (Just "w")
            ]
        ]

    golden
        "T005"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x")
            , Pointer 1 6 8 True (Just "y")
            , Pointer 1 10 12 True (Just "z")
            , Pointer 2 5 8 True (Just "w")
            ]
        ]

    golden
        "T006"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x")
            , Pointer 1 6 8 False (Just "y")
            , Pointer 1 10 12 True (Just "z")
            , Pointer 2 5 8 False (Just "w")
            ]
        ]

    golden
        "T007"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x")
            , Pointer 1 6 8 True Nothing
            , Pointer 1 10 12 False (Just "z")
            , Pointer 2 5 8 False (Just "w")
            , Pointer 3 1 3 True (Just "v")
            ]
        ]

    golden
        "T008"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [
            ]
        ]

    golden
        "T009"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 4 1 2 False (Just "empty")
            ]
        ]

    golden
        "T010"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 1 False (Just "empty")
            ]
        ]

    golden
        "T011"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 15 16 False (Just "empty")
            ]
        ]

    golden
        "T012"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 2 True (Just "x")
            , Pointer 1 3 4 True (Just "y")
            , Pointer 1 5 6 False (Just "z")
            , Pointer 1 7 8 False (Just "z")
            , Pointer 1 9 10 False (Just "z")
            , Pointer 2 5 8 False (Just "w")
            , Pointer 3 1 3 True (Just "v")
            ]
        ]

    golden
        "T013"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 2 True (Just "x")
            , Pointer 1 3 4 True (Just "y")
            , Pointer 1 7 8 True Nothing
            , Pointer 1 9 10 True (Just "z")
            , Pointer 2 5 8 False (Just "w")
            , Pointer 3 1 3 True (Just "v")
            ]
        ]

    golden
        "T014"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 4 True (Just "x")
            , Pointer 1 6 9 True (Just "x")
            , Pointer 3 1 3 True (Just "y")
            , Pointer 3 5 7 True (Just "y")
            ]
        ]

    golden
        "T015"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 3 False Nothing
            , Pointer 1 5 6 False Nothing
            , Pointer 1 7 9 False (Just "x")
            ]
        ]

    golden
        "T016"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 3 True Nothing
            , Pointer 1 5 6 True Nothing
            , Pointer 1 7 9 True (Just "x")
            ]
        ]

    golden
        "T017"
        "foo\nbar"
        []

    golden
        "T018"
        "foo\nbar"
        [ Errata (Just "header") [] (Just "body")
        ]

    golden
        "T019"
        "foo\nbar"
        [ Errata Nothing [] Nothing
        ]

    golden
        "T020"
        "foo\nbar"
        [ Errata
            (Just "header")
            [ Block basicStyle ("here", 1, 1) (Just "block header") [] (Just "block body")
            ]
            (Just "body")
        ]

    golden
        "T021"
        "foo\nbar"
        [ Errata
            Nothing
            [ Block basicStyle ("here", 1, 1) (Just "block header") [] (Just "block body")
            ]
            Nothing
        ]

    golden
        "T022"
        "foo\n\tbar"
        [ adhoc
            [ Pointer 2 2 3 False Nothing
            ]
        ]

    golden
        "T023"
        "こんにちは、日本語です"
        [ adhoc
            [ Pointer 1 1 6 False Nothing
            ]
        ]

    golden
        "T024"
        "jalapeño poppers"
        [ adhoc
            [ Pointer 1 2 4 False Nothing
            , Pointer 1 7 9 False Nothing
            , Pointer 1 12 14 False Nothing
            ]
        ]

    golden
        "T025"
        "bar\t\t\t.foo"
        [ adhoc
            [ Pointer 1 1 11 False Nothing
            ]
        ]

-- | Create a golden test by printing a list of 'Errata'.
golden :: String -> T.Text -> [Errata] -> Spec
golden name source es = it name $ Golden
    { output = TL.toStrict $ prettyErrors source es
    , encodePretty = T.unpack
    , testName = name
    , writeToFile = T.writeFile
    , readFromFile = T.readFile
    , directory = "./test/.golden"
    , failFirstTime = False
    }

-- | Usually, the meat of the work is in just printing one block. This makes an 'Errata' out of a bunch of pointers.
adhoc :: [Pointer] -> Errata
adhoc ps = Errata
    (Just "an error")
    [ Block
        basicStyle
        ("here", 1, 1)
        Nothing
        ps
        (Just "an error occurred here")
    ]
    Nothing
