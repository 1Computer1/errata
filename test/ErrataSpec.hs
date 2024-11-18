{-# LANGUAGE OverloadedStrings #-}

module ErrataSpec
    ( spec
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Errata
import           Errata.Styles
import           Errata.Types
import           Test.Hspec
import           Test.Hspec.Golden (Golden(..))

spec :: Spec
spec = do
    describe "blockMerged" $ do
        it "merges pointers on the same line" $
            let b = blockMerged basicStyle basicPointer "here" Nothing (1, 1, 2, Just "a") (1, 3, 4, Just "b") (Just "c") Nothing
                pointers = [Pointer 1 1 4 False (Just "c") basicPointer]
            in pointerData <$> blockPointers b `shouldBe` pointerData <$> pointers

        it "does not merge pointers on different line" $
            let b = blockMerged basicStyle basicPointer "here" Nothing (1, 1, 2, Just "a") (2, 3, 4, Just "b") (Just "c") Nothing
                pointers = [Pointer 1 1 2 True (Just "a") basicPointer, Pointer 2 3 4 True (Just "b") basicPointer]
            in pointerData <$> blockPointers b `shouldBe` pointerData <$> pointers

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
                [Pointer 1 1 6 False Nothing basicPointer]
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
                [ Pointer 2 10 13 False (Just "this has type `Int`") basicPointer
                , Pointer 3 10 17 False (Just "but this has type `String`") basicPointer
                ]
                Nothing
            , Block
                basicStyle
                ("file.hs", 1, 7)
                Nothing
                [ Pointer 1 7 9 True Nothing basicPointer
                , Pointer 2 5 9 True Nothing basicPointer
                , Pointer 3 5 9 True (Just "in this `if` expression") basicPointer
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
                [Pointer 1 10 14 False Nothing basicPointer]
                Nothing
            ]
            (Just "\nDid you mean to use one of these?\n\n    foldl\n    foldr")
        ]

    golden
        "T003"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 False (Just "label") basicPointer
            ]
        ]

    golden
        "T004"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 False (Just "x") basicPointer
            , Pointer 1 6 8 False (Just "y") basicPointer
            , Pointer 1 10 12 False (Just "z") basicPointer
            , Pointer 2 5 8 False (Just "w") basicPointer
            ]
        ]

    golden
        "T005"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x") basicPointer
            , Pointer 1 6 8 True (Just "y") basicPointer
            , Pointer 1 10 12 True (Just "z") basicPointer
            , Pointer 2 5 8 True (Just "w") basicPointer
            ]
        ]

    golden
        "T006"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x") basicPointer
            , Pointer 1 6 8 False (Just "y") basicPointer
            , Pointer 1 10 12 True (Just "z") basicPointer
            , Pointer 2 5 8 False (Just "w") basicPointer
            ]
        ]

    golden
        "T007"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x") basicPointer
            , Pointer 1 6 8 True Nothing basicPointer
            , Pointer 1 10 12 False (Just "z") basicPointer
            , Pointer 2 5 8 False (Just "w") basicPointer
            , Pointer 3 1 3 True (Just "v") basicPointer
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
            [ Pointer 4 1 2 False (Just "empty") basicPointer
            ]
        ]

    golden
        "T010"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 1 False (Just "empty") basicPointer
            ]
        ]

    golden
        "T011"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 15 16 False (Just "empty") basicPointer
            ]
        ]

    golden
        "T012"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 2 True (Just "x") basicPointer
            , Pointer 1 3 4 True (Just "y") basicPointer
            , Pointer 1 5 6 False (Just "z") basicPointer
            , Pointer 1 7 8 False (Just "z") basicPointer
            , Pointer 1 9 10 False (Just "z") basicPointer
            , Pointer 2 5 8 False (Just "w") basicPointer
            , Pointer 3 1 3 True (Just "v") basicPointer
            ]
        ]

    golden
        "T013"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 2 True (Just "x") basicPointer
            , Pointer 1 3 4 True (Just "y") basicPointer
            , Pointer 1 7 8 True Nothing basicPointer
            , Pointer 1 9 10 True (Just "z") basicPointer
            , Pointer 2 5 8 False (Just "w") basicPointer
            , Pointer 3 1 3 True (Just "v") basicPointer
            ]
        ]

    golden
        "T014"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 4 True (Just "x") basicPointer
            , Pointer 1 6 9 True (Just "x") basicPointer
            , Pointer 3 1 3 True (Just "y") basicPointer
            , Pointer 3 5 7 True (Just "y") basicPointer
            ]
        ]

    golden
        "T015"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 3 False Nothing basicPointer
            , Pointer 1 5 6 False Nothing basicPointer
            , Pointer 1 7 9 False (Just "x") basicPointer
            ]
        ]

    golden
        "T016"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 1 3 True Nothing basicPointer
            , Pointer 1 5 6 True Nothing basicPointer
            , Pointer 1 7 9 True (Just "x") basicPointer
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
            [ Pointer 2 2 3 False Nothing basicPointer
            ]
        ]

    golden
        "T023"
        "こんにちは、日本語です"
        [ adhoc
            [ Pointer 1 1 6 False Nothing basicPointer
            ]
        ]

    golden
        "T024"
        "jalapeño poppers"
        [ adhoc
            [ Pointer 1 2 4 False Nothing basicPointer
            , Pointer 1 7 9 False Nothing basicPointer
            , Pointer 1 12 14 False Nothing basicPointer
            ]
        ]

    golden
        "T025"
        "bar\t\t\t.foo"
        [ adhoc
            [ Pointer 1 1 11 False Nothing basicPointer
            ]
        ]

    golden
        "T026"
        "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8"
        [ adhoc
            [ Pointer 1 1 3 False Nothing basicPointer
            , Pointer 7 1 3 False Nothing basicPointer
            ]
        ]

    golden
        "T027"
        "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8"
        [ adhoc
            [ Pointer 1 1 3 True Nothing basicPointer
            , Pointer 7 1 3 True Nothing basicPointer
            ]
        ]

    golden
        "T028"
        "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8"
        [ adhoc
            [ Pointer 1 1 3 True (Just "label") basicPointer
            , Pointer 7 1 3 True Nothing basicPointer
            ]
        ]

    golden
        "T029"
        "line 1 foo bar do\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8 baz end"
        [ Errata
            (Just "error header message")
            [ Block
                basicStyle
                ("file.ext", 1, 16)
                (Just "block header message")
                [ Pointer 1 16 18 True (Just "start label") basicPointer
                , Pointer 2 6 7 False (Just "unconnected label") basicPointer
                , Pointer 3 6 7 True (Just "middle label") basicPointer
                , Pointer 8 6 7 True (Just "inner label") basicPointer
                , Pointer 8 12 15 True (Just "end label") basicPointer
                ]
                (Just "block body message")
            ]
            (Just "error body message")
        ]

    golden
        "T030"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 False (Just "x") (basicPointer { styleHook = "1", styleUnderline = "." })
            , Pointer 1 6 8 False (Just "y") (basicPointer { styleHook = "2", styleConnector = ":", styleUnderline = "~" })
            , Pointer 1 10 12 False (Just "z") (basicPointer { styleUnderline = "^" })
            , Pointer 2 5 8 False (Just "w") (basicPointer { styleUnderline = "'" })
            ]
        ]

    golden
        "T031"
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 False (Just "x") (basicPointer { styleEnableHook = False })
            , Pointer 1 6 8 False (Just "y") (basicPointer { styleEnableHook = False })
            , Pointer 1 10 12 False (Just "z") (basicPointer { styleEnableHook = False })
            ]
        ]

    golden
        "T032"
        "line 1 foo bar do\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8 baz end"
        [ Errata
            (Just "error header message")
            [ Block
                (basicStyle { styleExtraLinesAfter = 0, styleExtraLinesBefore = 0 })
                ("file.ext", 1, 16)
                (Just "block header message")
                [ Pointer 1 16 18 True (Just "start label") basicPointer
                , Pointer 2 6 7 False (Just "unconnected label") basicPointer
                , Pointer 3 6 7 True (Just "middle label") basicPointer
                , Pointer 8 6 7 True (Just "inner label") basicPointer
                , Pointer 8 12 15 True (Just "end label") basicPointer
                ]
                (Just "block body message")
            ]
            (Just "error body message")
        ]

    golden
        "T033"
        "line 1 foo bar do\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8 baz end"
        [ Errata
            (Just "error header message")
            [ Block
                (basicStyle { styleExtraLinesAfter = 2, styleExtraLinesBefore = 2 })
                ("file.ext", 1, 16)
                (Just "block header message")
                [ Pointer 1 16 18 True (Just "start label") basicPointer
                , Pointer 2 6 7 False (Just "unconnected label") basicPointer
                , Pointer 3 6 7 True (Just "middle label") basicPointer
                , Pointer 8 6 7 True (Just "inner label") basicPointer
                , Pointer 8 12 15 True (Just "end label") basicPointer
                ]
                (Just "block body message")
            ]
            (Just "error body message")
        ]

    golden
        "T034"
        "line 1 foo bar do\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8 baz end"
        [ Errata
            (Just "error header message")
            [ Block
                (basicStyle { styleExtraLinesAfter = 1, styleExtraLinesBefore = 2 })
                ("file.ext", 1, 16)
                (Just "block header message")
                [ Pointer 1 16 18 True (Just "start label") basicPointer
                , Pointer 2 6 7 False (Just "unconnected label") basicPointer
                , Pointer 3 6 7 True (Just "middle label") basicPointer
                , Pointer 8 6 7 True (Just "inner label") basicPointer
                , Pointer 8 12 15 True (Just "end label") basicPointer
                ]
                (Just "block body message")
            ]
            (Just "error body message")
        ]

    golden
        "T035"
        "line 1 foo bar do\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8 baz end"
        [ Errata
            (Just "error header message")
            [ Block
                (basicStyle { styleExtraLinesAfter = 1, styleExtraLinesBefore = 1 })
                ("file.ext", 1, 16)
                (Just "block header message")
                [ Pointer 1 16 18 True (Just "start label") basicPointer
                , Pointer 2 6 7 False (Just "unconnected label") basicPointer
                , Pointer 3 6 7 True (Just "middle label") basicPointer
                , Pointer 8 6 7 True (Just "inner label") basicPointer
                , Pointer 8 12 15 True (Just "end label") basicPointer
                ]
                (Just "block body message")
            ]
            (Just "error body message")
        ]

    golden
        "T036"
        "hello world"
        [ Errata
            (Just "error")
            [ Block
                (basicStyle { stylePaddingTop = False })
                ("simple", 1, 1)
                Nothing
                [Pointer 1 1 6 False Nothing basicPointer]
                Nothing
            ]
            Nothing
        ]

    golden
        "T037"
        "hello world"
        [ Errata
            (Just "error")
            [ Block
                (basicStyle { stylePaddingBottom = True })
                ("simple", 1, 1)
                Nothing
                [Pointer 1 1 6 False Nothing basicPointer]
                Nothing
            ]
            Nothing
        ]

    golden
        "T038"
        "hello world"
        [ Errata
            (Just "error")
            [ Block
                (basicStyle { styleEnableDecorations = False, stylePaddingTop = False })
                ("simple", 1, 1)
                Nothing
                [Pointer 1 1 6 False (Just "ignored") basicPointer]
                Nothing
            ]
            Nothing
        ]

    golden
        "T039"
        "line 1 foo bar do\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8 baz end"
        [ Errata
            (Just "error header message")
            [ Block
                (basicStyle { styleEnableDecorations = False, stylePaddingTop = False })
                ("file.ext", 1, 16)
                (Just "block header message")
                [ Pointer 1 16 18 True (Just "start label") basicPointer
                , Pointer 2 6 7 False (Just "unconnected label") basicPointer
                , Pointer 3 6 7 True (Just "middle label") basicPointer
                , Pointer 8 6 7 True (Just "inner label") basicPointer
                , Pointer 8 12 15 True (Just "end label") basicPointer
                ]
                (Just "block body message")
            ]
            (Just "error body message")
        ]

    golden
        "T040"
        "sum xs = fold (+) 0 xs"
        [ Errata
            (Just "─────── NAME UNKNOWN ───────\n\nThe name fold was not found.\n")
            [ Block
                (basicStyle { styleEnableDecorations = False, stylePaddingTop = False })
                ("file.hs", 1, 10)
                Nothing
                [Pointer 1 10 14 False Nothing basicPointer]
                Nothing
            ]
            (Just "\nDid you mean to use one of these?\n\n    foldl\n    foldr")
        ]

    golden
        "T041"
        "hello world"
        [ Errata
            (Just "error")
            [ Block
                (basicStyle { styleEnableLinePrefix = False })
                ("simple", 1, 1)
                Nothing
                [Pointer 1 1 6 False (Just "ignored") basicPointer]
                Nothing
            ]
            Nothing
        ]

    golden
        "T042"
        "line 1 foo bar do\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8 baz end"
        [ Errata
            (Just "error header message")
            [ Block
                (basicStyle { styleEnableLinePrefix = False })
                ("file.ext", 1, 16)
                (Just "block header message")
                [ Pointer 1 16 18 True (Just "start label") basicPointer
                , Pointer 2 6 7 False (Just "unconnected label") basicPointer
                , Pointer 3 6 7 True (Just "middle label") basicPointer
                , Pointer 8 6 7 True (Just "inner label") basicPointer
                , Pointer 8 12 15 True (Just "end label") basicPointer
                ]
                (Just "block body message")
            ]
            (Just "error body message")
        ]

    golden
        "T043"
        "sum xs = fold (+) 0 xs"
        [ Errata
            (Just "─────── NAME UNKNOWN ───────\n\nThe name fold was not found.\n")
            [ Block
                (basicStyle { styleEnableLinePrefix = False })
                ("file.hs", 1, 10)
                Nothing
                [Pointer 1 10 14 False Nothing basicPointer]
                Nothing
            ]
            (Just "\nDid you mean to use one of these?\n\n    foldl\n    foldr")
        ]

-- | Create a golden test by printing a list of 'Errata'.
golden :: String -> T.Text -> [Errata] -> Spec
golden name source es = it name $ Golden
    { output = TL.toStrict $ prettyErrors source es
    , encodePretty = T.unpack
    , writeToFile = T.writeFile
    , readFromFile = T.readFile
    , goldenFile = "./test/.golden/" <> name <> "/golden"
    , actualFile = Just ("./test/.golden/" <> name <> "/actual")
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
