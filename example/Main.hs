{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import           Errata

-- An ad-hoc converter.
adhoc :: [Pointer] -> Convert T.Text ()
adhoc ps = Convert
    { convertError = \() -> errataSimple "an error"
        (Block fancyRedStyle ("here", 1, 1) ps (Just "pbody"))
    , convertLines = T.lines
    , convertLine = id
    }

-- Definitions from the Haddock example for 'prettyErrors'.
data ParseError = ParseError
    { peFile       :: FilePath
    , peLine       :: Int
    , peColStart   :: Int
    , peColEnd     :: Int
    , peUnexpected :: T.Text
    , peExpected   :: [T.Text]
    }

converter :: Convert T.Text ParseError
converter = Convert
   { convertError = \(ParseError fp l cs ce unexpected expected) ->
       errataSimple
           "error: invalid syntax"
           (blockSimple basicStyle fp l (cs, ce)
               ("unexpected " <> unexpected <> "\nexpected " <> T.intercalate ", " expected))
   , convertLines = T.lines
   , convertLine = id
   }

printErrors :: T.Text -> N.NonEmpty ParseError -> IO ()
printErrors source es = TL.putStrLn $ prettyErrors converter source es

main :: IO ()
main = do
    putStrLn "Simple single pointer:"
    putStrLn ""
    TL.putStrLn $ prettyErrors' (adhoc
        [ Pointer 1 2 4 False (Just "x")
        ])
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [()]
    putStrLn ""

    putStrLn "Nothing is connected, some inner labels:"
    putStrLn ""
    TL.putStrLn $ prettyErrors' (adhoc
        [ Pointer 1 2 4 False (Just "x")
        , Pointer 1 6 8 False (Just "y")
        , Pointer 1 10 12 False (Just "z")
        , Pointer 2 5 8 False (Just "w")
        ])
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [()]
    putStrLn ""

    putStrLn "Everything is connected."
    putStrLn ""
    TL.putStrLn $ prettyErrors' (adhoc
        [ Pointer 1 2 4 True (Just "x")
        , Pointer 1 6 8 True (Just "y")
        , Pointer 1 10 12 True (Just "z")
        , Pointer 2 5 8 True (Just "w")
        ])
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [()]
    putStrLn ""

    putStrLn "Only one line is connected, and one of them is skewered through:"
    putStrLn ""
    TL.putStrLn $ prettyErrors' (adhoc
        [ Pointer 1 2 4 True (Just "x")
        , Pointer 1 6 8 False (Just "y")
        , Pointer 1 10 12 True (Just "z")
        , Pointer 2 5 8 False (Just "w")
        ])
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [()]
    putStrLn ""

    putStrLn "Everything is connected except for 2. One of them does not have a label:"
    putStrLn ""
    TL.putStrLn $ prettyErrors' (adhoc
        [ Pointer 1 2 4 True (Just "x")
        , Pointer 1 6 8 True Nothing
        , Pointer 1 10 12 False (Just "z")
        , Pointer 2 5 8 False (Just "w")
        , Pointer 3 1 3 True (Just "v")
        ])
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [()]
    putStrLn ""

    putStrLn "Example from the Haddock example for `prettyErrors`:"
    putStrLn ""
    printErrors
        "{\n    \"bad\": [1, 2,]\n    }"
        (pure (ParseError "./comma.json" 2 18 19 "]" ["null", "true", "false", "\"", "-", "digit", "[", "{"]))
    putStrLn ""

    putStrLn "Example from the readme:"
    putStrLn ""
    TL.putStrLn $ prettyErrors' (Convert
        { convertError = \() -> Errata
            (Just "\x1b[31merror[E001]: mismatching types in `if` expression\x1b[0m")
            (Block
                fancyRedStyle
                ("file.hs", 3, 10)
                [ Pointer 2 10 13 False (Just "\x1b[31mthis has type `Int`\x1b[0m")
                , Pointer 3 10 17 False (Just "\x1b[31mbut this has type `String`\x1b[0m")
                ]
                Nothing)
            [ Block
                fancyYellowStyle
                ("file.hs", 1, 7)
                [ Pointer 1 7 9 True Nothing
                , Pointer 2 5 9 True Nothing
                , Pointer 3 5 9 True (Just "\x1b[33min this `if` expression\x1b[0m")
                ]
                Nothing
            ]
            (Just "\x1b[33mnote: use --explain E001 to learn more\x1b[0m")
        , convertLines = T.lines
        , convertLine = id
        })
        "foo = if 1 > 2\n    then 100\n    else \"uh oh\""
        [()]
    putStrLn ""
