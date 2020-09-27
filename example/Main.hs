{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import           Errata

-------------------------------------------------------------
-- Definitions from the Haddock example for 'prettyErrors' --
-------------------------------------------------------------

data ParseError = ParseError
    { peFile       :: FilePath
    , peLine       :: Int
    , peCol        :: Int
    , peUnexpected :: T.Text
    , peExpected   :: [T.Text]
    }

toErrata :: ParseError -> Errata
toErrata (ParseError fp l c unexpected expected) =
    errataSimple
        (Just "an error occured!")
        (blockSimple basicStyle fp
            (Just "error: invalid syntax")
            (l, c, c + T.length unexpected, Just "this one")
            (Just $ "unexpected " <> unexpected <> "\nexpected " <> T.intercalate ", " expected))
        Nothing

printErrors :: T.Text -> [ParseError] -> IO ()
printErrors source es = TL.putStrLn $ prettyErrors source (toErrata <$> es)

--------------
-- Examples --
--------------

-- An ad-hoc errata.
adhoc :: [Pointer] -> Errata
adhoc ps = errataSimple (Just "an error") (Block fancyRedStyle ("here", 1, 1) Nothing ps (Just "pbody")) Nothing

main :: IO ()
main = do
    putStrLn "Simple single pointer:"
    putStrLn ""
    TL.putStrLn $ prettyErrors @String
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc [Pointer 1 2 4 False (Just "x")]
        ]
    putStrLn ""

    putStrLn "Nothing is connected, some inner labels:"
    putStrLn ""
    TL.putStrLn $ prettyErrors @String
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 False (Just "x")
            , Pointer 1 6 8 False (Just "y")
            , Pointer 1 10 12 False (Just "z")
            , Pointer 2 5 8 False (Just "w")
            ]
        ]
    putStrLn ""

    putStrLn "Everything is connected."
    putStrLn ""
    TL.putStrLn $ prettyErrors @String
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x")
            , Pointer 1 6 8 True (Just "y")
            , Pointer 1 10 12 True (Just "z")
            , Pointer 2 5 8 True (Just "w")
            ]
        ]
    putStrLn ""

    putStrLn "Only one line is connected, and one of them is skewered through:"
    putStrLn ""
    TL.putStrLn $ prettyErrors @String
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x")
            , Pointer 1 6 8 False (Just "y")
            , Pointer 1 10 12 True (Just "z")
            , Pointer 2 5 8 False (Just "w")
            ]
        ]
    putStrLn ""

    putStrLn "Everything is connected except for 2. One of them does not have a label:"
    putStrLn ""
    TL.putStrLn $ prettyErrors @String
        "abcdefghijk\nlmnopqrstuv\nwxyzfoobar"
        [ adhoc
            [ Pointer 1 2 4 True (Just "x")
            , Pointer 1 6 8 True Nothing
            , Pointer 1 10 12 False (Just "z")
            , Pointer 2 5 8 False (Just "w")
            , Pointer 3 1 3 True (Just "v")
            ]
        ]
    putStrLn ""

    putStrLn "Example from the Haddock example for `prettyErrors`:"
    putStrLn ""
    printErrors
        "{\n    \"bad\": [1, 2,]\n    }"
        (pure (ParseError "./comma.json" 2 18 "]" ["null", "true", "false", "\"", "-", "digit", "[", "{"]))
    putStrLn ""

    putStrLn "Example from the readme:"
    putStrLn ""
    TL.putStrLn $ prettyErrors @String
        "foo = if 1 > 2\n    then 100\n    else \"uh oh\""
        [ Errata
            (Just "\x1b[31merror[E001]: mismatching types in `if` expression\x1b[0m")
            (Block
                fancyRedStyle
                ("file.hs", 3, 10)
                Nothing
                [ Pointer 2 10 13 False (Just "\x1b[31mthis has type `Int`\x1b[0m")
                , Pointer 3 10 17 False (Just "\x1b[31mbut this has type `String`\x1b[0m")
                ]
                Nothing)
            [ Block
                fancyYellowStyle
                ("file.hs", 1, 7)
                Nothing
                [ Pointer 1 7 9 True Nothing
                , Pointer 2 5 9 True Nothing
                , Pointer 3 5 9 True (Just "\x1b[33min this `if` expression\x1b[0m")
                ]
                Nothing
            ]
            (Just "\x1b[33mnote: use --explain E001 to learn more\x1b[0m")
        ]
    putStrLn ""
