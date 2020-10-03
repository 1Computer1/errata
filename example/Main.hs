{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Data.List
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

-- | From the Haddock for 'prettyErrors'.
jsonExample :: IO ()
jsonExample = printErrors
    "{\n    \"bad\": [1, 2,]\n    }"
    [ParseError "./comma.json" 2 18 "]" ["null", "true", "false", "\"", "-", "digit", "[", "{"]]

-- | From the README.
foldExample :: IO ()
foldExample = TL.putStrLn $ prettyErrors @String
    "sum xs = fold (+) 0 xs"
    [ Errata
        (Just "\x1b[31m─────── NAME UNKNOWN ───────\x1b[0m\n\nThe name \x1b[31mfold\x1b[0m was not found.\n")
        [ Block
            fancyRedStyle { styleUnderline = " " }
            ("file.hs", 1, 10)
            Nothing
            [Pointer 1 10 14 False Nothing]
            Nothing
        ]
        (Just "\nDid you mean to use one of these?\n\n    \x1b[31mfoldl\x1b[0m\n    \x1b[31mfoldr\x1b[0m")
    ]

-- | From the README.
ifExample :: IO ()
ifExample = TL.putStrLn $ prettyErrors @String
    "foo = if 1 > 2\n    then 100\n    else \"uh oh\""
    [ Errata
        (Just "\x1b[31merror[E001]: mismatching types in `if` expression\x1b[0m")
        [ Block
            fancyRedStyle
            ("file.hs", 3, 10)
            Nothing
            [ Pointer 2 10 13 False (Just "\x1b[31mthis has type `Int`\x1b[0m")
            , Pointer 3 10 17 False (Just "\x1b[31mbut this has type `String`\x1b[0m")
            ]
            Nothing
        , Block
            fancyYellowStyle
            ("file.hs", 1, 7)
            Nothing
            [ Pointer 1 7 9 True Nothing
            , Pointer 2 5 9 True Nothing
            , Pointer 3 5 9 True (Just "\x1b[33min this `if` expression\x1b[0m")
            ]
            Nothing
        ]
        (Just "\n\x1b[33mnote: use --explain E001 to learn more\x1b[0m")
    ]

main :: IO ()
main = sequence_ . intersperse (putStrLn "") $
    [ jsonExample
    , foldExample
    , ifExample
    ]
