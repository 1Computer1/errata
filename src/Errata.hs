{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Errata
Copyright   : (c) 2020 comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

This module is for creating pretty error messages. We assume very little about the format you want to use, so much of
this module is to allow you to customize your error messages.

To get started, see the documentation for 'prettyErrors'. When using this module, we recommend you turn the
@OverloadedStrings@ extension and import "Data.Text" at the very least due to the use of 'Data.Text.Text' (strict).

The overall workflow to use the printer is to create 'Errata' and a 'Convert', which entails:

* Converting your errors to 'Errata' by filling in messages and 'Block's.
* To fill in a 'Block', you would have to extract source info from your errors and also create 'Pointer's.
* In addition, you will have to choose or create a 'Style' for your block.
* Converting your source to 'Data.Text.Text' in 'convertLines' and 'convertLine'.
-}
module Errata
    ( -- * Error format data
      Convert(..)
    , Errata(..)
    , errataSimple
      -- * Blocks and pointers
    , Block(..)
    , blockSimple
    , blockSimple'
    , blockConnected
    , blockConnected'
    , blockMerged
    , blockMerged'
    , Pointer(..)
    , pointerColumns
      -- * Styling options
    , Style(..)
    , basicStyle
    , fancyStyle
    , fancyRedStyle
    , fancyYellowStyle
    , highlight
      -- * Pretty printer
    , prettyErrors
    , prettyErrorsNE
    ) where

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Errata.Render
import           Errata.Types

-- | Creates a simple error that has a single block, with an optional header or body.
errataSimple
    :: Maybe T.Text -- ^ The header.
    -> Block        -- ^ The block.
    -> Maybe T.Text -- ^ The body.
    -> Errata
errataSimple header pointer body = Errata
    { errataHeader = header
    , errataBlock = pointer
    , errataBlocks = []
    , errataBody = body
    }

-- | A simple block that points to only one line and optionally has a label or a body message.
blockSimple
    :: Style        -- ^ The style of the pointer.
    -> FilePath     -- ^ The filepath.
    -> Int          -- ^ The line number starting at 1.
    -> (Int, Int)   -- ^ The column span. These start at 1.
    -> Maybe T.Text -- ^ The label.
    -> Maybe T.Text -- ^ The body message.
    -> Block
blockSimple style fp l (cs, ce) m lbl = Block
    { blockStyle = style
    , blockLocation = (fp, l, cs)
    , blockPointers = [Pointer l cs ce False lbl]
    , blockBody = m
    }

-- | A variant of 'blockSimple' that only points at one column.
blockSimple'
    :: Style        -- ^ The style of the pointer.
    -> FilePath     -- ^ The filepath.
    -> Int          -- ^ The line number starting at 1.
    -> Int          -- ^ The column number starting at 1.
    -> Maybe T.Text -- ^ The label.
    -> Maybe T.Text -- ^ The body message.
    -> Block
blockSimple' style fp l c lbl m = Block
    { blockStyle = style
    , blockLocation = (fp, l, c)
    , blockPointers = [Pointer l c (c + 1) False lbl]
    , blockBody = m
    }

-- | A block that points to two parts of the source that are visually connected together.
blockConnected
    :: Style                         -- ^ The style of the pointer.
    -> FilePath                      -- ^ The filepath.
    -> (Int, Int, Int, Maybe T.Text) -- ^ The first line number and column span, starting at 1, as well as a label.
    -> (Int, Int, Int, Maybe T.Text) -- ^ The second line number and column span, starting at 1, as well as a label.
    -> Maybe T.Text                  -- ^ The body message.
    -> Block
blockConnected style fp (l1, cs1, ce1, lbl1) (l2, cs2, ce2, lbl2) m = Block
    { blockStyle = style
    , blockLocation = (fp, l1, cs1)
    , blockPointers = [Pointer l1 cs1 ce1 True lbl1, Pointer l2 cs2 ce2 True lbl2]
    , blockBody = m
    }

-- | A variant of 'blockConnected' where the pointers point at only one column.
blockConnected'
    :: Style                    -- ^ The style of the pointer.
    -> FilePath                 -- ^ The filepath.
    -> (Int, Int, Maybe T.Text) -- ^ The first line number and column, starting at 1, as well as a label.
    -> (Int, Int, Maybe T.Text) -- ^ The second line number and column, starting at 1, as well as a label.
    -> Maybe T.Text             -- ^ The body message.
    -> Block
blockConnected' style fp (l1, c1, lbl1) (l2, c2, lbl2) m = Block
    { blockStyle = style
    , blockLocation = (fp, l1, c1)
    , blockPointers = [Pointer l1 c1 (c1 + 1) True lbl1, Pointer l2 c2 (c2 + 1) True lbl2]
    , blockBody = m
    }

{-|
A block that points to two parts of the source that are visually connected together.

If the two parts of the source happen to be on the same line, the pointers are merged into one.
-}
blockMerged
    :: Style                         -- ^ The style of the pointer.
    -> FilePath                      -- ^ The filepath.
    -> (Int, Int, Int, Maybe T.Text) -- ^ The first line number and column span, starting at 1, as well as a label.
    -> (Int, Int, Int, Maybe T.Text) -- ^ The second line number and column span, starting at 1, as well as a label.
    -> Maybe T.Text                  -- ^ The label for when the two pointers are merged into one.
    -> Maybe T.Text                  -- ^ The body message.
    -> Block
blockMerged style fp (l1, cs1, ce1, lbl1) (l2, cs2, ce2, lbl2) lbl m = Block
    { blockStyle = style
    , blockLocation = (fp, l1, cs1)
    , blockPointers = if l1 == l2
        then [Pointer l1 cs1 ce2 False lbl]
        else [Pointer l1 cs1 ce1 True lbl1, Pointer l2 cs2 ce2 True lbl2]
    , blockBody = m
    }

-- | A variant of 'blockMerged' where the pointers point at only one column.
blockMerged'
    :: Style                    -- ^ The style of the pointer.
    -> FilePath                 -- ^ The filepath.
    -> (Int, Int, Maybe T.Text) -- ^ The first line number and column, starting at 1, as well as a label.
    -> (Int, Int, Maybe T.Text) -- ^ The second line number and column, starting at 1, as well as a label.
    -> Maybe T.Text             -- ^ The label for when the two pointers are merged into one.
    -> Maybe T.Text             -- ^ The body message.
    -> Block
blockMerged' style fp (l1, c1, lbl1) (l2, c2, lbl2) lbl m = Block
    { blockStyle = style
    , blockLocation = (fp, l1, c1)
    , blockPointers = if l1 == l2
        then [Pointer l1 c1 (c2 + 1) False lbl]
        else [Pointer l1 c1 (c1 + 1) True lbl1, Pointer l2 c2 (c2 + 1) True lbl2]
    , blockBody = m
    }

{-|
A basic style using only ASCII characters.

Errors should look like so:

> error header message
> --> file.ext:1:16
>   |
> 1 |   line 1 foo bar do
>   |  ________________^^ start label
> 2 | | line 2
>   | |      ^ unconnected label
> 3 | | line 3
> . | |______^ middle label
> 6 | | line 6
> 7 | | line 7 baz end
>   | |______^_____^^^ end label
>   |        |
>   |        | inner label
> block body message
> error body message
-}
basicStyle :: Style
basicStyle = Style
    { styleLocation = \(fp, l, c) -> T.concat ["--> ", T.pack fp, ":", T.pack $ show l, ":", T.pack $ show c]
    , styleNumber = T.pack . show
    , styleLine = const id
    , styleEllipsis = "."
    , styleLinePrefix = "|"
    , styleUnderline = "^"
    , styleVertical = "|"
    , styleHorizontal = "_"
    , styleDownRight = " "
    , styleUpRight = "|"
    , styleUpDownRight = "|"
    }

{-|
A fancy style using Unicode characters.

Errors should look like so:

> error header message
> → file.ext:1:16
>   │
> 1 │   line 1 foo bar do
>   │ ┌────────────────^^ start label
> 2 │ │ line 2
>   │ │      ^ unconnected label
> 3 │ │ line 3
> . │ ├──────^ middle label
> 6 │ │ line 6
> 7 │ │ line 7 baz end
>   │ └──────^─────^^^ end label
>   │        │
>   │        └ inner label
> block body message
> error body message
-}
fancyStyle :: Style
fancyStyle = Style
    { styleLocation = \(fp, l, c) -> T.concat
        [ "→ ", T.pack fp, ":", T.pack $ show l, ":", T.pack $ show c
        ]
    , styleNumber = T.pack . show
    , styleLine = const id
    , styleEllipsis = "."
    , styleLinePrefix = "│"
    , styleUnderline = "^"
    , styleHorizontal = "─"
    , styleVertical = "│"
    , styleDownRight = "┌"
    , styleUpDownRight = "├"
    , styleUpRight = "└"
    }

-- | A fancy style using Unicode characters and ANSI colors, similar to 'fancyStyle'. Most things are colored red.
fancyRedStyle :: Style
fancyRedStyle = Style
    { styleLocation = \(fp, l, c) -> T.concat
        [ "\x1b[34m→\x1b[0m ", T.pack fp, ":", T.pack $ show l, ":", T.pack $ show c
        ]
    , styleNumber = T.pack . show
    , styleLine = highlight "\x1b[31m" "\x1b[0m"
    , styleEllipsis = "."
    , styleLinePrefix = "\x1b[34m│\x1b[0m"
    , styleUnderline = "\x1b[31m^\x1b[0m"
    , styleHorizontal = "\x1b[31m─\x1b[0m"
    , styleVertical = "\x1b[31m│\x1b[0m"
    , styleDownRight = "\x1b[31m┌\x1b[0m"
    , styleUpDownRight = "\x1b[31m├\x1b[0m"
    , styleUpRight = "\x1b[31m└\x1b[0m"
    }

-- | A fancy style using Unicode characters and ANSI colors, similar to 'fancyStyle'. Most things are colored yellow.
fancyYellowStyle :: Style
fancyYellowStyle = Style
    { styleLocation = \(fp, l, c) -> T.concat
        [ "\x1b[34m→\x1b[0m ", T.pack fp, ":", T.pack $ show l, ":", T.pack $ show c
        ]
    , styleNumber = T.pack . show
    , styleLine = highlight "\x1b[33m" "\x1b[0m"
    , styleEllipsis = "."
    , styleLinePrefix = "\x1b[34m│\x1b[0m"
    , styleUnderline = "\x1b[33m^\x1b[0m"
    , styleHorizontal = "\x1b[33m─\x1b[0m"
    , styleVertical = "\x1b[33m│\x1b[0m"
    , styleDownRight = "\x1b[33m┌\x1b[0m"
    , styleUpRight = "\x1b[33m└\x1b[0m"
    , styleUpDownRight = "\x1b[33m├\x1b[0m"
    }

-- | Adds highlighting to spans of text by enclosing it with some text e.g ANSI escape codes.
highlight
    :: T.Text       -- ^ Text to add before.
    -> T.Text       -- ^ Text to add after.
    -> [(Int, Int)] -- ^ Indices to enclose. These are column spans, starting at 1. They must not overlap.
    -> T.Text       -- ^ Text to highlight.
    -> T.Text
highlight open close = go False . concatMap (\(a, b) -> [a, b])
    where
        go _ [] xs = xs
        go False (i:is) xs =
            let (a, ys) = T.splitAt (i - 1) xs
            in a <> open <> go True (map (\x -> x - i + 1) is) ys
        go True (i:is) xs =
            let (a, ys) = T.splitAt (i - 1) xs
            in a <> close <> go False (map (\x -> x - i + 1) is) ys

{-|
Pretty prints errors using a converter. The original source is required. Returns a lazy 'Data.Text.Lazy.Text'
(this is the only place lazy text is used). If the list is empty, an empty string is returned.

Suppose we had an error of this type:

> data ParseError = ParseError
>     { peFile       :: FilePath
>     , peLine       :: Int
>     , peColStart   :: Int
>     , peColEnd     :: Int
>     , peUnexpected :: T.Text
>     , peExpected   :: [T.Text]
>     }

Then we can create a simple pretty printer like so:

> import qualified Data.List.NonEmpty as N
> import qualified Data.Text as T
> import qualified Data.Text.Lazy.IO as TL
> import           Errata
>
> toErrata :: ParseError -> Errata
> toErrata (ParseError fp l cs ce unexpected expected) =
>     errataSimple
>         "error: invalid syntax"
>         (blockSimple basicStyle fp l (cs, ce)
>             ("unexpected " <> unexpected <> "\nexpected " <> T.intercalate ", " expected))
>
> converter :: Convert T.Text
> converter = Convert
>     { convertLines = T.lines
>     , convertLine = id
>     }
>
> printErrors :: T.Text -> N.NonEmpty ParseError -> IO ()
> printErrors source es = TL.putStrLn $ prettyErrors converter source (toErrata <$> es)

Note that in the above example, we have @OverloadedStrings@ enabled to reduce uses of 'Data.Text.pack'.

An example error message from this might be:

> error: invalid syntax
> --> ./comma.json:2:18
>   |
> 2 |     "bad": [1, 2,]
>   |                  ^
> unexpected ]
> expected null, true, false, ", -, digit, [, {
-}
prettyErrors :: Convert source -> source -> [Errata] -> TL.Text
prettyErrors ef source errs = TB.toLazyText $ renderErrors ef source errs

-- | A variant of 'prettyErrors' for non-empty lists. You can ensure the output is never an empty string.
prettyErrorsNE :: Convert source -> source -> N.NonEmpty Errata -> TL.Text
prettyErrorsNE ef source errs = prettyErrors ef source (N.toList errs)
