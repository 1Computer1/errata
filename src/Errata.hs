{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Errata
Copyright   : (c) 2020 comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

This module is for creating pretty error messages. We assume very little about the format you want to use, so much of
this module is to allow you to customize your error messages.

To get started, see the documentation for 'prettyErrors'. When using this module, we recommend you turn on the
@OverloadedStrings@ extension and import "Data.Text" at the very least due to the use of 'Data.Text.Text' (strict).

The overall workflow to use the printer is to convert your error type to 'Errata', which entails filling in messages
and 'Block's. You can create 'Errata' and 'Block' from their constructors, or use the convenience functions for
common usecases, like 'errataSimple' and 'blockSimple'.

For easier reading, we define:

> type Line = Int
> type Column = Int
> type Header = Text
> type Body = Text
> type Label = Text
-}
module Errata
    ( -- * Error format data
      Errata (..)
    , errataSimple
      -- * Blocks and pointers
    , Block (..)
    , blockSimple
    , blockSimple'
    , blockConnected
    , blockConnected'
    , blockMerged
    , blockMerged'
    , Pointer (..)
      -- * Styling options
    , Style (..)
    , basicStyle
    , fancyStyle
    , fancyRedStyle
    , fancyYellowStyle
      -- * Pretty printer
    , prettyErrors
    ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Errata.Internal.Render
import           Errata.Source
import           Errata.Types

-- | Creates a simple error that has a single block, with an optional header or body.
errataSimple
    :: Maybe Header -- ^ The header.
    -> Block        -- ^ The block.
    -> Maybe Body   -- ^ The body.
    -> Errata
errataSimple header block body = Errata
    { errataHeader = header
    , errataBlocks = [block]
    , errataBody = body
    }

-- | A simple block that points to only one line and optionally has a label, header, or body message.
blockSimple
    :: Style                               -- ^ The style of the pointer.
    -> FilePath                            -- ^ The filepath.
    -> Maybe Header                        -- ^ The header message.
    -> (Line, Column, Column, Maybe Label) -- ^ The line number and column span, starting at 1, and a label.
    -> Maybe Body                          -- ^ The body message.
    -> Block
blockSimple style fp hm (l, cs, ce, lbl) bm = Block
    { blockStyle = style
    , blockLocation = (fp, l, cs)
    , blockHeader = hm
    , blockPointers = [Pointer l cs ce False lbl]
    , blockBody = bm
    }

-- | A variant of 'blockSimple' that only points at one column.
blockSimple'
    :: Style                       -- ^ The style of the pointer.
    -> FilePath                    -- ^ The filepath.
    -> Maybe Header                -- ^ The header message.
    -> (Line, Column, Maybe Label) -- ^ The line number and column, starting at 1, and a label.
    -> Maybe Body                  -- ^ The body message.
    -> Block
blockSimple' style fp hm (l, c, lbl) bm =
    blockSimple style fp hm (l, c, c + 1, lbl) bm

-- | A block that points to two parts of the source that are visually connected together.
blockConnected
    :: Style                               -- ^ The style of the pointer.
    -> FilePath                            -- ^ The filepath.
    -> Maybe Header                        -- ^ The header message.
    -> (Line, Column, Column, Maybe Label) -- ^ The first line number and column span, starting at 1, and a label.
    -> (Line, Column, Column, Maybe Label) -- ^ The second line number and column span, starting at 1, and a label.
    -> Maybe Body                          -- ^ The body message.
    -> Block
blockConnected style fp hm (l1, cs1, ce1, lbl1) (l2, cs2, ce2, lbl2) bm = Block
    { blockStyle = style
    , blockLocation = (fp, l1, cs1)
    , blockHeader = hm
    , blockPointers = [Pointer l1 cs1 ce1 True lbl1, Pointer l2 cs2 ce2 True lbl2]
    , blockBody = bm
    }

-- | A variant of 'blockConnected' where the pointers point at only one column.
blockConnected'
    :: Style                       -- ^ The style of the pointer.
    -> FilePath                    -- ^ The filepath.
    -> Maybe Header                -- ^ The header message.
    -> (Line, Column, Maybe Label) -- ^ The first line number and column, starting at 1, and a label.
    -> (Line, Column, Maybe Label) -- ^ The second line number and column, starting at 1, and a label.
    -> Maybe Body                  -- ^ The body message.
    -> Block
blockConnected' style fp hm (l1, c1, lbl1) (l2, c2, lbl2) bm =
    blockConnected style fp hm (l1, c1, c1 + 1, lbl1) (l2, c2, c2 + 1, lbl2) bm

{- | A block that points to two parts of the source that are visually connected together.

If the two parts of the source happen to be on the same line, the pointers are merged into one.
-}
blockMerged
    :: Style                               -- ^ The style of the pointer.
    -> FilePath                            -- ^ The filepath.
    -> Maybe Header                        -- ^ The header message.
    -> (Line, Column, Column, Maybe Label) -- ^ The first line number and column span, starting at 1, and a label.
    -> (Line, Column, Column, Maybe Label) -- ^ The second line number and column span, starting at 1, and a label.
    -> Maybe Label                         -- ^ The label for when the two pointers are merged into one.
    -> Maybe Body                          -- ^ The body message.
    -> Block
blockMerged style fp hm (l1, cs1, ce1, lbl1) (l2, cs2, ce2, lbl2) lbl bm = Block
    { blockStyle = style
    , blockLocation = (fp, l1, cs1)
    , blockHeader = hm
    , blockPointers = if l1 == l2
        then [Pointer l1 cs1 ce2 False lbl]
        else [Pointer l1 cs1 ce1 True lbl1, Pointer l2 cs2 ce2 True lbl2]
    , blockBody = bm
    }

-- | A variant of 'blockMerged' where the pointers point at only one column.
blockMerged'
    :: Style                       -- ^ The style of the pointer.
    -> FilePath                    -- ^ The filepath.
    -> Maybe Header                -- ^ The header message.
    -> (Line, Column, Maybe Label) -- ^ The first line number and column, starting at 1, and a label.
    -> (Line, Column, Maybe Label) -- ^ The second line number and column, starting at 1, and a label.
    -> Maybe Label                 -- ^ The label for when the two pointers are merged into one.
    -> Maybe Body                  -- ^ The body message.
    -> Block
blockMerged' style fp hm (l1, c1, lbl1) (l2, c2, lbl2) lbl bm =
    blockMerged style fp hm (l1, c1, c1 + 1, lbl1) (l2, c2, c2 + 1, lbl2) lbl bm

{- | A basic style using only ASCII characters.

Errors should look like so:

> error header message
> --> file.ext:1:16
> block header message
>   |
> 1 |   line 1 foo bar do
>   |  ________________^^ start label
> 2 | | line 2
>   | |      ^ unconnected label
> 3 | | line 3
>   | |______^ middle label
> 4 | | line 4
> 5 | | line 5
> . | |
> 7 | | line 7
> 8 | | line 8 baz end
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
    , styleTabWidth = 4
    }

{- | A fancy style using Unicode characters.

Errors should look like so:

> error header message
> → file.ext:1:16
> block header message
>   │
> 1 │   line 1 foo bar do
>   │ ┌────────────────^^ start label
> 2 │ │ line 2
>   │ │      ^ unconnected label
> 3 │ │ line 3
>   │ ├──────^ middle label
> 4 │ │ line 4
> 5 │ │ line 5
> . │ │
> 7 │ │ line 7
> 8 │ │ line 8 baz end
>   │ └──────^─────^^^ end label
>   │        │
>   │        └ inner label
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
    , styleTabWidth = 4
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
    , styleTabWidth = 4
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
    , styleTabWidth = 4
    }

{- | Pretty prints errors. The original source is required. Returns 'Data.Text.Lazy.Text' (lazy). If the list is empty,
an empty string is returned.

Suppose we had an error of this type:

> data ParseError = ParseError
>     { peFile       :: FilePath
>     , peLine       :: Int
>     , peCol        :: Int
>     , peUnexpected :: T.Text
>     , peExpected   :: [T.Text]
>     }

Then we can create a simple pretty printer like so:

@
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import           "Errata"

toErrata :: ParseError -> 'Errata'
toErrata (ParseError fp l c unexpected expected) =
    'errataSimple'
        (Just \"an error occured!\")
        ('blockSimple' 'basicStyle' fp
            (Just \"error: invalid syntax\")
            (l, c, c + T.length unexpected, Just \"this one\")
            (Just $ \"unexpected \" \<> unexpected \<> \"\\nexpected \" \<> T.intercalate \", \" expected))
        Nothing

printErrors :: T.Text -> [ParseError] -> IO ()
printErrors source es = TL.putStrLn $ 'prettyErrors' source (map toErrata es)
@

Note that in the above example, we have @OverloadedStrings@ enabled to reduce uses of 'Data.Text.pack'.

An example error message from this might be:

> an error occured!
> --> ./comma.json:2:18
> error: invalid syntax
>   |
> 2 |     "bad": [1, 2,]
>   |                  ^ this one
> unexpected ]
> expected null, true, false, ", -, digit, [, {
-}
prettyErrors :: Source source => source -> [Errata] -> TL.Text
prettyErrors source errs = TB.toLazyText $ renderErrors source errs
