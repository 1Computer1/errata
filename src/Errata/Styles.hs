{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Errata.Styles
Copyright   : (c) 2020- comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Premade styles for blocks and pointers.
-}
module Errata.Styles
    ( basicStyle
    , basicPointer
    , fancyStyle
    , fancyPointer
    , fancyRedStyle
    , fancyRedPointer
    , fancyYellowStyle
    , fancyYellowPointer
    , highlight
    ) where

import           Data.Bifunctor (bimap, second)
import qualified Data.Text as T
import           Errata.Types

{- | A basic style using only ASCII characters.

Errors should look like so (with 'basicPointer'):

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
    , styleLine = highlight
    , styleEllipsis = "."
    , styleLinePrefix = "|"
    , styleVertical = "|"
    , styleHorizontal = "_"
    , styleDownRight = " "
    , styleUpRight = "|"
    , styleUpDownRight = "|"
    , styleTabWidth = 4
    , styleExtraLinesAfter = 2
    , styleExtraLinesBefore = 1
    , stylePaddingTop = True
    , stylePaddingBottom = False
    , styleEnableDecorations = True
    , styleEnableLinePrefix = True
    }

-- | Pointers using only ASCII characters.
basicPointer :: PointerStyle
basicPointer = PointerStyle
    { styleHighlight = id
    , styleUnderline = "^"
    , styleHook = "|"
    , styleConnector = "|"
    , styleEnableHook = True
    }

{- | A fancy style using Unicode characters.

Errors should look like so (with 'fancyPointer'):

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
    , styleLine = highlight
    , styleEllipsis = "."
    , styleLinePrefix = "│"
    , styleHorizontal = "─"
    , styleVertical = "│"
    , styleDownRight = "┌"
    , styleUpDownRight = "├"
    , styleUpRight = "└"
    , styleTabWidth = 4
    , styleExtraLinesAfter = 2
    , styleExtraLinesBefore = 1
    , stylePaddingTop = True
    , stylePaddingBottom = False
    , styleEnableDecorations = True
    , styleEnableLinePrefix = True
    }

-- | Pointers using Unicode characters and ANSI colors.
fancyPointer :: PointerStyle
fancyPointer = PointerStyle
    { styleHighlight = id
    , styleUnderline = "^"
    , styleHook = "└"
    , styleConnector = "│"
    , styleEnableHook = True
    }

-- | A fancy style using Unicode characters and ANSI colors, similar to 'fancyStyle'. Most things are colored red.
fancyRedStyle :: Style
fancyRedStyle = Style
    { styleLocation = \(fp, l, c) -> T.concat
        [ "\x1b[34m→\x1b[0m ", T.pack fp, ":", T.pack $ show l, ":", T.pack $ show c
        ]
    , styleNumber = T.pack . show
    , styleLine = highlight
    , styleEllipsis = "."
    , styleLinePrefix = "\x1b[34m│\x1b[0m"
    , styleHorizontal = "\x1b[31m─\x1b[0m"
    , styleVertical = "\x1b[31m│\x1b[0m"
    , styleDownRight = "\x1b[31m┌\x1b[0m"
    , styleUpDownRight = "\x1b[31m├\x1b[0m"
    , styleUpRight = "\x1b[31m└\x1b[0m"
    , styleTabWidth = 4
    , styleExtraLinesAfter = 2
    , styleExtraLinesBefore = 1
    , stylePaddingTop = True
    , stylePaddingBottom = False
    , styleEnableDecorations = True
    , styleEnableLinePrefix = True
    }

-- | Red pointers using Unicode characters and ANSI colors.
fancyRedPointer :: PointerStyle
fancyRedPointer = PointerStyle
    { styleHighlight = \x -> "\x1b[31m" <> x <> "\x1b[0m"
    , styleUnderline = "\x1b[31m^\x1b[0m"
    , styleHook = "\x1b[31m└\x1b[0m"
    , styleConnector = "\x1b[31m│\x1b[0m"
    , styleEnableHook = True
    }

-- | A fancy style using Unicode characters and ANSI colors, similar to 'fancyStyle'. Most things are colored yellow.
fancyYellowStyle :: Style
fancyYellowStyle = Style
    { styleLocation = \(fp, l, c) -> T.concat
        [ "\x1b[34m→\x1b[0m ", T.pack fp, ":", T.pack $ show l, ":", T.pack $ show c
        ]
    , styleNumber = T.pack . show
    , styleLine = highlight
    , styleEllipsis = "."
    , styleLinePrefix = "\x1b[34m│\x1b[0m"
    , styleHorizontal = "\x1b[33m─\x1b[0m"
    , styleVertical = "\x1b[33m│\x1b[0m"
    , styleDownRight = "\x1b[33m┌\x1b[0m"
    , styleUpRight = "\x1b[33m└\x1b[0m"
    , styleUpDownRight = "\x1b[33m├\x1b[0m"
    , styleTabWidth = 4
    , styleExtraLinesAfter = 2
    , styleExtraLinesBefore = 1
    , stylePaddingTop = True
    , stylePaddingBottom = False
    , styleEnableDecorations = True
    , styleEnableLinePrefix = True
    }

-- | Yellow pointers using Unicode characters and ANSI colors.
fancyYellowPointer :: PointerStyle
fancyYellowPointer = PointerStyle
    { styleHighlight = \x -> "\x1b[33m" <> x <> "\x1b[0m"
    , styleUnderline = "\x1b[33m^\x1b[0m"
    , styleHook = "\x1b[33m└\x1b[0m"
    , styleConnector = "\x1b[33m│\x1b[0m"
    , styleEnableHook = True
    }

-- | Adds highlighting to spans of text by modifying it with the given styles' highlights.
highlight
    :: [(PointerStyle, (Column, Column))] -- ^ Styles and columns to work on. These are sorted, starting at 1. They must not overlap.
    -> T.Text                             -- ^ Text to highlight.
    -> T.Text
highlight [] xs = xs
highlight ((p, (s, e)):ps) xs =
    let (pre, xs') = T.splitAt (s - 1) xs
        (txt, xs'') = T.splitAt (e - s) xs'
        hi = styleHighlight p
        ps' = second (both (\i -> i - e + 1)) <$> ps
    in pre <> hi txt <> highlight ps' xs''
    where
        both f = bimap f f
