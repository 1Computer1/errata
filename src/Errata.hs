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

The overall workflow to use the printer is to create a 'Convert', which entails:

* Converting your source to 'Data.Text.Text' in 'convertLines' and 'convertLine'.
* Converting your errors to 'Errata' in 'convertError', by filling in messages and 'Block's.
* To fill in a 'Block', you would have to extract source info from your errors and also create 'Pointer's.
* In addition, you will have to choose or create a 'Style' for your block.
-}
module Errata
    ( -- * Error format data
      Convert(..)
    , Errata(..)
    , errataSimple
      -- * Blocks and pointers
    , Block(..)
    , blockSimple
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
    , prettyErrors'
    ) where

import           Data.List
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Sequence as S
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

-- | Functions to read the source text and format error messages.
data Convert source error = Convert
    { {-|
      Converts your error type into 'Errata' for pretty printing.

      This assumes your error somehow has the span of the error in terms of lines and columns, in order to create
      pointers to the source code.
      -}
      convertError :: error -> Errata

      -- | Splits the source into lines. Usually, this is e.g. 'lines' (String) or 'Data.Text.lines' (Text).
    , convertLines :: source -> [source]

      -- | Converts the source text to text for printing. The given source text is always a single line of the source.
    , convertLine :: source -> T.Text
    }

-- | A collection of information for pretty printing an error.
data Errata = Errata
    { errataHeader :: Maybe T.Text -- ^ The message that appears above all the blocks.
    , errataBlock  :: Block        -- ^ The main error block, which will be used for sorting errors.
    , errataBlocks :: [Block]      -- ^ Extra blocks in the source code to display. Blocks themselves are not sorted.
    , errataBody   :: Maybe T.Text -- ^ The message that appears below all the blocks.
    }

-- | Creates a simple error that has a header and single block.
errataSimple :: T.Text -> Block -> Errata
errataSimple header pointer = Errata
    { errataHeader = Just header
    , errataBlock = pointer
    , errataBlocks = []
    , errataBody = Nothing
    }

{-|
Information about a block in the source code, such as pointers and messages.

Each block has a style associated with it.
-}
data Block = Block
    { -- | The style of the block.
      blockStyle :: Style

      {-|
      The filepath, line, and column of the block. These start at 1.

      This is used for sorting errors, as well as to create the text that details the location.
      -}
    , blockLocation :: (FilePath, Int, Int)

      {-|
      The block's pointers. These are used to "point out" parts of the source code in this block.

      The locations of each of these pointers must be non-overlapping. If the pointers are touching at a boundary
      however, that is allowed.
      -}
    , blockPointers :: [Pointer]

      -- | The message for the block. This will appear below the source lines.
    , blockBody :: Maybe T.Text
    }

-- | A simple block that points to only one line and has a body message.
blockSimple
    :: Style      -- ^ The style of the pointer.
    -> FilePath   -- ^ The filepath.
    -> Int        -- ^ The line number starting at 1.
    -> (Int, Int) -- ^ The column span. These start at 1.
    -> T.Text     -- ^ The body message.
    -> Block
blockSimple style fp l (cs, ce) m = Block
    { blockStyle = style
    , blockLocation = (fp, l, cs)
    , blockPointers = [Pointer l cs ce False Nothing]
    , blockBody = Just m
    }

{-|
A pointer is the span of the source code at a line, from one column to another. Each of the positions start at 1.

A pointer may also have a label that will display inline.

A pointer may also be connected to all the other pointers within the same block.
-}
data Pointer = Pointer
    { pointerLine     :: Int          -- ^ The line of the span.
    , pointerColStart :: Int          -- ^ The starting column of the span.
    , pointerColEnd   :: Int          -- ^ The ending column of the span.
    , pointerConnect  :: Bool         -- ^ Whether this pointer connects with other pointers.
    , pointerLabel    :: Maybe T.Text -- ^ An optional label for the span.
    }

-- | Gets the column span for a 'Pointer'.
pointerColumns :: Pointer -> (Int, Int)
pointerColumns p = (pointerColStart p, pointerColEnd p)

-- | Stylization options for a block, e.g. characters to use.
data Style = Style
    { -- | Shows the location of a block at a file, line, and column.
      styleLocation :: (FilePath, Int, Int) -> T.Text

      -- | Shows the line number /n/ for a source line. The result should visually be the same length as just @show n@.
    , styleNumber :: Int -> T.Text

      {-|
      Stylize a source line.

      Column pointers of the text that are being underlined are given for highlighting purposes. The result of this
      should visually take up the same space as the original line.
      -}
    , styleLine :: [(Int, Int)] -> T.Text -> T.Text

      {-|
      The text to use as an ellipsis in the position of line numbers for when lines are omitted. This should visually
      be one character.
      -}
    , styleEllipsis :: T.Text

      -- | The prefix before the source lines.
    , styleLinePrefix :: T.Text

      {-|
      The text to underline a character in a pointer. This should visually be one character.
      -}
    , styleUnderline :: T.Text

      {-|
      The text to use as a vertical bar when connecting pointers. This should visually be one character.
      -}
    , styleVertical :: T.Text

      {-|
      The text to use as a horizontal bar when connecting pointers. This should visually be one character.
      -}
    , styleHorizontal :: T.Text

      {-|
      The text to use as a connector downwards and rightwards when connecting pointers. This should visually
      be one character.
      -}
    , styleDownRight :: T.Text

      {-|
      The text to use as a connector upwards and rightwards when connecting pointers. This should visually
      be one character.
      -}
    , styleUpRight :: T.Text

      {-|
      The text to use as a connector upwards, downwards, and rightwards when connecting pointers. This should visually
      be one character.
      -}
    , styleUpDownRight :: T.Text
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
Pretty prints errors using a converter. The original source is required. Returns a 'Data.Text.Lazy.Text'
(lazy; this is the only place lazy text is used).

Suppose we had an error of this type:

> data ParseError = ParseError
>     { peFile       :: FilePath
>     , peLine       :: Int
>     , peColStart   :: Int
>     , peColEnd     :: Int
>     , peUnexpected :: T.Text
>     , peExpected   :: [T.Text]
>     }

Then we can create a simple 'Convert' instance like so:

> import qualified Data.List.NonEmpty as N
> import qualified Data.Text as T
> import qualified Data.Text.Lazy.IO as TL
> import           Errata
>
> converter :: Convert T.Text ParseError
> converter = Convert
>    { convertError = \(ParseError fp l cs ce unexpected expected) ->
>        errataSimple
>            "error: invalid syntax"
>            (blockSimple basicStyle fp l (cs, ce)
>                ("unexpected " <> unexpected <> "\nexpected " <> T.intercalate ", " expected))
>    , convertLines = T.lines
>    , convertLine = id
>    }
>
> printErrors :: T.Text -> N.NonEmpty ParseError -> IO ()
> printErrors source es = TL.putStrLn $ prettyErrors converter source es

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
prettyErrors :: Convert source error -> source -> N.NonEmpty error -> TL.Text
prettyErrors ef source errs = prettyErrors' ef source (N.toList errs)

-- | Variant of 'prettyErrors' for lists. Simply gives an empty string for empty lists.
prettyErrors' :: Convert source error -> source -> [error] -> TL.Text
prettyErrors' ef@(Convert {..}) source errs = TB.toLazyText $ unsplit "\n\n" prettified
    where
        sortedErrata
            = sortOn (\(Errata {..}) -> blockLocation errataBlock)
            . map convertError
            $ errs

        -- We may arbitrarily index the source lines a lot, so a Seq is appropriate.
        -- If push comes to shove, this could be replaced with an 'Array' or a 'Vec'.
        slines = S.fromList (convertLines source)
        prettified = map (prettyErrata ef slines) sortedErrata

-- | A single pretty error from metadata and source lines.
prettyErrata :: Convert source error -> S.Seq source -> Errata -> TB.Builder
prettyErrata ef@(Convert {..}) slines (Errata {..}) = errorMessage
    where
        errorMessage = mconcat
            [ TB.fromText $ maybe "" (<> "\n") errataHeader
            , unsplit "\n\n" (map (prettyBlock ef slines) (errataBlock : errataBlocks))
            , TB.fromText $ maybe "" ("\n\n" <>) errataBody
            ]

-- | A single pretty block from block data and source lines.
prettyBlock :: Convert source error -> S.Seq source -> Block -> TB.Builder
prettyBlock ef@(Convert {..}) slines block@(Block {..}) = blockMessage
    where
        blockMessage = mconcat
            [ TB.fromText $ styleLocation blockStyle blockLocation
            , maybe "" ("\n" <>) (prettySourceLines ef slines block <$> N.nonEmpty blockPointers)
            , TB.fromText $ maybe "" ("\n" <>) blockBody
            ]

-- | The source lines for a block.
prettySourceLines
    :: Convert source error
    -> S.Seq source
    -> Block
    -> N.NonEmpty Pointer
    -> TB.Builder
prettySourceLines (Convert {..}) slines (Block {..}) lspans = unsplit "\n" sourceLines
    where
        Style {..} = blockStyle

        -- Min and max line numbers, as well padding size before the line prefix.
        minLine = fst (M.findMin pointersGrouped)
        maxLine = fst (M.findMax pointersGrouped)
        padding = length (show maxLine)

        -- Shows a line in accordance to the style.
        -- We might get a line that's out-of-bounds, usually the EOF line, so we can default to empty.
        showLine :: [(Int, Int)] -> Int -> TB.Builder
        showLine hs n = TB.fromText . maybe "" id . fmap (styleLine hs . convertLine) $ S.lookup (n - 1) slines

        -- Generic prefix without line number.
        prefix = mconcat
            [ replicateB padding " ", " ", TB.fromText styleLinePrefix, " "
            ]

        -- Prefix for omitting lines when spanning many lines.
        omitPrefix = mconcat
            [ TB.fromText styleEllipsis, replicateB (padding - 1) " ", " ", TB.fromText styleLinePrefix, " "
            ]

        -- Prefix with a line number.
        linePrefix :: Int -> TB.Builder
        linePrefix n = mconcat
            [ TB.fromText (styleNumber n), replicateB (padding - length (show n)) " ", " "
            , TB.fromText styleLinePrefix, " "
            ]

        -- The pointers grouped by line.
        pointersGrouped = M.fromListWith (<>) $ map (\x -> (pointerLine x, pure x)) (N.toList lspans)

        -- The resulting source lines.
        -- Extra prefix for padding.
        sourceLines = prefix : makeSourceLines 0 [minLine .. maxLine]

        -- Whether there will be a multiline span.
        hasConnMulti = M.size (M.filter (any pointerConnect) pointersGrouped) > 1

        -- Whether line /n/ has a connection to somewhere else (including the same line).
        hasConn :: Int -> Bool
        hasConn n = maybe False (any pointerConnect) $ M.lookup n pointersGrouped

        -- Whether line /n/ has a connection to a line before or after it (but not including).
        connAround :: Int -> (Bool, Bool)
        connAround n =
            let (a, b) = M.split n pointersGrouped
            in ((any . any) pointerConnect a, (any . any) pointerConnect b)

        -- Makes the source lines.
        -- We have an @extra@ parameter to keep track of extra lines when spanning multiple lines.
        makeSourceLines :: Int -> [Int] -> [TB.Builder]

        -- No lines left.
        makeSourceLines _ [] = []

        -- The next line is a line we have to decorate with pointers.
        makeSourceLines _ (n:ns)
            | Just p <- M.lookup n pointersGrouped = makeDecoratedLines p <> makeSourceLines 0 ns

        -- The next line is an extra line, within a limit (currently 2, may be configurable later).
        makeSourceLines extra (n:ns)
            | extra < 2 =
                let mid = if
                        | snd (connAround n) -> TB.fromText styleVertical <> " "
                        | hasConnMulti       -> "  "
                        | otherwise          -> ""
                in (linePrefix n <> mid <> showLine [] n) : makeSourceLines (extra + 1) ns

        -- We reached the extra line limit, so now there's some logic to figure out what's next.
        makeSourceLines _ ns =
            let (es, ns') = break (`M.member` pointersGrouped) ns
            in case (es, ns') of
                -- There were no lines left to decorate anyways.
                (_, []) -> []

                -- There are lines left to decorate, and it came right after.
                ([], _) -> makeSourceLines 0 ns'

                -- There is a single extra line, so we can use that as the before-line.
                -- No need for omission, because it came right before.
                ([n], _) ->
                    let mid = if
                            | snd (connAround n) -> TB.fromText styleVertical <> " "
                            | hasConnMulti       -> "  "
                            | otherwise          -> ""
                    in (linePrefix n <> mid <> showLine [] n) : makeSourceLines 0 ns'

                -- There are more than one line in between, so we omit all but the last.
                -- We use the last one as the before-line.
                (_, _) ->
                    let n = last es
                        mid = if
                            | snd (connAround n) -> TB.fromText styleVertical <> " "
                            | hasConnMulti       -> "  "
                            | otherwise          -> ""
                    in (omitPrefix <> mid) : (linePrefix n <> mid <> showLine [] n) : makeSourceLines 0 ns'

        -- Decorate a line that has pointers.
        -- The pointers we get are assumed to be all on the same line.
        makeDecoratedLines :: N.NonEmpty Pointer -> [TB.Builder]
        makeDecoratedLines pointers = (linePrefix line <> TB.fromText lineConnector <> sline) : decorationLines
            where
                lineConnector = if
                    | hasConnBefore && hasConnUnder -> styleVertical <> " "
                    | hasConnMulti                  -> "  "
                    | otherwise                     -> ""

                -- Shortcuts to where this line connects to.
                hasConnHere = hasConn line
                (hasConnBefore, hasConnAfter) = connAround line
                hasConnAround = hasConnBefore || hasConnAfter
                hasConnOver = hasConnHere || hasConnBefore
                hasConnUnder = hasConnHere || hasConnAfter

                -- The sorted pointers by column.
                -- There's a reverse for when we create decorations.
                pointersSorted = N.fromList . sortOn pointerColumns $ N.toList pointers
                pointersSorted' = N.reverse pointersSorted

                -- The line we're on.
                line = pointerLine $ N.head pointers
                sline = showLine (map pointerColumns (N.toList pointersSorted)) line

                -- The resulting decoration lines.
                decorationLines = if
                    -- There's only one pointer, so no need for more than just an underline and label.
                    | N.length pointersSorted' == 1                           -> [underline pointersSorted']

                    -- There's no labels at all, so we just need the underline.
                    | all (isNothing . pointerLabel) (N.tail pointersSorted') -> [underline pointersSorted']

                    -- Otherwise, we have three steps to do:
                    --   The underline directly underneath.
                    --   An extra connector for the labels other than the rightmost one.
                    --   The remaining connectors and the labels.
                    | otherwise ->
                        let hasLabels = filter (isJust . pointerLabel) $ N.tail pointersSorted'
                        in underline pointersSorted'
                            : connectors hasLabels
                            : parar (\a (rest, xs) -> connectorAndLabel rest a : xs) [] hasLabels

                -- Create an underline directly under the source.
                underline :: N.NonEmpty Pointer -> TB.Builder
                underline ps =
                    let (decor, _) = foldDecorations
                            (\n isFirst rest -> if
                                | isFirst && any pointerConnect rest && hasConnAround -> replicateB n styleHorizontal
                                | isFirst                                             -> replicateB n " "
                                | any pointerConnect rest                             -> replicateB n styleHorizontal
                                | otherwise                                           -> replicateB n " "
                            )
                            ""
                            (\n -> replicateB n styleUnderline)
                            (N.toList ps)
                        lbl = maybe "" (" " <>) . pointerLabel $ N.head ps
                        mid = if
                            | hasConnHere && hasConnBefore && hasConnAfter -> styleUpDownRight <> styleHorizontal
                            | hasConnHere && hasConnBefore                 -> styleUpRight <> styleHorizontal
                            | hasConnHere && hasConnAfter                  -> styleDownRight <> styleHorizontal
                            | hasConnBefore && hasConnAfter                -> styleVertical <> " "
                            | hasConnMulti                                 -> "  "
                            | otherwise -> ""
                    in prefix <> TB.fromText mid <> decor <> TB.fromText lbl

                -- Create connectors underneath.
                -- It's assumed all these pointers have labels.
                connectors :: [Pointer] -> TB.Builder
                connectors ps =
                    let (decor, _) = foldDecorations
                            (\n _ _ -> replicateB n " ")
                            (TB.fromText styleVertical)
                            (\n -> replicateB (n - 1) " ")
                            ps
                        mid = if
                            | hasConnOver && hasConnAfter -> styleVertical <> " "
                            | hasConnMulti                -> "  "
                            | otherwise                   -> ""
                    in prefix <> TB.fromText mid <> decor

                -- Create connectors and labels underneath.
                -- It's assumed all these pointers have labels.
                -- The single pointer passed in is the label to make at the end of the decorations.
                connectorAndLabel :: [Pointer] -> Pointer -> TB.Builder
                connectorAndLabel ps p =
                    let (decor, finalCol) = foldDecorations
                            (\n _ _ -> replicateB n " ")
                            (TB.fromText styleVertical)
                            (\n -> replicateB (n - 1) " ")
                            ps
                        lbl = maybe ""
                            (\x -> mconcat
                                [ replicateB (pointerColStart p - finalCol) " "
                                , TB.fromText styleUpRight
                                , " "
                                , TB.fromText x
                                ]
                            )
                            (pointerLabel p)
                        mid = if
                            | hasConnOver && hasConnAfter -> styleVertical <> " "
                            | hasConnMulti                -> "  "
                            | otherwise                   -> ""
                    in prefix <> TB.fromText mid <> decor <> lbl

-- | Makes a line of decorations below the source.
foldDecorations
    :: (Int -> Bool -> [Pointer] -> TB.Builder) -- ^ Catch up from the previous pointer to this pointer.
    -> TB.Builder                               -- ^ Something in the middle.
    -> (Int -> TB.Builder)                      -- ^ Reach the next pointer.
    -> [Pointer]
    -> (TB.Builder, Int)
foldDecorations catchUp something reachAfter ps =
    let (decor, finalCol, _, _) = foldr
            (\(Pointer {..}) (xs, c, rest, isFirst) ->
                ( mconcat
                    [ xs
                    , catchUp (pointerColStart - c) isFirst rest
                    , something
                    , reachAfter (pointerColEnd - pointerColStart)
                    ]
                , pointerColEnd
                , tail rest
                , False
                )
            )
            ("", 1, reverse ps, True)
            ps
    in (decor, finalCol)

-- | Paramorphism on lists (lazily, from the right).
parar :: (a -> ([a], b) -> b) -> b -> [a] -> b
parar _ b []     = b
parar f b (a:as) = f a (as, parar f b as)

-- | Puts text between each item.
unsplit :: (Semigroup a, IsString a) => a -> [a] -> a
unsplit _ [] = ""
unsplit a (x:xs) = foldl' (\acc y -> acc <> a <> y) x xs

-- | Replicates text into a builder.
replicateB :: Int -> T.Text -> TB.Builder
replicateB n = TB.fromText . T.replicate n
