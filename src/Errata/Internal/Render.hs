{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{- |
Module      : Errata.Internal.Render
Copyright   : (c) 2020- comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Functions for rendering the errors. You should not need to import this, as these functions are lower-level.

This module is internal, and may break across non-breaking versions.
-}
module Errata.Internal.Render
    ( renderErrors
    , renderErrata
    , renderBlock
    , renderSourceLines
    , groupBlockPointers
    , slices
    , makeSourceTable
    ) where

import           Control.Applicative (ZipList (..))
import           Control.Arrow ((&&&))
import qualified Data.IntMap as I
import           Data.List (foldl', inits, sortOn)
import           Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import           Errata.Source
import           Errata.Types

#if defined(usewcwidth)
import           Foreign.C
#endif

-- | Renders a collection of 'Errata'.
renderErrors :: Source source => source -> [Errata] -> TB.Builder
renderErrors source errs = errorMessage
    where
        -- The pointers grouped by line, for each Errata.
        blockPointersGrouped = map (map groupBlockPointers . errataBlocks) errs

        -- Min and max line numbers as defined by the pointers of each block, for each Errata.
        minPointers = (map . map) (maybe 1 id . fmap fst . I.lookupMin) blockPointersGrouped
        maxPointers = (map . map) (maybe 0 id . fmap fst . I.lookupMax) blockPointersGrouped

        minLine = minimum (concat minPointers)
        maxLine = maximum (concat maxPointers)

        {- Optimization: we use a Patricia tree (IntMap) indexed by start line
        into respective tail slices of the list of source lines @slines@.

        If we were to use the list @slines@ as-is:
            O(n) seeking per source block, O(n) traversal
        Since, we would be linearly traversing to the start of each source block every
        time with no caching for future source blocks at (or close to) the same starting
        line as previous source blocks.

        If we were to use an IntMap of source lines by itself:
            Seeking becomes free, at the expense of O(n log n) traversal per source block
        Since, we are performing an O(log n) average case Patricia lookup per line.

        Whereas if we use a hybrid IntMap + association list approach:
            O(n + log n) worst case, O(log n) average case, seeking per source block, O(n) traversal
        Worse case is unevaluated slices, as this would force @slices@ evaluation, which is
        an O(n) list traversal, on top of an O(log n) Patricia lookup. Partially-evaluated leafs will
        have slightly better asymptotics, and fully-evaluated leafs will be O(log n) average case,
        which is just the cost of a Patricia lookup.

        For sufficiently large block counts with scattered pointers per block, which we assume
        holds for real-world use cases, the traversal savings on repeat lookups will quickly favor
        hybrid association list + IntMap asymptotics.
        -}
        srcTable = makeSourceTable minLine maxLine (sourceToLines source)

        errataMessages = getZipList $ renderErrata srcTable
            <$> ZipList errs
            <*> ZipList blockPointersGrouped
            <*> ZipList minPointers
            <*> ZipList maxPointers

        errorMessage = unsplit "\n\n" errataMessages

-- | Group the pointers of a block by the line they appear on.
groupBlockPointers :: Block -> I.IntMap [Pointer]
groupBlockPointers = I.fromListWith (<>) . map (\p -> (pointerLine p, pure p)) . blockPointers

-- | Create a source table from the given line span and source lines.
makeSourceTable :: Source a => Line -> Line -> [a] -> I.IntMap [a]
makeSourceTable minLine maxLine slines = I.fromDistinctAscList $
    zip [minLine .. maxLine] (drop (minLine - 1) (slices slines))

{- | Turns a list into a list of tail slices of the original list, with each element at index @i@ dropping
the first @i@ elements of the original list and tailing an 'emptySource'.

This allows for correct behavior on out-of-source-bounds pointers.
-}
slices :: Source a => [a] -> [[a]]
slices [] = repeat (repeat emptySource)
slices xs = (xs <> repeat emptySource) : slices (tail xs)

-- | Renders a single 'Errata'.
renderErrata
    :: Source source
    => I.IntMap [source]    -- ^ The source table.
    -> Errata               -- ^ The 'Errata' to render.
    -> [I.IntMap [Pointer]] -- ^ The pointers of each block grouped by line.
    -> [Line]               -- ^ The mininum line of each block.
    -> [Line]               -- ^ The maxinum line of each block.
    -> TB.Builder
renderErrata srcTable (Errata {..}) blockPointersGrouped minPointers maxPointers = errorMessage
    where
        blockMessages = getZipList $ renderBlock srcTable
            <$> ZipList errataBlocks
            <*> ZipList blockPointersGrouped
            <*> ZipList (zip minPointers maxPointers)

        errorMessage = mconcat
            [ TB.fromText $ maybe "" id errataHeader
            , case blockMessages of
                [] -> ""
                xs -> case errataHeader of
                    Nothing -> unsplit "\n\n" xs
                    Just _  -> "\n" <> unsplit "\n\n" xs
            , TB.fromText $ maybe "" ("\n" <>) errataBody
            ]

-- | Renders a single block.
renderBlock
    :: Source source
    => I.IntMap [source]  -- ^ The source table.
    -> Block              -- ^ The block to render.
    -> I.IntMap [Pointer] -- ^ The pointers of this block grouped by line.
    -> (Line, Line)       -- ^ The mininum and maximum lines of this block.
    -> TB.Builder
renderBlock srcTable block@(Block {..}) blockPointersGrouped ~(minBlockLine, maxBlockLine) = blockMessage
    where
        slines = zip [minBlockLine .. maxBlockLine] (maybe [] id $ I.lookup minBlockLine srcTable)

        -- Padding size before the line prefix.
        padding = length (show maxBlockLine)

        blockMessage = mconcat
            [ TB.fromText $ styleLocation blockStyle blockLocation
            , TB.fromText $ maybe "" ("\n" <>) blockHeader
            , maybe "" ("\n" <>) $ renderSourceLines slines block padding blockPointersGrouped
            , TB.fromText $ maybe "" ("\n" <>) blockBody
            ]

-- | Renders the source lines for a block.
renderSourceLines
    :: forall source
    .  Source source
    => [(Line, source)]   -- ^ The source lines, from the minimum line to the maximum line for the block.
    -> Block              -- ^ The block to render.
    -> Int                -- ^ The length of the actual number of the maximum line.
    -> I.IntMap [Pointer] -- ^ The pointers of this block grouped by line.
    -> Maybe (TB.Builder)
renderSourceLines _ _ _ (I.null -> True) = Nothing
renderSourceLines slines (Block {..}) padding pointersGrouped = Just $ unsplit "\n" decoratedLines
    where
        {- Terminology used in this code:
            ↓↓ gutter
          │    ← padding line
        1 │   line 1 foo bar do
          │ ┌────────^───────^^
          │ │        │ ← connector
          │ │ hook → └ hi ← label
        2 │ │ line 2
        3 │ │ line 3
          │ ├──────^
        4 │ │ line 4 ← extra line
        5 │ │ line 5 ← extra line
        . │ │ ← omission
        7 │ │ line 7 ← extra line
        8 │ │ line 8 baz end
          │ └──────^─────^^^ ← underline
        ↑↑↑↑        ↑↑↑↑↑
        prefix      catch up
        -}
        Style {..} = blockStyle

        -- Shows a line in accordance to the style.
        -- We might get a line that's out-of-bounds, usually the EOF line, so we can default to empty.
        showLine :: [(PointerStyle, (Column, Column))] -> source -> TB.Builder
        showLine hs = TB.fromText . T.replace "\t" (T.replicate styleTabWidth " ") . styleLine hs . sourceToText

        -- Generic prefix without line number, used for non-source lines i.e. decorations.
        prefix :: TB.Builder
        prefix = if styleEnableLinePrefix
            then mconcat [replicateB padding " ", " ", TB.fromText styleLinePrefix, " "]
            else ""

        -- Prefix with a line number, used for source lines.
        linePrefix :: Line -> TB.Builder
        linePrefix n = if styleEnableLinePrefix
            then mconcat [TB.fromText (styleNumber n), replicateB (padding - length (show n)) " ", " ", TB.fromText styleLinePrefix, " "]
            else ""

        -- The resulting source lines with decorations; extra prefix included for padding.
        decoratedLines :: [TB.Builder]
        decoratedLines = [paddingLine | stylePaddingTop] <> makeDecoratedLines 0 slines<> [paddingLine | stylePaddingBottom]
            where
                paddingLine = if styleEnableLinePrefix
                    then mconcat [replicateB padding " ", " ", TB.fromText styleLinePrefix]
                    else ""

        -- Whether there will be a multiline span in the block.
        hasConnMulti :: Bool
        hasConnMulti = I.size (I.filter (any pointerConnect) pointersGrouped) > 1

        -- Whether line /n/ has a connection to somewhere else (including the same line).
        hasConn :: Line -> Bool
        hasConn n = maybe False (any pointerConnect) $ I.lookup n pointersGrouped

        -- Whether line /n/ has a connection to a line before or after it (but not including).
        connAround :: Line -> (Bool, Bool)
        connAround n =
            let (a, b) = I.split n pointersGrouped
            in ((any . any) pointerConnect a, (any . any) pointerConnect b)

        -- Decorates all the pointed-to source lines, along with extra lines.
        -- We have an @extra@ parameter to keep track of extra lines when spanning multiple lines.
        makeDecoratedLines :: Line -> [(Line, source)] -> [TB.Builder]
        -- No lines left.
        makeDecoratedLines _ [] = []
        -- The next line is a line we have to decorate with pointers.
        makeDecoratedLines _ (pr@(n, _):ls)
            | Just p <- I.lookup n pointersGrouped = decorateLine p pr <> makeDecoratedLines 0 ls
        -- The next line is an extra line, within a limit.
        makeDecoratedLines extra ((n, l):ls)
            | extra < styleExtraLinesAfter =
                let mid = if
                        | not styleEnableDecorations -> ""
                        | snd (connAround n)         -> TB.fromText styleVertical <> " "
                        | hasConnMulti               -> "  "
                        | otherwise                  -> ""
                in (linePrefix n <> mid <> showLine [] l) : makeDecoratedLines (extra + 1) ls
        -- We reached the extra line limit, so now there's some logic to figure out what's next.
        makeDecoratedLines _ ls =
            let (es, ls') = break ((`I.member` pointersGrouped) . fst) ls
            in case (es, ls') of
                -- There were no lines left to decorate anyways.
                (_, []) -> []
                -- There are lines left to decorate, and it came right after.
                ([], _) -> makeDecoratedLines 0 ls'
                -- There are more than one line in between, so we take as much as is configured.
                (_, _) ->
                    let es' = reverse . take styleExtraLinesBefore . reverse $ es
                        extras = flip map es' $ \(n, l) ->
                            let gutter = if
                                    | not styleEnableDecorations -> ""
                                    | snd (connAround n)         -> TB.fromText styleVertical <> " "
                                    | hasConnMulti               -> "  "
                                    | otherwise                  -> ""
                            in linePrefix n <> gutter <> showLine [] l
                    in case compareLength es' es of
                        -- We only add the omission line if it doesn't take all of the lines.
                        LT -> let
                            -- Prefix and gutter for omitting lines when spanning many lines.
                            omitPrefix = if styleEnableLinePrefix
                                then mconcat [TB.fromText styleEllipsis, replicateB (padding - 1) " ", " ", TB.fromText styleLinePrefix]
                                else ""
                            omitGutter = if
                                | not styleEnableDecorations       -> ""
                                | snd . connAround . fst $ head ls -> " " <> TB.fromText styleVertical
                                | otherwise                        -> ""
                            in (omitPrefix <> omitGutter) : extras <> makeDecoratedLines 0 ls'
                        _ -> extras <> makeDecoratedLines 0 ls'

        -- Decorate a line that has pointers.
        -- The pointers we get are assumed to be all on the same line.
        decorateLine :: [Pointer] -> (Line, source) -> [TB.Builder]
        decorateLine pointers (n, l) = (linePrefix n <> gutter <> stylizedLine) : decorationLines
            where
                gutter = if
                    | not styleEnableDecorations    -> ""
                    | hasConnBefore && hasConnUnder -> TB.fromText styleVertical <> " "
                    | hasConnMulti                  -> "  "
                    | otherwise                     -> ""

                -- Shortcuts to where this line connects to.
                hasConnHere = hasConn n
                (hasConnBefore, hasConnAfter) = connAround n
                hasConnAround = hasConnBefore || hasConnAfter
                hasConnOver = hasConnHere || hasConnBefore
                hasConnUnder = hasConnHere || hasConnAfter

                -- The sorted pointers by column.
                pointersSorted = sortOn pointerColumns pointers

                -- The actual source line.
                sourceLine = sourceToText l

                -- The source line stylized.
                stylizedLine = showLine (map (pointerStyle &&& pointerColumns) pointersSorted) l

                -- The resulting decoration lines.
                decorationLines = case filter (isJust . pointerLabel) (init pointersSorted) of
                    _ | not styleEnableDecorations -> []
                    -- There's only one pointer, so no need for more than just an underline and label.
                    _ | length pointersSorted == 1 -> [underline pointersSorted]
                    -- There's no labels at all, so we just need the underline.
                    [] -> [underline pointersSorted]
                    -- Otherwise, we have three steps to do:
                    -- The underline directly underneath.
                    -- An extra connector for the labels other than the rightmost one.
                    -- The remaining connectors and the labels.
                    hasLabels -> underline pointersSorted
                        : connectors hasLabels
                        : (map connectorAndLabel . reverse . tail $ inits hasLabels)

                -- Create an underline directly under the source. The last pointer can have a label on this line.
                underline :: [Pointer] -> TB.Builder
                underline ps =
                    let (decor, _) = foldDecorations
                            (\k isFirst rest text -> if
                                | isFirst && any pointerConnect rest && hasConnAround -> replaceWithWidth k styleTabWidth text styleHorizontal
                                | isFirst                                             -> replaceWithWidth k styleTabWidth text " "
                                | any pointerConnect rest                             -> replaceWithWidth k styleTabWidth text styleHorizontal
                                | otherwise                                           -> replaceWithWidth k styleTabWidth text " "
                            )
                            (\k p text ->
                                let x = styleUnderline (pointerStyle p)
                                in (k, replaceWithWidth k styleTabWidth text x)
                            )
                            ps
                            sourceLine
                        lbl = maybe "" (" " <>) . pointerLabel $ last ps
                        decorGutter = if
                            | hasConnHere && hasConnBefore && hasConnAfter -> styleUpDownRight <> styleHorizontal
                            | hasConnHere && hasConnBefore                 -> styleUpRight <> styleHorizontal
                            | hasConnHere && hasConnAfter                  -> styleDownRight <> styleHorizontal
                            | hasConnBefore && hasConnAfter                -> styleVertical <> " "
                            | hasConnMulti                                 -> "  "
                            | otherwise                                    -> ""
                    in prefix <> TB.fromText decorGutter <> decor <> TB.fromText lbl

                -- Create connectors underneath. No labels are rendered here.
                connectors :: [Pointer] -> TB.Builder
                connectors ps =
                    let (decor, _) = foldDecorations
                            (\k _ _ text -> replaceWithWidth k styleTabWidth text " ")
                            (\_ p _ ->
                                let x = styleConnector (pointerStyle p)
                                in (1, TB.fromText x)
                            )
                            ps
                            sourceLine
                        decorGutter = if
                            | hasConnOver && hasConnAfter -> styleVertical <> " "
                            | hasConnMulti                -> "  "
                            | otherwise                   -> ""
                    in prefix <> TB.fromText decorGutter <> decor

                -- Create connectors and labels underneath. The last pointer can have a label on this line.
                connectorAndLabel :: [Pointer] -> TB.Builder
                connectorAndLabel ps =
                    let (decor, finalCol) = foldDecorations
                            (\k _ _ text -> replaceWithWidth k styleTabWidth text " ")
                            (\_ p _ ->
                                let x = styleConnector (pointerStyle p)
                                in (1, TB.fromText x)
                            )
                            (init ps)
                            sourceLine
                        pointer = last ps
                        hook = styleHook (pointerStyle pointer)
                        lbl = maybe ""
                            (\x -> if
                                | styleEnableHook (pointerStyle pointer) -> mconcat
                                    [ replicateB (pointerColStart pointer - finalCol) " "
                                    , TB.fromText hook
                                    , " "
                                    , TB.fromText x
                                    ]
                                | otherwise -> mconcat
                                    [ replicateB (pointerColStart pointer - finalCol) " "
                                    , TB.fromText x
                                    ]
                            )
                            (pointerLabel pointer)
                        decorGutter = if
                            | hasConnOver && hasConnAfter -> styleVertical <> " "
                            | hasConnMulti                -> "  "
                            | otherwise                   -> ""
                    in prefix <> TB.fromText decorGutter <> decor <> lbl

-- | Makes a line of decorations below the source.
foldDecorations
    :: (Int -> Bool -> [Pointer] -> T.Text -> TB.Builder)
    {- ^ Catch up from the previous pointer to this pointer.

    @catchUp distance isFirst pointers text@ should return text of at least length @distance@.
    -}
    -> (Int -> Pointer -> T.Text -> (Int, TB.Builder))
    {- ^ Add text underneath the pointer before the next pointer.

    @underlinePointer pointerLen pointer text@ should return the text and its length.
    -}
    -> [Pointer]
    -> T.Text
    -> (TB.Builder, Column)
foldDecorations catchUp underlinePointer ps line =
    let (decor, finalCol, _, _) = paral
            (\(rest, (xs, c, isFirst, remainingLine)) p@(Pointer {..}) ->
                let (textBefore, textUnderAndRest) = T.splitAt (pointerColStart - c) remainingLine
                    (textUnder, textRest) = T.splitAt (pointerColEnd - pointerColStart) textUnderAndRest
                    (afterLen, afterText) = underlinePointer (pointerColEnd - pointerColStart) p textUnder
                in
                ( mconcat
                    [ xs
                    , catchUp (pointerColStart - c) isFirst (p:rest) textBefore
                    , afterText
                    ]
                , pointerColStart + afterLen
                , False
                , textRest
                )
            )
            ("", 1, True, line)
            ps
    in (decor, finalCol)

-- | Paramorphism on lists (strictly, from the left).
paral :: (([a], b) -> a -> b) -> b -> [a] -> b
paral _ b [] = b
paral f b (a:as) =
    let !b' = f (as, b) a
    in paral f b' as

-- | Compares length of two lists without traversing them completely.
compareLength :: [a] -> [b] -> Ordering
compareLength []     []     = EQ
compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength []     _      = LT
compareLength _      []     = GT

-- | Puts text between each item.
unsplit :: TB.Builder -> [TB.Builder] -> TB.Builder
unsplit _ []     = ""
unsplit a (x:xs) = foldl' (\acc y -> acc <> a <> y) x xs
{-# INLINE unsplit #-}

-- | Replicates text into a builder.
replicateB :: Int -> T.Text -> TB.Builder
replicateB n xs = TB.fromText (T.replicate n xs)
{-# INLINE replicateB #-}

{- | Replaces each character in the text with the appropriate instances of the given text based on character width.

The result will also be right-padded with the given text to the given length.

For tabs, the tab width given is used to make it equivalent to that many spaces.
-}
replaceWithWidth :: Int -> Int -> T.Text -> T.Text -> TB.Builder
replaceWithWidth len tab ref xs = T.foldl' (\acc c -> acc <> replicateB (width c) xs) "" ref <> replicateB (len - T.length ref) xs
    where
        width '\t' = tab
        width c = charWidth c
{-# INLINE replaceWithWidth #-}

#if defined(usewcwidth)
foreign import ccall unsafe "wchar.h wcwidth" wcwidth :: CWchar -> CInt
{-| Get the designated render width of a character, based on the native wcwidth.
Where wcwidth would return -1, 0 is returned instead.

The result will depend on the current locale and Unicode version.
-}
charWidth :: Char -> Int
charWidth = max 0 . fromEnum . wcwidth . toEnum . fromEnum
#else
{-| Get the designated render width of a character: 0 for a combining character, 1 for a regular character,
2 for a wide character. (Wide characters are rendered as exactly double width in apps and fonts that support it.)

(From Pandoc.)
-}
charWidth :: Char -> Int
charWidth c = if
    | c < '\x0300'                     -> 1
    -- Combining
    | c >= '\x0300' && c <= '\x036F'   -> 0
    | c >= '\x0370' && c <= '\x10FC'   -> 1
    | c >= '\x1100' && c <= '\x115F'   -> 2
    | c >= '\x1160' && c <= '\x11A2'   -> 1
    | c >= '\x11A3' && c <= '\x11A7'   -> 2
    | c >= '\x11A8' && c <= '\x11F9'   -> 1
    | c >= '\x11FA' && c <= '\x11FF'   -> 2
    | c >= '\x1200' && c <= '\x2328'   -> 1
    | c >= '\x2329' && c <= '\x232A'   -> 2
    | c >= '\x232B' && c <= '\x2E31'   -> 1
    | c >= '\x2E80' && c <= '\x303E'   -> 2
    | c == '\x303F'                    -> 1
    | c >= '\x3041' && c <= '\x3247'   -> 2
    -- Ambiguous
    | c >= '\x3248' && c <= '\x324F'   -> 1
    | c >= '\x3250' && c <= '\x4DBF'   -> 2
    | c >= '\x4DC0' && c <= '\x4DFF'   -> 1
    | c >= '\x4E00' && c <= '\xA4C6'   -> 2
    | c >= '\xA4D0' && c <= '\xA95F'   -> 1
    | c >= '\xA960' && c <= '\xA97C'   -> 2
    | c >= '\xA980' && c <= '\xABF9'   -> 1
    | c >= '\xAC00' && c <= '\xD7FB'   -> 2
    | c >= '\xD800' && c <= '\xDFFF'   -> 1
    -- Ambiguous
    | c >= '\xE000' && c <= '\xF8FF'   -> 1
    | c >= '\xF900' && c <= '\xFAFF'   -> 2
    | c >= '\xFB00' && c <= '\xFDFD'   -> 1
    -- Ambiguous
    | c >= '\xFE00' && c <= '\xFE0F'   -> 1
    | c >= '\xFE10' && c <= '\xFE19'   -> 2
    | c >= '\xFE20' && c <= '\xFE26'   -> 1
    | c >= '\xFE30' && c <= '\xFE6B'   -> 2
    | c >= '\xFE70' && c <= '\xFEFF'   -> 1
    | c >= '\xFF01' && c <= '\xFF60'   -> 2
    | c >= '\xFF61' && c <= '\x16A38'  -> 1
    | c >= '\x1B000' && c <= '\x1B001' -> 2
    | c >= '\x1D000' && c <= '\x1F1FF' -> 1
    | c >= '\x1F200' && c <= '\x1F251' -> 2
    | c >= '\x1F300' && c <= '\x1F773' -> 1
    | c >= '\x20000' && c <= '\x3FFFD' -> 2
    | otherwise                        -> 1
#endif
