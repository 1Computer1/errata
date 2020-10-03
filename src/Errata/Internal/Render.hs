{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-|
Module      : Errata.Internal.Render
Copyright   : (c) 2020 comp
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
    ) where

import           Control.Applicative
import qualified Data.IntMap as I
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import           Errata.Source
import           Errata.Types

-- | Renders errors.
renderErrors :: Source source => source -> [Errata] -> TB.Builder
renderErrors source errs = unsplit "\n\n" prettified
    where
        slines = sourceToLines source
        prettified = map (renderErrata slines) errs

-- | A single pretty error from metadata and source lines.
renderErrata :: Source source => [source] -> Errata -> TB.Builder
renderErrata slines (Errata {..}) = errorMessage
    where
        -- The pointers grouped by line.
        blockPointersGrouped = I.fromListWith (<>)
          . map (\p -> (pointerLine p, pure p))
          . blockPointers <$> errataBlocks

        -- Min and max line numbers as defined by the pointers of each block.
        minPointers = maybe 1 id . fmap fst . I.lookupMin <$> blockPointersGrouped
        maxPointers = maybe 0 id . fmap fst . I.lookupMax <$> blockPointersGrouped

        minLine = minimum minPointers
        maxLine = maximum maxPointers

        -- Turns a list into a list of tail slices of the original list,
        -- each element at index @i@ dropping the first @i@ elements of the original list
        -- and tailing a monoidal unit 'mempty'. This allows for correct behavior on
        -- out-of-source-bounds pointers.
        slices :: Monoid a => [a] -> [[a]]
        slices [] = repeat (repeat mempty)
        slices xs = (xs <> repeat mempty) : slices (tail xs)

        {-
          Optimization: we use a Patricia tree (IntMap) indexed by start line
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
            O(n + log n) worst case, O(log n) average case, seeking per source block,
              O(n) traversal
          Worse case is unevaluated slices, as this would force @slices@ evaluation, which is
          an O(n) list traversal, on top of an O(log n) Patricia lookup. Partially-evaluated leafs will
          have slightly better asymptotics, and fully-evaluated leafs will be O(log n) average case,
          which is just the cost of a Patricia lookup.

          For sufficiently large block counts with scattered pointers per block, which we assume
          holds for real-world use cases, the traversal savings on repeat lookups will quickly favor
          hybrid association list + IntMap asymptotics.
        -}
        srcTable = I.fromDistinctAscList
          (zip [minLine..maxLine] (drop (minLine - 1) (slices slines)))

        blockMessages = getZipList $
            renderBlock srcTable
                <$> ZipList errataBlocks
                <*> ZipList blockPointersGrouped
                <*> ZipList (zip minPointers maxPointers)

        errorMessage = mconcat
            [ TB.fromText $ maybe "" id errataHeader
            , case blockMessages of
                [] -> ""
                xs -> "\n" <> unsplit "\n\n" xs
            , TB.fromText $ maybe "" ("\n" <>) errataBody
            ]

-- | A single pretty block from block data and source lines.
renderBlock
  :: Source source
  => I.IntMap [source]
  -> Block
  -> I.IntMap [Pointer]
  -> (Line, Line)
  -> TB.Builder
renderBlock srcTable block@(Block {..}) blockPointersGrouped ~(minBlockLine, maxBlockLine) = blockMessage
    where
        slines = zip [minBlockLine..maxBlockLine]
          (maybe [] id $ I.lookup minBlockLine srcTable)

        -- Padding size before the line prefix.
        padding = length (show maxBlockLine)

        blockMessage = mconcat
            [ TB.fromText $ styleLocation blockStyle blockLocation
            , TB.fromText $ maybe "" ("\n" <>) blockHeader
            , maybe "" ("\n" <>) $ renderSourceLines slines block padding blockPointersGrouped
            , TB.fromText $ maybe "" ("\n" <>) blockBody
            ]

-- | The source lines for a block.
renderSourceLines
    :: forall source. Source source
    => [(Line, source)]
    -> Block
    -> Int
    -> I.IntMap [Pointer]
    -> Maybe (TB.Builder)
renderSourceLines _ _ _ (I.null -> True) = Nothing
renderSourceLines slines (Block {..}) padding pointersGrouped = Just $ unsplit "\n" sourceLines
    where
        Style {..} = blockStyle

        -- Shows a line in accordance to the style.
        -- We might get a line that's out-of-bounds, usually the EOF line, so we can default to empty.
        showLine :: [(Column, Column)] -> source -> TB.Builder
        showLine hs = TB.fromText . styleLine hs . sourceToText

        -- Generic prefix without line number.
        prefix = mconcat
            [ replicateB padding " ", " ", TB.fromText styleLinePrefix, " "
            ]

        -- Prefix for omitting lines when spanning many lines.
        omitPrefix = mconcat
            [ TB.fromText styleEllipsis, replicateB (padding - 1) " ", " ", TB.fromText styleLinePrefix, " "
            ]

        -- Prefix with a line number.
        linePrefix :: Line -> TB.Builder
        linePrefix n = mconcat
            [ TB.fromText (styleNumber n), replicateB (padding - length (show n)) " ", " "
            , TB.fromText styleLinePrefix, " "
            ]

        -- The resulting source lines.
        -- Extra prefix for padding.
        sourceLines = mconcat [replicateB padding " ", " ", TB.fromText styleLinePrefix]
            : makeSourceLines 0 slines

        -- Whether there will be a multiline span.
        hasConnMulti = I.size (I.filter (any pointerConnect) pointersGrouped) > 1

        -- Whether line /n/ has a connection to somewhere else (including the same line).
        hasConn :: Line -> Bool
        hasConn n = maybe False (any pointerConnect) $ I.lookup n pointersGrouped

        -- Whether line /n/ has a connection to a line before or after it (but not including).
        connAround :: Line -> (Bool, Bool)
        connAround n =
            let (a, b) = I.split n pointersGrouped
            in ((any . any) pointerConnect a, (any . any) pointerConnect b)

        -- Makes the source lines.
        -- We have an @extra@ parameter to keep track of extra lines when spanning multiple lines.
        makeSourceLines :: Line -> [(Line, source)] -> [TB.Builder]

        -- No lines left.
        makeSourceLines _ [] = []

        -- The next line is a line we have to decorate with pointers.
        makeSourceLines _ (pr@(n, _):ns)
            | Just p <- I.lookup n pointersGrouped = makeDecoratedLines p pr <> makeSourceLines 0 ns

        -- The next line is an extra line, within a limit (currently 2, may be configurable later).
        makeSourceLines extra ((n, l):ns)
            | extra < 2 =
                let mid = if
                        | snd (connAround n) -> TB.fromText styleVertical <> " "
                        | hasConnMulti       -> "  "
                        | otherwise          -> ""
                in (linePrefix n <> mid <> showLine [] l) : makeSourceLines (extra + 1) ns

        -- We reached the extra line limit, so now there's some logic to figure out what's next.
        makeSourceLines _ ns =
            let (es, ns') = break ((`I.member` pointersGrouped) . fst) ns
            in case (es, ns') of
                -- There were no lines left to decorate anyways.
                (_, []) -> []

                -- There are lines left to decorate, and it came right after.
                ([], _) -> makeSourceLines 0 ns'

                -- There is a single extra line, so we can use that as the before-line.
                -- No need for omission, because it came right before.
                ([(n, l)], _) ->
                    let mid = if
                            | snd (connAround n) -> TB.fromText styleVertical <> " "
                            | hasConnMulti       -> "  "
                            | otherwise          -> ""
                    in (linePrefix n <> mid <> showLine [] l) : makeSourceLines 0 ns'

                -- There are more than one line in between, so we omit all but the last.
                -- We use the last one as the before-line.
                (_, _) ->
                    let (n, l) = last es
                        mid = if
                            | snd (connAround n) -> TB.fromText styleVertical <> " "
                            | hasConnMulti       -> "  "
                            | otherwise          -> ""
                    in (omitPrefix <> mid) : (linePrefix n <> mid <> showLine [] l) : makeSourceLines 0 ns'

        -- Decorate a line that has pointers.
        -- The pointers we get are assumed to be all on the same line.
        makeDecoratedLines :: [Pointer] -> (Line, source) -> [TB.Builder]
        makeDecoratedLines pointers (num, line) =
            (linePrefix num <> TB.fromText lineConnector <> sline) : decorationLines
            where
                lineConnector = if
                    | hasConnBefore && hasConnUnder -> styleVertical <> " "
                    | hasConnMulti                  -> "  "
                    | otherwise                     -> ""

                -- Shortcuts to where this line connects to.
                hasConnHere = hasConn num
                (hasConnBefore, hasConnAfter) = connAround num
                hasConnAround = hasConnBefore || hasConnAfter
                hasConnOver = hasConnHere || hasConnBefore
                hasConnUnder = hasConnHere || hasConnAfter

                -- The sorted pointers by column.
                pointersSorted = sortOn pointerColumns pointers

                -- The line number we're on.
                sline = showLine (map pointerColumns pointersSorted) line

                -- The resulting decoration lines.
                decorationLines = case filter (isJust . pointerLabel) (init pointersSorted) of
                    -- There's only one pointer, so no need for more than just an underline and label.
                    _ | length pointersSorted == 1 -> [underline pointersSorted]

                    -- There's no labels at all, so we just need the underline.
                    [] -> [underline pointersSorted]

                    -- Otherwise, we have three steps to do:
                    --   The underline directly underneath.
                    --   An extra connector for the labels other than the rightmost one.
                    --   The remaining connectors and the labels.
                    hasLabels -> underline pointersSorted
                        : connectors hasLabels
                        : (map connectorAndLabel . reverse . tail $ inits hasLabels)

                -- Create an underline directly under the source. The last pointer can have a label on this line.
                underline :: [Pointer] -> TB.Builder
                underline ps =
                    let (decor, _) = foldDecorations
                            (\n isFirst rest -> if
                                | isFirst && any pointerConnect rest && hasConnAround -> replicateB n styleHorizontal
                                | isFirst -> replicateB n " "
                                | any pointerConnect rest -> replicateB n styleHorizontal
                                | otherwise -> replicateB n " "
                            )
                            (\n -> (n, replicateB n styleUnderline))
                            ps
                        lbl = maybe "" (" " <>) . pointerLabel $ last ps
                        mid = if
                            | hasConnHere && hasConnBefore && hasConnAfter -> styleUpDownRight <> styleHorizontal
                            | hasConnHere && hasConnBefore                 -> styleUpRight <> styleHorizontal
                            | hasConnHere && hasConnAfter                  -> styleDownRight <> styleHorizontal
                            | hasConnBefore && hasConnAfter                -> styleVertical <> " "
                            | hasConnMulti                                 -> "  "
                            | otherwise                                    -> ""
                    in prefix <> TB.fromText mid <> decor <> TB.fromText lbl

                -- Create connectors underneath. No labels are rendered here.
                connectors :: [Pointer] -> TB.Builder
                connectors ps =
                    let (decor, _) = foldDecorations
                            (\n _ _ -> replicateB n " ")
                            (\_ -> (1, TB.fromText styleVertical))
                            ps
                        mid = if
                            | hasConnOver && hasConnAfter -> styleVertical <> " "
                            | hasConnMulti                -> "  "
                            | otherwise                   -> ""
                    in prefix <> TB.fromText mid <> decor

                -- Create connectors and labels underneath. The last pointer can have a label on this line.
                connectorAndLabel :: [Pointer] -> TB.Builder
                connectorAndLabel ps =
                    let (decor, finalCol) = foldDecorations
                            (\n _ _ -> replicateB n " ")
                            (\_ -> (1, TB.fromText styleVertical))
                            (init ps)
                        lbl = maybe ""
                            (\x -> mconcat
                                [ replicateB (pointerColStart (last ps) - finalCol) " "
                                , TB.fromText styleUpRight
                                , " "
                                , TB.fromText x
                                ]
                            )
                            (pointerLabel (last ps))
                        mid = if
                            | hasConnOver && hasConnAfter -> styleVertical <> " "
                            | hasConnMulti                -> "  "
                            | otherwise                   -> ""
                    in prefix <> TB.fromText mid <> decor <> lbl

-- | Makes a line of decorations below the source.
foldDecorations
    :: (Column -> Bool -> [Pointer] -> TB.Builder) -- ^ Catch up from the previous pointer to this pointer.
    -> (Column -> (Int, TB.Builder))               -- ^ Something in the middle that ends before the next pointer.
    -> [Pointer]
    -> (TB.Builder, Column)
foldDecorations catchUp reachAfter ps =
    let (decor, finalCol, _) = paral
            (\(rest, (xs, c, isFirst)) p@(Pointer {..}) ->
                let (afterLen, afterText) = reachAfter (pointerColEnd - pointerColStart)
                in
                ( mconcat
                    [ xs
                    , catchUp (pointerColStart - c) isFirst (p:rest)
                    , afterText
                    ]
                , pointerColStart + afterLen
                , False
                )
            )
            ("", 1, True)
            ps
    in (decor, finalCol)

-- | Paramorphism on lists (strictly, from the left).
paral :: (([a], b) -> a -> b) -> b -> [a] -> b
paral _ b [] = b
paral f b (a:as) =
    let !b' = f (as, b) a
    in paral f b' as

-- | Puts text between each item.
unsplit :: TB.Builder -> [TB.Builder] -> TB.Builder
unsplit _ [] = ""
unsplit a (x:xs) = foldl' (\acc y -> acc <> a <> y) x xs
{-# INLINE unsplit #-}

-- | Replicates text into a builder.
replicateB :: Int -> T.Text -> TB.Builder
replicateB n xs = TB.fromText (T.replicate n xs)
{-# INLINE replicateB #-}
