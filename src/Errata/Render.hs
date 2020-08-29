{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Errata.Render
Copyright   : (c) 2020 comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Functions for rendering the errors. You should not need to import this, as these functions are lower-level.

This module is internal, and may break across non-breaking versions.
-}
module Errata.Render
    ( renderErrors
    , renderErrata
    , renderBlock
    , renderSourceLines
    ) where

import           Data.List
import qualified Data.List.NonEmpty as N
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import           Errata.Types

-- | Renders errors.
renderErrors :: Convert source -> source -> [Errata] -> TB.Builder
renderErrors ef@(Convert {..}) source errs = unsplit "\n\n" prettified
    where
        sortedErrata = sortOn (\(Errata {..}) -> blockLocation errataBlock) $ errs

        -- We may arbitrarily index the source lines a lot, so a Seq is appropriate.
        -- If push comes to shove, this could be replaced with an 'Array' or a 'Vec'.
        slines = S.fromList (convertLines source)
        prettified = map (renderErrata ef slines) sortedErrata

-- | A single pretty error from metadata and source lines.
renderErrata :: Convert source -> S.Seq source -> Errata -> TB.Builder
renderErrata ef@(Convert {..}) slines (Errata {..}) = errorMessage
    where
        errorMessage = mconcat
            [ TB.fromText $ maybe "" (<> "\n") errataHeader
            , unsplit "\n\n" (map (renderBlock ef slines) (errataBlock : errataBlocks))
            , TB.fromText $ maybe "" ("\n\n" <>) errataBody
            ]

-- | A single pretty block from block data and source lines.
renderBlock :: Convert source -> S.Seq source -> Block -> TB.Builder
renderBlock ef@(Convert {..}) slines block@(Block {..}) = blockMessage
    where
        blockMessage = mconcat
            [ TB.fromText $ styleLocation blockStyle blockLocation
            , maybe "" ("\n" <>) (renderSourceLines ef slines block <$> N.nonEmpty blockPointers)
            , TB.fromText $ maybe "" ("\n" <>) blockBody
            ]

-- | The source lines for a block.
renderSourceLines
    :: Convert source
    -> S.Seq source
    -> Block
    -> N.NonEmpty Pointer
    -> TB.Builder
renderSourceLines (Convert {..}) slines (Block {..}) lspans = unsplit "\n" sourceLines
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
