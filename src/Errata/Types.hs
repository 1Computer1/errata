{-|
Module      : Errata.Types
Copyright   : (c) 2020 comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Type definitions. Most of these are re-exported in "Errata", so you should not need to import this module, unless you
need some of the helper functions for making new functionality on top of Errata.
-}
module Errata.Types
    ( -- * Type synonyms
      Line
    , Column
    , Header
    , Body
    , Label
      -- * Error format data
    , Errata(..)
      -- * Blocks and pointers
    , Block(..)
    , Pointer(..)
    , pointerColumns
      -- * Styling options
    , Style(..)
    , highlight
    ) where

import qualified Data.Text as T

-- | Line number, starts at 1.
type Line = Int

-- | Column number, starts at 1.
type Column = Int

-- | Header text. Generally goes above things.
type Header = T.Text

-- | Body text. Generally goes below things.
type Body = T.Text

-- | Label text. Generally goes inline with things.
type Label = T.Text

-- | A collection of information for pretty printing an error.
data Errata = Errata
    { errataHeader :: Maybe Header -- ^ The message that appears above all the blocks.
    , errataBlocks :: [Block]      -- ^ Blocks in the source code to display.
    , errataBody   :: Maybe Body   -- ^ The message that appears below all the blocks.
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
    , blockLocation :: (FilePath, Line, Column)

      -- | The header message for the block. This will appear below the location and above the source lines.
    , blockHeader :: Maybe Header

      {-|
      The block's pointers. These are used to "point out" parts of the source code in this block.

      The locations of each of these pointers must be non-overlapping. If the pointers are touching at a boundary
      however, that is allowed.
      -}
    , blockPointers :: [Pointer]

      -- | The body message for the block. This will appear below the source lines.
    , blockBody :: Maybe Body
    }

{-|
A pointer is the span of the source code at a line, from one column to another. Each of the positions start at 1.

A pointer may also have a label that will display inline.

A pointer may also be connected to all the other pointers within the same block.
-}
data Pointer = Pointer
    { pointerLine     :: Line        -- ^ The line of the pointer.
    , pointerColStart :: Column      -- ^ The starting column of the pointer.
    , pointerColEnd   :: Column      -- ^ The ending column of the pointer.
    , pointerConnect  :: Bool        -- ^ Whether this pointer connects with other pointers.
    , pointerLabel    :: Maybe Label -- ^ An optional label for the pointer.
    }
    deriving (Show, Eq)

-- | Gets the column span for a 'Pointer'.
pointerColumns :: Pointer -> (Line, Column)
pointerColumns p = (pointerColStart p, pointerColEnd p)

-- | Stylization options for a block, e.g. characters to use.
data Style = Style
    { -- | Shows the location of a block at a file, line, and column.
      styleLocation :: (FilePath, Line, Column) -> T.Text

      -- | Shows the line number /n/ for a source line. The result should visually be the same length as just @show n@.
    , styleNumber :: Line -> T.Text

      {-|
      Stylize a source line.

      Column pointers of the text that are being underlined are given for highlighting purposes. The result of this
      should visually take up the same space as the original line.
      -}
    , styleLine :: [(Column, Column)] -> T.Text -> T.Text

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

-- | Adds highlighting to spans of text by enclosing it with some text e.g ANSI escape codes.
highlight
    :: T.Text             -- ^ Text to add before.
    -> T.Text             -- ^ Text to add after.
    -> [(Column, Column)] -- ^ Indices to enclose. These are column spans, starting at 1. They must not overlap.
    -> T.Text             -- ^ Text to highlight.
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
