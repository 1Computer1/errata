{-# LANGUAGE RecordWildCards #-}

{- |
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
    , Errata (..)
      -- * Blocks and pointers
    , Block (..)
    , Pointer (..)
    , pointerColumns
    , pointerData
      -- * Styling options
    , Style (..)
    , PointerStyle (..)
    ) where

import qualified Data.Text as T

-- | Line number, starts at 1, increments every new line character.
type Line = Int

-- | Column number, starts at 1, increments every 'Char'.
type Column = Int

-- | Header text. Generally goes above things.
type Header = T.Text

-- | Body text. Generally goes below things.
type Body = T.Text

-- | Label text. Generally goes inline with things.
type Label = T.Text

-- | A collection of information for pretty printing an error.
data Errata = Errata
    { errataHeader :: Maybe Header
      -- ^ The message that appears above all the blocks.
    , errataBlocks :: [Block]
      -- ^ Blocks in the source code to display.
    , errataBody :: Maybe Body
      -- ^ The message that appears below all the blocks.
    }

{- | Information about a block in the source code, such as pointers and messages.

Each block has a style associated with it.
-}
data Block = Block
    { blockStyle :: Style
      -- ^ The style of the block.
    , blockLocation :: (FilePath, Line, Column)
      {- ^ The filepath, line, and column of the block. These start at 1.

      This is used to create the text that details the location.
      -}
    , blockHeader :: Maybe Header
      {- ^ The header message for the block.

      This will appear below the location and above the source lines.
      -}
    , blockPointers :: [Pointer]
      {- ^ The block's pointers. These are used to "point out" parts of the source code in this block.

      The locations of each of these pointers must be non-overlapping. If the pointers are touching at a boundary
      however, that is allowed.
      -}
    , blockBody :: Maybe Body
      {- ^ The body message for the block.

      This will appear below the source lines.
      -}
    }

{- | A pointer is the span of the source code at a line, from one column to another. Each of the positions start at 1.

A pointer may also have a label that will display inline.

A pointer may also be connected to all the other pointers within the same block.
-}
data Pointer = Pointer
    { pointerLine :: Line
      -- ^ The line of the pointer.
    , pointerColStart :: Column
      -- ^ The starting column of the pointer.
    , pointerColEnd :: Column
      -- ^ The ending column of the pointer.
    , pointerConnect :: Bool
      -- ^ Whether this pointer connects with other pointers.
    , pointerLabel :: Maybe Label
      -- ^ An optional label for the pointer.
    , pointerStyle :: PointerStyle
      -- ^ A style for this pointer.
    }

-- | Gets the column span for a 'Pointer'.
pointerColumns :: Pointer -> (Column, Column)
pointerColumns (Pointer {..}) = (pointerColStart, pointerColEnd)

-- | Gets physical information about a pointer.
pointerData :: Pointer -> (Line, Column, Column, Bool, Maybe Label)
pointerData (Pointer {..}) = (pointerLine, pointerColStart, pointerColEnd, pointerConnect, pointerLabel)

-- | Stylization options for a block, e.g. characters to use.
data Style = Style
    { styleLocation :: (FilePath, Line, Column) -> T.Text
      {- ^ Shows the location of a block at a file, line, and column.

      This is put on its own line just above the source lines.
      -}
    , styleNumber :: Line -> T.Text
      {- ^ Shows the line number /n/ for a source line.

      The result should visually be the same length as just @show n@.
      -}
    , styleLine :: [(PointerStyle, (Column, Column))] -> T.Text -> T.Text
      {- ^ Stylize a source line.

      The style and the column span (sorted, starting at 1) of the text that is being underlined are given for
      highlighting purposes (see 'Errata.Styles.highlight').
      They can be ignored for source code highlighting instead, for example.
      The result of this should visually take up the same space as the original line.
      -}
    , styleEllipsis :: T.Text
      {- ^ The text to use as an ellipsis in the position of line numbers for when lines are omitted.

      This should visually be one character.
      -}
    , styleLinePrefix :: T.Text
      {- ^ The prefix before the source lines.

      Before it may be the line number, and after it the source line.
      -}
    , styleVertical :: T.Text
      {- ^ The text to use as a vertical bar when connecting pointers.

      This should visually be one character.
      -}
    , styleHorizontal :: T.Text
      {- ^ The text to use as a horizontal bar when connecting pointers.

      This should visually be one character.
      -}
    , styleDownRight :: T.Text
      {- ^ The text to use as a connector downwards and rightwards when connecting pointers.

      This should visually be one character.
      -}
    , styleUpRight :: T.Text
      {- ^ The text to use as a connector upwards and rightwards when connecting pointers.

      This should visually be one character.
      -}
    , styleUpDownRight :: T.Text
      {- ^ The text to use as a connector upwards, downwards, and rightwards when connecting pointers.

      This should visually be one character.
      -}
    , styleTabWidth :: Int
      {- ^ The number of spaces a tab character is equivalent to.

      Your source will have tabs replaced with this many spaces.
      -}
    }

-- | Stylization options for an individual pointer, e.g. characters to use.
data PointerStyle = PointerStyle
  { styleHighlight :: T.Text -> T.Text
    {- ^ Stylize the text that this pointer is underlining.

    This is only used if 'styleLine' uses the given pointer styles, for example with 'highlight'.
    The result of this should visually take up the same space as the original text.
    -}
  , styleUnderline :: T.Text
    {- ^ The text to underline a character in a pointer.

    This should visually be one character.
    -}
  , styleHook :: T.Text
    {- ^ The text to use as a connector upwards and hooking to the right for the label of a pointer that drops down.

    This probably looks best as one character.
    -}
  , styleConnector :: T.Text
    {- ^ The text to use as a vertical bar when connecting a pointer that drops down to its label.

    This should visually be one character.
    -}
  }
