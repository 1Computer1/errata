{-|
Module      : Errata.Types
Copyright   : (c) 2020 comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Type definitions. All of these are re-exported in "Errata", so you should not need to import this module.
-}
module Errata.Types
    ( Convert(..)
    , Errata(..)
    , Block(..)
    , Pointer(..)
    , pointerColumns
    , Style(..)
    ) where

import qualified Data.Text as T

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
