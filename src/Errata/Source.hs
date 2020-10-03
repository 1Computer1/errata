{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Errata.Source
Copyright   : (c) 2020 comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

A class for source text types. You should not need to use this, except to add new source types.
-}
module Errata.Source
    ( Source(..)
    ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

{-|
A class for manipulating and converting source text.

For @ByteString@ source types, you should convert it to one of the built-in instances with your encoding of choice.

Every @Source@ must be a @Monoid@ to allow for a @mempty@ unit which will be used in the event that a pointer references an out-of-bounds source line.
-}
class Monoid s => Source s where
    -- | Splits the source into lines.
    sourceToLines :: s -> [s]

    -- | Converts the source text to 'Data.Text.Text' (strict). The given source text is a single line of the source.
    sourceToText :: s -> T.Text

instance Source String where
    sourceToLines = lines
    sourceToText = T.pack

instance Source T.Text where
    sourceToLines = T.lines
    sourceToText = id

instance Source TL.Text where
    sourceToLines = TL.lines
    sourceToText = TL.toStrict
