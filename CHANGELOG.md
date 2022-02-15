# Changelog

**Errata** uses [PVP Versioning](https://pvp.haskell.org).

## Unreleased (0.4.0.0)

* Added styling individual pointers with `PointerStyle` (e.g. characters, highlighting).
    * This changes how `styleLine` and `highlight` works and moves `styleUnderline` to `PointerStyle`.

    * This also adds a `PointerStyle` parameter to the helper functions so e.g. `blockSimple fancyStyle ...` should now be `blockSimple fancyStyle fancyPointer ...`.

    * Crazy example from the tests (you can imagine coloring things differently):
        ```
        an error
        --> here:1:1
          |
        1 | abcdefghijk
          |  ..  ~~  ^^ z
          |  |   :
          |  |   2 y
          |  1 x
        2 | lmnopqrstuv
          |     ''' w
        an error occurred here
        ```

* Moved existing and added new premade styles to `Errata.Styles`. `Errata` no longer exports premade styles. Also moved `highlight` there.

* Added support for characters with different widths (full-width, combining characters, others). The cabal flag `usewcwidth` (default false) can be enabled to use the native `wcwidth` function.

* Added support for replacing tabs with spaces with the `styleTabWidth` option in `Style` (defaults to 4).

* Added option for the lines before and after the omission line with `styleExtraLinesAfter` and `styleExtraLinesBefore` in `Style`.

* Added option for padding lines before and after the source `stylePaddingTop` and `stylePaddingBottom` in `Style`.

* Added option for disabling the hook with `styleEnableHook` in `PointerStyle`.

* Added option for toggling all decorations completely with `styleEnableDecorations` in `Style`. Useful if you only want highlighting and not underlining. Combine with `stylePaddingTop = False` for a compact source code block.

* Added `Show` instances to all the types. Style functions are applied to some sample text.

* Changed `Monoid` constraint of `Source` to just requiring an `emptySource` value.

* Fixed trailing whitespace in the omission line.

## 0.3.0.0

* Support GHC 9.0.1 (and eventually 9.2).

* Optimized rendering of errors ([#5](https://github.com/1Computer1/errata/pull/5)). Huge thanks to [RiugaBachi](https://github.com/RiugaBachi) for this! This also adds a `Monoid` constraint to `Source`.

* Reworked the pretty printer so that it no longer prints trailing whitespace in most places.

* Removed the `errataBlock` field, and only use `errataBlocks`. Now, an `Errata` can have no blocks attached to it at all. They will also no longer be sorted beforehand, as that should be up to the user.

* Removed `prettyErrorsNE`, as it is no longer useful for what it was documented for.

* Fixed the rendering of `Block`s with no `Pointer`s adding extra blank lines.

## 0.2.0.0

* Added new `blockHeader` field to `Block`, which will put text underneath the location text but above the source lines. This also affects all the block helper functions, which now have an argument for the header.

* The `blockSimple` and `blockSimple'` functions are now passed tuples of positions and labels instead of `Int`s, which is more consistent with the rest of the helper functions.

* Defined type synonyms for line, column, headers, bodies, and labels, for the purpose of documentation. It should be much easier to know what is expected by just reading the types now.

* Use `GHC.Arr.Array` for keeping source lines, which should be faster for indexing and should not force the individual lines until they are needed.

## 0.1.0.0

* Initial release.
