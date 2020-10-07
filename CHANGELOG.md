# Changelog

**Errata** uses [PVP Versioning](https://pvp.haskell.org).

## Unreleased (0.3.0.0)

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
