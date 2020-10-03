# Changelog

**Errata** uses [PVP Versioning](https://pvp.haskell.org).

## Unreleased

* Optimized rendering of errors (#5). Huge thanks to [RiugaBachi](https://github.com/RiugaBachi) for this!

* Reworked the pretty printer so that it no longer prints trailing whitespace in most places.

## 0.2.0.0

* Added new `blockHeader` field to `Block`, which will put text underneath the location text but above the source lines. This also affects all the block helper functions, which now have an argument for the header.

* The `blockSimple` and `blockSimple'` functions are now passed tuples of positions and labels instead of `Int`s, which is more consistent with the rest of the helper functions.

* Defined type synonyms for line, column, headers, bodies, and labels, for the purpose of documentation. It should be much easier to know what is expected by just reading the types now.

* Use `GHC.Arr.Array` for keeping source lines, which should be faster for indexing and should not force the individual lines until they are needed.

## 0.1.0.0

* Initial release.
