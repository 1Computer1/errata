# Changelog

**Errata** uses [PVP Versioning](https://pvp.haskell.org).

## Unreleased

* Added new `blockHeader` field to `Block`, which will put text underneath the location text but above the source lines. This also affects all the block helper functions, which now have an argument for the header.

* The `blockSimple` and `blockSimple'` functions are now passed tuples of positions and labels instead of `Int`s, which is more consistent with the rest of the helper functions.

* Defined type synonyms for line, column, headers, bodies, and labels, for the purpose of documentation. It should be much easier to know what is expected by just reading the types now.

## 0.1.0.0

* Initial release.
