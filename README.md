# Errata

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)
[![Hackage](https://img.shields.io/hackage/v/errata.svg?logo=haskell)](https://hackage.haskell.org/package/errata)
[![CI](https://github.com/1Computer1/errata/workflows/CI/badge.svg)](https://github.com/1Computer1/errata/actions?query=workflow%3ACI)

**Errata** is an extremely customizable error pretty printer that can handle many kinds of error formatting.

## Features

**Errata** can handle errors that are all over the source or errors that are connected to each other spanning multiple lines. You can be as simple or as fancy as you like!  

You can also customize the format of the printer in several ways:  

- Custom messages and labels
- Custom character sets for symbols
- Highlighting the source, messages, and symbols

## Examples

A clean, modern error message that is trying to be helpful:

![An error message that points out that the `fold` function was not found in scope. It then asks if the user meant to use `foldl` or `foldr`](./errata_fold.png)

A busy error message with underlining and connections:

![An error message that highlights mismatching types in an `if` expression. The first section underlines the mismatching values, and the second section underlines the `if` expression](./errata_if.png)
