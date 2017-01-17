# BloomDown

A Purescript library for parsing Bloom Built's dialect of Markdown, called *BloomDown*, a small & selective subset of CommonMark.

### Tests

The tests use [purescript-strongcheck](http://github.com/purescript-contrib/purescript-strongcheck) to verify that an arbitrary `BloomDown` document can be rendered as a `String` and then parsed to a `BloomDown` equal to the original.

## Features

In general, BloomDown is a subset of [CommonMark](http://spec.commonmark.org/), supporting the following features:

* Leaf Blocks
  * Headers
  * Paragraphs
* Inlines
  * Images

The parser produces an ADT which describes the structure of the document.
