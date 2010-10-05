# README

## CSV files

CSV files are the de-facto standard in arbitrary data retrieval, transfer or hand-off occasions. They are text-based, which means almost any application can be coerced to understand them. They are only limited by the OS on how much data they can contain, which makes them very convenient in general use.

However, in practice, they have a number of downsides:
  - They suffer from a widespread lack of standards. Everybody seems to pick different separators, quotation characters and line endings.
  - Most programs/languages load them in their entirety, which causes problems with larger data files.


## This package

This package is intended as a blazing-fast CSV parser/processor that can operate in constant-space using iteratees.


