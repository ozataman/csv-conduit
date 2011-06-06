# README

## CSV Files and Haskell

CSV files are the de-facto standard in many cases of data transfer,
particularly when dealing with enterprise application or disparate database
systems.

While there are a number of csv libraries in Haskell, at the time of this
project's start in 2010, there wasn't one that provided all of the following:

* Full flexibility in quote characters, separators, input/output
* Constant space operation
* Robust parsing and error resiliency
* Fast operation
* Convenient interface that supports a variety of use cases

This library is an attempt to close these gaps.


## This package

csv-enumerator is an enumerator-based CSV parsing library that is easy to use,
flexible and fast. Furthermore, it provides ways to use constant-space during
operation, which is absolutely critical in many real world use cases.


### Introduction

* ByteStrings are used for everything
* There are 2 basic row types and they implement *exactly* the same operations,
  so you can chose the right one for the job at hand:
  - type MapRow = Map ByteString ByteString
  - type Row = [ByteString]
* Folding over a CSV file can be thought of as the most basic operation.
* Higher level convenience functions are provided to "map" over CSV files,
  modifying and transforming them along the way.
* Helpers are provided for simple input/output of CSV files for simple use
  cases.
* For extreme / advanced use cases, the user can drop down to the
  Enumerator/Iteratee level and do interleaved IO among other things.

### API Docs

The API is quite well documented and I would encourage you to keep it handy.

### Speed

While fast operation is of concern, I have so far cared more about correct
operation and a flexible API. Please let me know if you notice any performance
regressions or optimization opportunities.


### Usage Examples

#### Example 1: Basic Operation 

    {-# LANGUAGE OverloadedStrings #-}

    import Data.CSV.Enumerator
    import Data.Char (isSpace)
    import qualified Data.Map as M
    import Data.Map ((!))

    -- Naive whitespace stripper
    strip = reverse . B.dropWhile isSpace . reverse . B.dropWhile isSpace

    -- A function that takes a row and "emits" zero or more rows as output.
    processRow :: MapRow -> [MapRow]
    processRow row = [M.insert "Column1" fixedCol row]
      where fixedCol = strip (row ! "Column1")

    main = mapCSVFile "InputFile.csv" defCSVSettings procesRow "OutputFile.csv"

and we are done. 


Further examples to be provided at a later time.



### TODO - Next Steps

* Need to think about specializing an Exception type for the library and
  properly notifying the user when parsing-related problems occur.
* Some operations can be further broken down to their atoms, increasing the
  flexibility of the library.
* The CSVeable typeclass can be refactored to have a more minimal definition.
* Operating on Text in addition to ByteString would be phenomenal.
* A test-suite needs to be added.
* Some benchmarking would be nice.


Any and all kinds of help is much appreciated!

