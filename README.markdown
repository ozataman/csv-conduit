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

csv-conduit is a conduits based CSV parsing library that is easy to
use, flexible and fast. Furthermore, it provides ways to use
constant-space during operation, which is absolutely critical in many
real world use cases.


### Introduction

* The CSVeable typeclass implements the key operations.
* CSVeable is parameterized on both a stream type and a target CSV row type.
* There are 2 basic row types and they implement *exactly* the same operations,
  so you can chose the right one for the job at hand:
  - type MapRow t = Map t t
  - type Row t = [t]
* You basically use the Conduits defined in this library to do the
  parsing from a CSV stream and rendering back into a CSV stream.
* Use the full flexibility and modularity of conduits for sources and sinks.

### Speed

While fast operation is of concern, I have so far cared more about correct
operation and a flexible API. Please let me know if you notice any performance
regressions or optimization opportunities.


### Usage Examples

#### Example 1: Basic Operation 

    {-# LANGUAGE OverloadedStrings #-}

    import Data.Conduit.Text
    import Data.Conduit.Binary
    import Data.Conduit
    import Data.CSV.Conduit
    
    -- Let's simply stream from a file, parse the CSV, reserialize it
    -- and push back into another file.
    test :: IO ()
    test = runResourceT $ 
      sourceFile "test/BigFile.csv" $= 
      decode utf8 $=
      (intoCSV defCSVSettings 
        :: forall m. MonadResource m => Conduit Text m (MapRow Text)) $= 
      fromCSV defCSVSettings $=
      encode utf8 $$
      sinkFile "test/BigFileOut.csv"


and we are done. 



