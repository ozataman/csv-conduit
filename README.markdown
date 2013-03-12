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


## Author & Contributors

Ozgun Ataman (@ozataman)
Daniel Bergey (@bergey)
BJTerry (@BJTerry)
Dmitry Dzhus (@dzhus)
Mike Craig (@mkscrg)
Daniel Corson (@dancor)
Niklas Hambüchen (@nh2)


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


#### Example #1: Basics Using Convenience API

    {-# LANGUAGE OverloadedStrings #-}

    import Data.Conduit
    import Data.Conduit.Binary
    import Data.Conduit.List as CL
    import Data.CSV.Conduit
    import Data.Text (Text)
    
    -- Just reverse te columns
    myProcessor :: Monad m => Conduit (Row Text) m (Row Text)
    myProcessor = CL.map reverse
    
    test :: IO ()
    test = runResourceT $ 
      transformCSV defCSVSettings 
                   (sourceFile "input.csv") 
                   myProcessor
                   (sinkFile "output.csv")


#### Example #2: Basics Using Conduit API

    {-# LANGUAGE OverloadedStrings #-}

    import Data.Conduit
    import Data.Conduit.Binary
    import Data.CSV.Conduit
    import Data.Text (Text)

    myProcessor :: Conduit (Row Text) m (Row Text)
    myProcessor = undefined
    
    -- Let's simply stream from a file, parse the CSV, reserialize it
    -- and push back into another file.
    test :: IO ()
    test = runResourceT $ 
      sourceFile "test/BigFile.csv" $= 
      intoCSV defCSVSettings $=
      myProcessor $=
      fromCSV defCSVSettings $$
      sinkFile "test/BigFileOut.csv"


