# README
[![cabal
build](https://github.com/dmvianna/csv-conduit/actions/workflows/cabal.yml/badge.svg)](https://github.com/dmvianna/csv-conduit/actions)
[![stack build](https://github.com/dmvianna/csv-conduit/actions/workflows/stack.yml/badge.svg)](https://github.com/dmvianna/csv-conduit/actions)

## CSV Files and Haskell

CSV files are the de-facto standard in many cases of data transfer,
particularly when dealing with enterprise application or disparate database
systems.

While there are a number of csv libraries in Haskell, at the time of
this project's start, there wasn't one that provided all of the
following:

* Full flexibility in quote characters, separators, input/output
* Constant space operation
* Robust parsing and error resiliency
* Battle-tested reliability in real-world datasets
* Fast operation
* Convenient interface that supports a variety of use cases

Over time, people created other plausible CSV packages like cassava.
The major benefit from this library remains to be:

* Direct participation in the conduit ecosystem, which is now quite
  large, and all the benefits that come with it.
* Flexibility in CSV format definition.
* Resiliency to errors in the input data.


## This package

csv-conduit is a conduit-based CSV parsing library that is easy to
use, flexible and fast. It leverages the conduit infrastructure to
provide constant-space operation, which is quite critical in many real
world use cases.

For example, you can use http-conduit to download a CSV file from the
internet and plug its Source into intoCSV to stream-convert the
download into the Row data type and do something with it as the data
streams, that is without having to download the entire file to disk
first.


## Author & Contributors

- Ozgun Ataman (@ozataman)
- Daniel Bergey (@bergey)
- BJTerry (@BJTerry)
- Mike Craig (@mkscrg)
- Daniel Corson (@dancor)
- Dmitry Dzhus (@dzhus)
- Niklas Hambüchen (@nh2)
- Facundo Domínguez (@facundominguez)


### Introduction

* The CSVeable typeclass implements the key operations.
* CSVeable is parameterized on both a stream type and a target CSV row type.
* There are 2 basic row types and they implement *exactly* the same operations,
  so you can chose the right one for the job at hand:
  - `type MapRow t = Map t t`
  - `type Row t = [t]`
* You basically use the Conduits defined in this library to do the
  parsing from a CSV stream and rendering back into a CSV stream.
* Use the full flexibility and modularity of conduits for sources and sinks.

### Speed

While fast operation is of concern, I have so far cared more about correct
operation and a flexible API. Please let me know if you notice any performance
regressions or optimization opportunities.


### Usage Examples


#### Example #1: Basics Using Convenience API

```haskell
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
```

#### Example #2: Basics Using Conduit API

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Data.Conduit.Binary
import Data.CSV.Conduit
import Data.Text (Text)

myProcessor :: Monad m => Conduit (Row Text) m (Row Text)
myProcessor = awaitForever $ yield

-- Let's simply stream from a file, parse the CSV, reserialize it
-- and push back into another file.
test :: IO ()
test = runResourceT $
  sourceFile "test/BigFile.csv" $=
  intoCSV defCSVSettings $=
  myProcessor $=
  fromCSV defCSVSettings $$
  sinkFile "test/BigFileOut.csv"
```
