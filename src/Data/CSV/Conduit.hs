{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.CSV.Conduit
    (

    -- * Main Interface
      decodeCSV
    , readCSVFile
    , writeCSVFile
    , transformCSV
    , transformCSV'
    , mapCSVFile
    , writeHeaders
    , writeHeadersOrdered

    -- Types
    , CSV (..)
    , CSVSettings (..)
    , defCSVSettings
    , MapRow
    , OrderedMapRow
    , Row

    -- * Re-exported For Convenience
    , runResourceT
    ) where

-------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad.Catch.Pure           (CatchT)
import           Control.Monad.Catch.Pure           (runCatchT)
import           Control.Monad.Except
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Resource       (MonadResource, MonadThrow,
                                                     runResourceT)
import           Data.Attoparsec.Types              (Parser)
import qualified Data.ByteString                    as B
import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as B8
import           Data.ByteString.Internal           (c2w)
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary                (sinkFile, sinkIOHandle,
                                                     sourceFile)
import qualified Data.Conduit.List                  as C
import qualified Data.Map                           as M
import qualified Data.Map.Ordered                   as MO
import           Data.String
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.Vector                        as V
import qualified Data.Vector.Generic                as GV
import qualified Data.Vector.Generic.Mutable        as GMV
import           Data.Void                           as Void
import           System.IO
-------------------------------------------------------------------------------
import           Data.CSV.Conduit.Conversion        (FromNamedRecord (..),
                                                     FromNamedRecordOrdered (..),
                                                     Named (..),
                                                     NamedOrdered (..),
                                                     ToNamedRecord (..),
                                                     ToNamedRecordOrdered (..),
                                                     runParser)
import qualified Data.CSV.Conduit.Parser.ByteString as BSP
import qualified Data.CSV.Conduit.Parser.Text       as TP
import           Data.CSV.Conduit.Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Represents types 'r' that are CSV-like and can be converted
-- to/from an underlying stream of type 's'. There is nothing scary
-- about the type:
--
-- @s@ represents stream types that can be converted to\/from CSV rows.
-- Examples are 'ByteString', 'Text' and 'String'.
--
-- @r@ represents the target CSV row representations that this library
-- can work with. Examples are the 'Row' types, the 'Record' type and
-- the 'MapRow' family of types. We can also convert directly to
-- complex Haskell types using the 'Data.CSV.Conduit.Conversion'
-- module that was borrowed from the cassava package, which was itself
-- inspired by the aeson package.
--
--
-- Example #1: Basics Using Convenience API
--
-- >import Data.Conduit
-- >import Data.Conduit.Binary
-- >import Data.Conduit.List as CL
-- >import Data.CSV.Conduit
-- >
-- >myProcessor :: Conduit (Row Text) m (Row Text)
-- >myProcessor = CL.map reverse
-- >
-- >test = runResourceT $
-- >  transformCSV defCSVSettings
-- >               (sourceFile "input.csv")
-- >               myProcessor
-- >               (sinkFile "output.csv")
--
--
-- Example #2: Basics Using Conduit API
--
-- >import Data.Conduit
-- >import Data.Conduit.Binary
-- >import Data.CSV.Conduit
-- >
-- >myProcessor :: Conduit (MapRow Text) m (MapRow Text)
-- >myProcessor = undefined
-- >
-- >test = runResourceT $ runConduit $
-- >  sourceFile "test/BigFile.csv" .|
-- >  intoCSV defCSVSettings .|
-- >  myProcessor .|
-- >  (writeHeaders defCSVSettings >> fromCSV defCSVSettings) .|
-- >  sinkFile "test/BigFileOut.csv"
class CSV s r where

  -----------------------------------------------------------------------------
  -- | Convert a CSV row into strict ByteString equivalent.
  rowToStr :: CSVSettings -> r -> s

  -----------------------------------------------------------------------------
  -- | Turn a stream of 's' into a stream of CSV row type. An example
  -- would be parsing a ByteString stream as rows of 'MapRow' 'Text'.
  intoCSV :: (MonadThrow m) => CSVSettings -> ConduitM s r m ()

  -----------------------------------------------------------------------------
  -- | Turn a stream of CSV row type back into a stream of 's'. An
  -- example would be rendering a stream of 'Row' 'ByteString' rows as
  -- 'Text'.
  fromCSV :: Monad m => CSVSettings -> ConduitM r s m ()





------------------------------------------------------------------------------
-- | 'Row' instance using 'ByteString'
instance CSV ByteString (Row ByteString) where
  rowToStr s !r =
    let
      sep = B.pack [c2w (csvSep s)]
      wrapField !f = case csvQuoteCharAndStyle s of
        Just (x, quoteEmpty) ->
          case quoteEmpty == DoQuoteEmpty || B8.length f /= 0 of
            True -> (x `B8.cons` escape x f) `B8.snoc` x
            False -> f
        Nothing   -> f
      escape c str = B8.intercalate (B8.pack [c,c]) $ B8.split c str
    in B.intercalate sep . map wrapField $ r

  intoCSV set = intoCSVRow (BSP.row set)
  fromCSV set = fromCSVRow set


------------------------------------------------------------------------------
-- | 'Row' instance using 'Text'
instance CSV Text (Row Text) where
  rowToStr s !r =
    let
      sep = T.pack [csvSep s]
      wrapField !f = case csvQuoteCharAndStyle s of
        Just (x, quoteEmpty) -> case quoteEmpty == DoQuoteEmpty || not (T.null f) of
          True -> x `T.cons` escape x f `T.snoc` x
          False -> f
        Nothing -> f
      escape c str = T.intercalate (T.pack [c,c]) $ T.split (== c) str
    in T.intercalate sep . map wrapField $ r

  intoCSV set = intoCSVRow (TP.row set)
  fromCSV set = fromCSVRow set


-------------------------------------------------------------------------------
-- | 'Row' instance using 'Text' based on 'ByteString' stream
instance CSV ByteString (Row Text) where
    rowToStr s r = T.encodeUtf8 $ rowToStr s r
    intoCSV set = intoCSV set .| C.map (map T.decodeUtf8)
    fromCSV set = fromCSV set .| C.map T.encodeUtf8



-------------------------------------------------------------------------------
-- | 'Row' instance using 'String' based on 'ByteString' stream.
-- Please note this uses the ByteString operations underneath and has
-- lots of unnecessary overhead. Included for convenience.
instance CSV ByteString (Row String) where
    rowToStr s r = rowToStr s $ map B8.pack r
    intoCSV set = intoCSV set .| C.map (map B8.unpack)
    fromCSV set = C.map (map B8.pack) .| fromCSV set


-- | Support for parsing rows in the 'Vector' form.
instance (CSV s (Row s)) => CSV s (V.Vector s) where
    rowToStr s r = rowToStr s . V.toList $ r
    intoCSV set = intoCSV set .| C.map (V.fromList)
    fromCSV set = C.map (V.toList) .| fromCSV set



-------------------------------------------------------------------------------
fromCSVRow :: (Monad m, IsString s, CSV s r)
           => CSVSettings -> ConduitM r s m ()
fromCSVRow set = awaitForever $ \row -> mapM_ yield [rowToStr set row, "\n"]



-------------------------------------------------------------------------------
intoCSVRow :: (MonadThrow m, AttoparsecInput i) => Parser i (Maybe o) -> ConduitM i o m ()
intoCSVRow p = parse .| puller
  where
    parse = {-# SCC "conduitParser_p" #-} conduitParser p
    puller = {-# SCC "puller" #-}
      awaitForever $ \ (_, mrow) -> maybe (return ()) yield mrow


-------------------------------------------------------------------------------
-- | Generic 'MapRow' instance; any stream type with a 'Row' instance
-- automatically gets a 'MapRow' instance.
instance (CSV s (Row s'), Ord s', IsString s) => CSV s (MapRow s') where
  rowToStr s r = rowToStr s . M.elems $ r
  intoCSV set = intoCSVMap set
  fromCSV set = fromCSVMap set

instance (CSV s (Row s'), Ord s', IsString s) => CSV s (OrderedMapRow s') where
  rowToStr s r = rowToStr s . (map snd . MO.assocs) $ r
  intoCSV set = intoCSVMapOrdered set
  fromCSV set = fromCSVMapOrdered set


-------------------------------------------------------------------------------
intoCSVMap :: (Ord a, MonadThrow m, CSV s [a])
           => CSVSettings -> ConduitM s (MapRow a) m ()
intoCSVMap set = intoCSV set .| (headers >>= converter)
  where
    headers = do
      mrow <- await
      case mrow of
        Nothing -> return []
        Just [] -> headers
        Just hs -> return hs
    converter hs = awaitForever $ yield . toMapCSV hs
    toMapCSV !hs !fs = M.fromList $ zip hs fs

intoCSVMapOrdered :: (Ord a, MonadThrow m, CSV s [a])
           => CSVSettings -> ConduitM s (OrderedMapRow a) m ()
intoCSVMapOrdered set = intoCSV set .| (headers >>= converter)
  where
    headers = do
      mrow <- await
      case mrow of
        Nothing -> return []
        Just [] -> headers
        Just hs -> return hs
    converter hs = awaitForever $ yield . toMapCSV hs
    toMapCSV !hs !fs = MO.fromList $ zip hs fs


-- | Conversion of stream directly to/from a custom complex haskell
-- type.
instance (FromNamedRecord a, ToNamedRecord a, CSV s (MapRow ByteString)) =>
    CSV s (Named a) where
    rowToStr s a = rowToStr s . toNamedRecord . getNamed $ a
    intoCSV set = intoCSV set .| C.mapMaybe go
        where
          go x = either (const Nothing) (Just . Named) $
                 runParser (parseNamedRecord x)

    fromCSV set = C.map go .| fromCSV set
        where
          go = toNamedRecord . getNamed

instance (FromNamedRecordOrdered a, ToNamedRecordOrdered a, CSV s (OrderedMapRow ByteString)) =>
    CSV s (NamedOrdered a) where
    rowToStr s a = rowToStr s . toNamedRecordOrdered . getNamedOrdered $ a
    intoCSV set = intoCSV set .| C.mapMaybe go
        where
          go x = either (const Nothing) (Just . NamedOrdered) $
                 runParser (parseNamedRecordOrdered x)

    fromCSV set = C.map go .| fromCSV set
        where
          go = toNamedRecordOrdered . getNamedOrdered


-------------------------------------------------------------------------------
fromCSVMap :: (Monad m, IsString s, CSV s [a])
           => CSVSettings -> ConduitM (M.Map k a) s m ()
fromCSVMap set = awaitForever push
  where
    push r = mapM_ yield [rowToStr set (M.elems r), "\n"]

fromCSVMapOrdered :: (Monad m, IsString s, CSV s [a])
                  => CSVSettings -> ConduitM (MO.OMap k a) s m ()
fromCSVMapOrdered set = awaitForever push
  where
    push r = mapM_ yield [rowToStr set (map snd $ MO.assocs r), "\n"]


-------------------------------------------------------------------------------
-- | Write headers AND the row into the output stream, once. If you
-- don't call this while using 'MapRow' family of row types, then your
-- resulting output will NOT have any headers in it.
--
-- Usage: Just chain this using the 'Monad' instance in your pipeline:
--
-- > runConduit $ ... .| writeHeaders settings >> fromCSV settings .| sinkFile "..."
writeHeaders
    :: (Monad m, CSV s (Row r), IsString s)
    => CSVSettings
    -> ConduitM (MapRow r) s m ()
writeHeaders set = do
  mrow <- await
  case mrow of
    Nothing -> return ()
    Just row -> mapM_ yield [ rowToStr set (M.keys row)
                            , "\n"
                            , rowToStr set (M.elems row)
                            , "\n" ]

writeHeadersOrdered
    :: (Monad m, CSV s (Row r), IsString s)
    => CSVSettings
    -> ConduitM (OrderedMapRow r) s m ()
writeHeadersOrdered set = do
  mrow <- await
  case mrow of
    Nothing -> return ()
    Just row -> mapM_ yield [ rowToStr set (map fst $ MO.assocs row)
                            , "\n"
                            , rowToStr set (map snd $ MO.assocs row)
                            , "\n" ]


                          ---------------------------
                          -- Convenience Functions --
                          ---------------------------


-------------------------------------------------------------------------------
-- | Read the entire contents of a CSV file into memory.
readCSVFile
    :: (MonadIO m, CSV ByteString a)
    => CSVSettings -- ^ Settings to use in deciphering stream
    -> FilePath    -- ^ Input file
    -> m (V.Vector a)
readCSVFile set fp = liftIO . runResourceT $ runConduit $ sourceFile fp .| intoCSV set .| transPipe lift (sinkVector growthFactor)
  where
    growthFactor = 10


-------------------------------------------------------------------------------
-- | A simple way to decode a CSV string. Don't be alarmed by the
-- polymorphic nature of the signature. 's' is the type for the string
-- and 'v' is a kind of 'Vector' here.
--
-- For example for 'ByteString':
--
-- >>> s <- LB.readFile "my.csv"
-- >>> decodeCSV defCSVSettings s :: Either SomeException (Vector (Vector ByteString))
--
-- will work as long as the data is comma separated.
decodeCSV
    :: forall v a s. (GV.Vector v a, CSV s a)
    => CSVSettings
    -> s
    -> Either SomeException (v a)
decodeCSV set bs = runST $ runExceptT pipeline
  where
    src :: ConduitM () s (ExceptT SomeException (ST s1)) ()
    src = C.sourceList [bs]
    csvConvert :: ConduitM s a (ExceptT SomeException (ST s1)) ()
    csvConvert = transPipe (ExceptT . runCatchT) csvConvert'
    csvConvert' :: ConduitM s a (CatchT (ST s1)) ()
    csvConvert' = intoCSV set
    growthFactor = 10
    sink :: ConduitM a Void.Void (ExceptT SomeException (ST s1)) (v a)
    sink = sinkVector growthFactor
    pipeline :: ExceptT SomeException (ST s1) (v a)
    pipeline = runConduit (src .| csvConvert .| sink)



-------------------------------------------------------------------------------
-- | Write CSV data into file. As we use a 'ByteString' sink, you'll
-- need to get your data into a 'ByteString' stream type.
writeCSVFile
  :: (CSV ByteString a)
  => CSVSettings
  -- ^ CSV Settings
  -> FilePath
  -- ^ Target file
  -> IOMode
  -- ^ Write vs. append mode
  -> [a]
  -- ^ List of rows
  -> IO ()
writeCSVFile set fo fmode rows = runResourceT $ runConduit $ do
  C.sourceList rows .| fromCSV set .|
    sinkIOHandle (openFile fo fmode)


-------------------------------------------------------------------------------
-- | Map over the rows of a CSV file. Provided for convenience for
-- historical reasons.
--
-- An easy way to run this function would be 'runResourceT' after
-- feeding it all the arguments.
mapCSVFile
    :: ( MonadResource m
       , CSV ByteString a
       , CSV ByteString b
# if MIN_VERSION_resourcet(1,2,0)
       , MonadThrow m
#endif
       )
      => CSVSettings
      -- ^ Settings to use both for both input and output
      -> (a -> [b])
      -- ^ A mapping function
      -> FilePath
      -- ^ Input file
      -> FilePath
      -- ^ Output file
      -> m ()
mapCSVFile set f fi fo =
  transformCSV set (sourceFile fi) (C.concatMap f) (sinkFile fo)


-------------------------------------------------------------------------------
-- | Like transformCSV' but uses the same settings for both input and
-- output.
transformCSV
    :: (MonadThrow m, CSV s a, CSV s' b)
    => CSVSettings
    -- ^ Settings to be used for both input and output
    -> ConduitM () s m ()
    -- ^ A raw stream data source. Ex: 'sourceFile inFile'
    -> ConduitM a b m ()
    -- ^ A transforming conduit
    -> ConduitM s' Void.Void m ()
    -- ^ A raw stream data sink. Ex: 'sinkFile outFile'
    -> m ()
transformCSV set = transformCSV' set set


-------------------------------------------------------------------------------
-- | General purpose CSV transformer. Apply a list-like processing
-- function from 'Data.Conduit.List' to the rows of a CSV stream. You
-- need to provide a stream data source, a transformer and a stream
-- data sink.
--
-- An easy way to run this function would be 'runResourceT' after
-- feeding it all the arguments.
--
-- Example - map a function over the rows of a CSV file:
--
-- > transformCSV setIn setOut (sourceFile inFile) (C.map f) (sinkFile outFile)
transformCSV'
    :: (MonadThrow m, CSV s a, CSV s' b)
    => CSVSettings
    -- ^ Settings to be used for input
    -> CSVSettings
    -- ^ Settings to be used for output
    -> ConduitM () s m ()
    -- ^ A raw stream data source. Ex: 'sourceFile inFile'
    -> ConduitM a b m ()
    -- ^ A transforming conduit
    -> ConduitM s' Void.Void m ()
    -- ^ A raw stream data sink. Ex: 'sinkFile outFile'
    -> m ()
transformCSV' setIn setOut source c sink = runConduit $
    source .|
    intoCSV setIn .|
    c .|
    fromCSV setOut .|
    sink




                              ------------------
                              -- Vector Utils --
                              ------------------



-------------------------------------------------------------------------------
-- | An efficient sink that incrementally grows a vector from the input stream
sinkVector :: (PrimMonad m, GV.Vector v a) => Int -> ConduitM a o m (v a)
sinkVector by = do
    v <- lift $ GMV.new by
    go 0 v
  where
    -- i is the index of the next element to be written by go
    -- also exactly the number of elements in v so far
    go i v = do
        res <- await
        case res of
          Nothing -> do
            v' <- lift $ GV.freeze $ GMV.slice 0 i v
            return $! v'
          Just x -> do
            v' <- case GMV.length v == i of
                    True  -> lift $ GMV.grow v by
                    False -> return v
            lift $ GMV.write v' i x
            go (i+1) v'
