{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           Data.Time
import qualified Data.Csv                      as Csv
import qualified Data.Csv.Incremental          as CsvI
import           GHC.Generics
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified System.Random                 as R
import           Text.RawString.QQ
import qualified NeatInterpolation             as NI
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy.Encoding       as LTE
import           Witch

data Person = Person
  { name :: !T.Text
  , age  :: !Int
  }
  deriving (Generic, Show)

instance Csv.FromRecord Person
instance Csv.ToRecord Person
instance Csv.ToNamedRecord Person

personJohn = Person "John" 27

personList = [personJohn, Person "Kianna" 19]

-- TODO upstream?
instance Cast T.Text LBS.ByteString where
  cast = into @LBS.ByteString . into @BS.ByteString

-- Csv.decode @Person Csv.HasHeader personCsv
-- Right [Person {name = "John", age = 27},Person {name = "Jane", age = 28}]
personCsv :: LBS.ByteString
personCsv = into @LBS.ByteString [NI.trimming|
name, age
John,27
Jane,28
|]

-- Csv.decode @Person Csv.NoHeader personCsvNoHeader
personCsvNoHeader :: LBS.ByteString
personCsvNoHeader = into @LBS.ByteString [NI.trimming|
John,27
Jane,28
|]

randomAges :: IO [Int]
randomAges = R.newStdGen >>= pure . take 10 . R.randomRs (1, 100)

main :: IO ()
main = putStrLn "Hello, Haskell!"
