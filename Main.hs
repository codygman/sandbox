{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Lens               as AesonL
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Csv                      as Csv
import qualified Data.Csv.Incremental          as CsvI
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy.Encoding       as LTE
import           Data.Time
import qualified Data.Vector                   as V
import           GHC.Generics
import qualified NeatInterpolation             as NI
import qualified System.Random                 as R
import           Text.RawString.QQ
import qualified Turtle                        as Turtle
import           Witch

data Person = Person
  { name :: !T.Text
  , age  :: !Int
  }
  deriving (Generic, Show)

instance Csv.FromRecord Person
instance Csv.ToRecord Person
instance Csv.ToNamedRecord Person


data Ramen = Ramen
  { reviewNumber :: !Int
  , brand        :: !T.Text
  , variety      :: !T.Text
  , style        :: !T.Text
  , country      :: !T.Text
  , stars        :: !(Maybe Double)
  , topTen       :: !T.Text
  }
  deriving (Generic, Show)

instance Csv.FromRecord Ramen
instance Csv.ToRecord Ramen
instance Csv.ToNamedRecord Ramen

personJohn = Person "John" 27

personList = [personJohn, Person "Kianna" 19]

-- TODO upstream?
-- instance Cast T.Text LBS.ByteString where
--   cast = _ . _

-- -- Csv.decode @Person Csv.HasHeader personCsv
-- -- Right [Person {name = "John", age = 27},Person {name = "Jane", age = 28}]
-- personCsv :: LBS.ByteString
-- personCsv = _ [NI.trimming|
-- name, age
-- John,27
-- Jane,28
--   |]

-- -- Csv.decode @Person Csv.NoHeader personCsvNoHeader
-- personCsvNoHeader :: LBS.ByteString
-- personCsvNoHeader = _ [NI.trimming|
-- John,27
-- Jane,28
-- |]

randomAges :: IO [Int]
randomAges = R.newStdGen >>= pure . take 10 . R.randomRs (1, 100)


randomJson = [NI.trimming|
{
    "firstName": "Rack",
    "lastName": "Jackon",
    "gender": "man",
    "age": 24,
    "address": {
        "streetAddress": "126",
        "city": "San Jone",
        "state": "CA",
        "postalCode": "394221"
    },
    "phoneNumbers": [
        { "type": "home", "number": "7383627627" }
    ]
}
|]

turtleExample :: Turtle.MonadIO io => io Turtle.ExitCode
turtleExample = Turtle.proc "date" [] mempty

main :: IO ()
main = putStrLn "Hello, Haskell!"
