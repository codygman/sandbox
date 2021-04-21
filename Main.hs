{-# LANGUAGE DeriveGeneric #-}
module Main where

import qualified Data.Csv as Csv
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Random as R
import Text.RawString.QQ
import qualified NeatInterpolation as NI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE
import Witch

data Person = Person { name :: !T.Text , age :: !Int }
    deriving (Generic, Show)

instance Csv.FromRecord Person
instance Csv.ToRecord Person

personJohn = Person "John" 27

personList = [ personJohn
             , Person "Kianna" 19
             ]

-- personCsv :: LBS.ByteString
-- personCsv = _ [NI.trimming|
-- name, age
-- John,27
-- Jane,28
-- |]

-- personCsvNoHeader :: LBS.ByteString  
-- personCsvNoHeader = _ [NI.trimming|
-- John,27
-- Jane,28
-- |]

randomAges :: IO [Int]
randomAges = R.newStdGen >>= pure . take 10 . R.randomRs (1,100)
  
main :: IO ()
main = putStrLn "Hello, Haskell!"
