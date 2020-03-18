-- | 
{-#  LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels,DuplicateRecordFields #-}
module Db where
import Database.Selda   --proxychains stack install selda-0.4.0.0
import Database.Selda.SQLite
import Control.Exception
import Data.Typeable

import Data.Text


data Stock = Stock  -- the field member name must be exact same with field of table in database which already exist
-- order no matter, just parts no matter, only name and type matter
  {
    -- name :: Text,
    _code :: Text,
    _date :: Int,
    _name :: Text,
    _shares :: Int,-- use Int not Double,for saving space of database
    _value :: Int,
    _factor :: Double,
    _open :: Int,
    _high :: Int,
    _close :: Int,
    _low :: Int,
    _average :: Double,
    _fuquan_average :: Double
  } deriving (Generic) -- not deriving Show, for default show not show utf8 Chinese correcttly

instance SqlRow Stock
   

instance Show Stock where
  show stock = "Stock" ++ " {"++
    "\n_code =" ++ (unpack . _code $ stock) ++
    ",\n_date =" ++ (show._date $ stock) ++
    ",\n_name =" ++ (unpack . _name $ stock) ++
    ",\n_open =" ++ (show . (/ 1000) . (fromIntegral :: Int -> Float). _open $ stock) ++
    ",\n_high =" ++ (show . (/ 1000) . (fromIntegral :: Int -> Float). _high $ stock) ++
    ",\n_close =" ++ (show . (/ 1000) . (fromIntegral :: Int -> Float). _close $ stock) ++
    ",\n_low =" ++ (show . (/ 1000) . (fromIntegral :: Int -> Float). _low $ stock)  ++
    ",\n_shares =" ++ (show  . _shares $ stock) ++
    ",\n_value =" ++ (show  . _value $ stock) ++
    "\n}\n"
defaultStock = Stock "600000" 20200101 "浦发银行" 0 0 0.0 0 0 0 0 0.0 0.0 
