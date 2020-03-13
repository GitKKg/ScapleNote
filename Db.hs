-- | 
{-#  LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels,DuplicateRecordFields #-}
module Db where
import Database.Selda   --proxychains stack install selda-0.4.0.0
import Database.Selda.SQLite
import Control.Exception
import Data.Typeable


data Stock = Stock  -- the field member name must be exact same with field of table in database which already exist
-- order no matter, just parts no matter, only name and type matter
  {
    -- name :: Text,
    code :: Text,
    date :: Int,
    name :: Text,
    shares :: Double,
    value :: Double,
    factor :: Double,
    open :: Int,
    high :: Int,
    close :: Int,
    low :: Int,
    average :: Double,
    fuquan_average :: Double
  } deriving (Generic, Show)
instance SqlRow Stock

defaultStock = Stock "600000" 20200101 "浦发银行" 0.0 0.0 0.0 0 0 0 0 0.0 0.0 
