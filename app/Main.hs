{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Data.ByteString hiding (concat, concatMap, map)
import Data.List.Split (chunksOf)
import Data.String (fromString)
import Data.Text hiding (chunksOf, concat, concatMap, map)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data StupidUser = StupidUser { userName :: Text
                 , fooCount :: Integer
                 } deriving (Show)

instance FromRow StupidUser where
  fromRow = StupidUser <$> field <*> field

allUsers :: Connection -> IO [StupidUser]
allUsers c = query_ c "SELECT username, foocount FROM StupidUser;"

-- Retrieve n users but only i threads at a time.
-- (Yes this is a totally contrived example that artificially limits the SQL query to returning 1 row at a time)
asyncUsers :: Connection -> Integer -> Int -> IO [[[StupidUser]]]
asyncUsers c n i =
  let q = "SELECT username, foocount FROM StupidUser offset ? fetch next ? rows only;"
      pages = chunksOf i [0..n]
  in
    mapM (mapConcurrently (\j -> query c q (j, 1 :: Integer))) pages

connectionString :: ByteString
connectionString = fromString "dbname='asyncplay'"

main :: IO ()
main = do
  conn <- connectPostgreSQL connectionString
  Prelude.putStrLn "Synchronous query"
  users <- allUsers conn
  mapM_ print users

  Prelude.putStrLn "Asynchronous query"
  usersagain <- asyncUsers conn 3 2
  mapM_ print usersagain
