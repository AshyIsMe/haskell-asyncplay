{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Text
import Data.ByteString
import Data.String (fromString)
import Control.Concurrent.Async

data StupidUser = StupidUser { userName :: Text
                 , fooCount :: Integer
                 } deriving (Show)

instance FromRow StupidUser where
  fromRow = StupidUser <$> field <*> field

allUsers :: Connection -> IO [StupidUser]
allUsers c = query_ c "SELECT username, foocount FROM StupidUser;"

asyncUsers :: Connection -> Integer -> IO [[StupidUser]]
asyncUsers c i =
  let q = "SELECT username, foocount FROM StupidUser offset ? fetch next ? rows only;"
  in do
    as <- mapM (\j -> async $ query c q (j, 1 :: Integer)) [0..i]
    mapM wait as


connectionString :: ByteString
connectionString = fromString "dbname='asyncplay'"

main :: IO ()
main = do
  conn <- connectPostgreSQL connectionString
  Prelude.putStrLn "Synchronous query"
  users <- allUsers conn
  mapM_ print users

  Prelude.putStrLn "Asynchronous query"
  usersagain <- asyncUsers conn 3
  mapM_ print usersagain
