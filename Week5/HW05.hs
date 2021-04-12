{-# OPTIONS_GHC -Wall #-}
module Week5.HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Bits
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
-- import qualified Data.Map.Strict as Map

import Week5.Parser ( FromJSON(..), decode, encode )

-- Exercise 1 -----------------------------------------

-- To run in GHCI:
-- getSecret "Week5/clues/dog-original.jpg" "Week5/clues/dog.jpg"

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret dog1 dog2 = do
  bytesDog1 <- BS.readFile dog1
  bytesDog2 <- BS.readFile dog2
  let word8Array = BS.zipWith xor bytesDog1 bytesDog2 
  let nonNullWord8Array = filter (/=0) word8Array
  let byteString = BS.pack nonNullWord8Array
  pure byteString
  -- idk why m using pure here, 
  -- i was looking at mhaven backend and saw this being used to return values from functions,
  -- is there any other reason behind this and when specifically to use it

-- Exercise 2 -----------------------------------------

-- To run in GHCI:
-- secret <- getSecret "Week5/clues/dog-original.jpg" "Week5/clues/dog.jpg"
-- decryptWithKey secret "Week5/clues/victims.json"

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey secret filePath = do
  encriptedFileByteString <- BS.readFile $ filePath++".enc"
  let decryptedWrod8Array = BS.zipWith xor ((BS.pack . cycle . BS.unpack) secret) encriptedFileByteString
  let byteString = BS.pack decryptedWrod8Array
  BS.writeFile filePath byteString

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = (decode <$>) . BS.readFile 

-- -- Exercise 4 -----------------------------------------

-- getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
-- getBadTs = undefined

-- -- Exercise 5 -----------------------------------------

-- getFlow :: [Transaction] -> Map String Integer
-- getFlow = undefined

-- -- Exercise 6 -----------------------------------------

-- getCriminal :: Map String Integer -> String
-- getCriminal = undefined

-- -- Exercise 7 -----------------------------------------

-- undoTs :: Map String Integer -> [TId] -> [Transaction]
-- undoTs = undefined

-- -- Exercise 8 -----------------------------------------

-- writeJSON :: ToJSON a => FilePath -> a -> IO ()
-- writeJSON = undefined

-- -- Exercise 9 -----------------------------------------

-- doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
--              -> FilePath -> IO String
-- doEverything dog1 dog2 trans vict fids out = do
--   key <- getSecret dog1 dog2
--   decryptWithKey key vict
--   mts <- getBadTs vict trans
--   case mts of
--     Nothing -> error "No Transactions"
--     Just ts -> do
--       mids <- parseFile fids
--       case mids of
--         Nothing  -> error "No ids"
--         Just ids -> do
--           let flow = getFlow ts       
--           writeJSON out (undoTs flow ids)
--           return (getCriminal flow)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   crim <- 
--     case args of
--       dog1:dog2:trans:vict:ids:out:_ ->
--           doEverything dog1 dog2 trans vict ids out
--       _ -> doEverything "./clues/dog-original.jpg"
--                         "./clues/dog.jpg"
--                         "./clues/transactions.json"
--                         "./clues/victims.json"
--                         "./clues/new-ids.json"
--                         "./clues/new-transactions.json"
--   putStrLn crim
