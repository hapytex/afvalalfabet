module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text
import qualified Data.Vector as V

data WasteRecord = WasteRecord { name :: Text, location :: [Text] }

-- torecords :: (Text, Text) -> WasteRecord

main :: IO ()
main = do
    csvData <- BL.readFile "data/data.csv"
    case decode HasHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (foo, bar) ->
            putStrLn $ foo ++ " -> " ++ bar
