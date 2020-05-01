{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Char as C
import Data.Csv
import Data.List(sort)
import Data.String(fromString)
import Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V

import Text.LaTeX.Base
import Text.LaTeX.Base.Class

data WasteRecord = WasteRecord { name :: Text, specs :: Text, location :: [Text] } deriving (Eq, Ord, Show)

slug :: WasteRecord -> Text
slug (WasteRecord n s _) = T.map f (T.toLower (n <> s))
    where f c | '0' <= c && c <= '9' = c
              | 'a' <= c && c <= 'z' = c
              | otherwise = '-'

toWasteRecord :: (Text, Text, Text) -> WasteRecord
toWasteRecord (na, sp, lcs) = WasteRecord (strip na) (strip sp) (Prelude.map strip (T.splitOn "/" lcs))

newLetter :: LaTeXC l => Char -> l
newLetter c = comm1 "dictchar" (raw dc) <> comm1 "lettergroup" (raw sc)
    where c' = C.toUpper c
          sc = T.singleton c'
          dc = T.cons c' (T.cons ' ' (T.singleton (C.toLower c)))

wasteToLaTeX :: LaTeXC l => WasteRecord -> l
wasteToLaTeX w@(WasteRecord n s l) = optFixComm "entry" 1 [raw (slug w), raw n, subs <> raw (protectText (T.intercalate ", " l))] <> mconcat (Prelude.map (comm1 "index" . raw) l)
  where subs | T.null s = ""
             | otherwise = textit (raw (protectText (T.cons '(' (s <> ") "))))

wasteToLaTeX' :: LaTeXC l => (WasteRecord, WasteRecord) -> l
wasteToLaTeX' (WasteRecord a _ _, w@(WasteRecord b _ _)) = f (wasteToLaTeX w)
    where f | T.take 1 (T.toUpper a) /= T.take 1 (T.toUpper b), (c:_) <- T.unpack b = (newLetter c <>)
            | otherwise = id

main :: IO ()
main = do
    csvData <- BL.readFile "data/data.csv"
    case decode HasHeader csvData :: Either String (V.Vector (Text, Text, Text)) of
        Left err -> putStrLn err
        Right v -> execLaTeXT (_document wr') >>= TI.putStrLn . render -- (V.toList (V.map toWasteRecord v))
            where wr = (V.fromList . sort . V.toList . V.map toWasteRecord) v
                  wr' = V.zip (V.cons (WasteRecord "" "" []) wr) wr
                  

_document :: Monad m => V.Vector (WasteRecord, WasteRecord) -> LaTeXT_ m
_document entries = do
    documentclass [] "dictionary"
    usepackage [raw "dutch"] "babel"
    title "Afval-alfabet"
    author ""
    document (V.mapM_ wasteToLaTeX' entries >> section' "Locaties")
