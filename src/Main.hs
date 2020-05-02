{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map(Map)
import qualified Data.ByteString.Lazy as BL
import Data.Char as C
import Data.Csv
import Data.List(sortOn)
-- import Data.String(fromString)
import Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V

import Text.LaTeX.Base
import Text.LaTeX.Base.Class

type WasteIndex = Map (Text, Text) WasteRecord

titleFirst :: Text -> Text
titleFirst t = T.toUpper t1 <> t2
    where (t1, t2) = T.splitAt 1 t

newtype Tip = Tip Text deriving (Eq, Ord, Show)

data WasteRecord = WasteRecord { name :: Text, specs :: Text, location :: [Text], tips :: [Tip] } deriving (Eq, Ord, Show)

data WasteLocation = WasteLocation { label :: Text, locName :: Text, locColor :: Text , contact :: Text}

slug :: WasteRecord -> Text
slug (WasteRecord n s _ _) = T.map f (T.toLower (n <> s))
    where f c | '0' <= c && c <= '9' = c
              | 'a' <= c && c <= 'z' = c
              | otherwise = '-'

toWasteLocation :: (Text, Text, Text, Text) -> WasteLocation
toWasteLocation (l, n, c, t) = WasteLocation l n c t

toWasteRecord :: (Text, Text, Text) -> WasteRecord
toWasteRecord (na, sp, lcs) = WasteRecord (titleFirst (strip na)) (strip sp) (Prelude.map strip (T.splitOn "/" lcs)) []

orderRecord :: WasteRecord -> (Text, Text)
orderRecord (WasteRecord t s _ _ ) = (T.toCaseFold t, T.toCaseFold s)

addTip :: WasteRecord -> Tip -> WasteRecord
addTip w@WasteRecord{tips=ts} t = w {tips=t:ts}

newLetter :: LaTeXC l => Char -> l
newLetter c = comm1 "dictchar" (raw dc) <> comm1 "lettergroup" (raw sc)
    where c' = C.toUpper c
          sc = T.singleton c'
          dc = T.cons c' (T.cons ' ' (T.singleton (C.toLower c)))

wasteToLaTeX :: LaTeXC l => WasteRecord -> l
wasteToLaTeX w@(WasteRecord n s l _) = optFixComm "entry" 1 [raw (slug w), raw n, subs <> raw (protectText (T.intercalate ", " l)) <> mconcat (Prelude.map (optFixComm "index" 1 . (raw "locations" :) . pure . raw) l)]
  where subs | T.null s = ""
             | otherwise = textit (raw (protectText (T.cons '(' (s <> ") "))))

wasteToLaTeX' :: LaTeXC l => (WasteRecord, WasteRecord) -> l
wasteToLaTeX' (WasteRecord a _ _ _, w@(WasteRecord b _ _ _)) = f (wasteToLaTeX w)
    where f | T.take 1 (T.toUpper a) /= T.take 1 (T.toUpper b), (c:_) <- T.unpack b = (newLetter c <>)
            | otherwise = id


readLocations :: IO (V.Vector WasteLocation)
readLocations = do
    csvData <- BL.readFile "data/where.csv"
    pure $ case decode HasHeader csvData of
        Left err -> fail ("Failed to parse the location file: " <> err)
        Right v -> V.map toWasteLocation v

readWasteRecords :: IO (V.Vector WasteRecord)
readWasteRecords = do
    csvData <- BL.readFile "data/data.csv"
    pure $ case decode HasHeader csvData of
        Left err -> fail ("Failed to parse the entries file: " <> err)
        Right v -> V.map toWasteRecord v


main :: IO ()
main = do
    wl <- readLocations
    wr <- readWasteRecords
    let _wr = (V.fromList . sortOn orderRecord . V.toList) wr
        wr' = V.zip (V.cons (WasteRecord "" "" [] []) _wr) _wr
    execLaTeXT (_document wr') >>= TI.putStrLn . render -- (V.toList (V.map toWasteRecord v))
                  

_document :: Monad m => V.Vector (WasteRecord, WasteRecord) -> LaTeXT_ m
_document entries = do
    documentclass [] "dictionary"
    usepackage [raw "dutch"] "babel"
    usepackage [] "index"
    usepackage [] "xcolor"
    comm0 "makeindex"
    comm4 "newindex" (raw "locations") (raw "adx") (raw "and") (raw "Locaties")
    title "Afval-alfabet"
    author ""
    document (V.mapM_ wasteToLaTeX' entries >> newpage >> comm0 "printindex" >> optFixComm "printindex" 1 [raw "locations"])
