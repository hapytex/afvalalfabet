{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map(Map)
import qualified Data.ByteString.Lazy as BL
import Data.Char as C
import Data.Csv
import Data.Function(on)
import Data.List(sort)
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

data WasteRecord = WasteRecord { name :: Text, specs :: Text, location :: [Text], tips :: [Tip] } deriving (Eq, Show)

instance Ord WasteRecord where
    compare wa wb = on compare (T.toCaseFold . name) wa wb <> on go (T.toCaseFold . specs) wa wb
        where go sa sb | T.null sa && T.null sb = EQ
                       | T.null sa = GT
                       | T.null sb = LT
                       | otherwise = compare sa sb

data WasteLocation = WasteLocation { label :: Text, locName :: Text, locBgColor :: Text, logFgColor :: Text, contact :: Text} deriving (Eq, Ord, Show)

slug :: WasteRecord -> Text
slug (WasteRecord n s _ _) = T.map f (T.toLower (n <> s))
    where f c | '0' <= c && c <= '9' = c
              | 'a' <= c && c <= 'z' = c
              | otherwise = '-'

toWasteLocation :: (Text, Text, Text, Text, Text) -> WasteLocation
toWasteLocation (l, n, cbg, cfg, t) = WasteLocation l n cbg cfg t

toWasteRecord :: (Text, Text, Text) -> WasteRecord
toWasteRecord (na, sp, lcs) = WasteRecord (titleFirst (strip na)) (strip sp) (Prelude.map strip (T.splitOn "/" lcs)) []

addTip :: WasteRecord -> Tip -> WasteRecord
addTip w@WasteRecord{tips=ts} t = w {tips=t:ts}

newLetter :: LaTeXC l => Char -> l
newLetter c = comm1 "dictchar" (raw dc) <> comm1 "lettergroup" (raw sc)
    where c' = C.toUpper c
          sc = T.singleton c'
          dc = T.cons c' (T.cons ' ' (T.singleton (C.toLower c)))

defcolor :: LaTeXC l => Text -> Text -> Text -> (l -> l, l)
defcolor _ "fg" "" = (id, "black")
defcolor _ "bg" "" = (id, "white")
defcolor n t cl = ((comm3 "definecolor" colname "HTML" (raw (T.toUpper (T.drop 1 cl))) <>), colname)
    where colname = raw (T.filter (' ' /=) (n <> "-" <> t))

locationToLaTeX :: LaTeXC l => WasteLocation -> l
locationToLaTeX (WasteLocation l a bg fg _) = cfg (cbg (comm2 "newglossaryentry" (raw l) (raw "name={" <> comm2 "colorbox" nbg (comm0 "strut" <> comm2 "textcolor" nfg (raw (protectText l))) <> comm1 "hspace*" "0.25cm" <> raw "}, description={" <> text <> raw "}")))
    where (cfg, nfg) = defcolor l "fg" fg
          (cbg, nbg) = defcolor l "bg" bg
          text | T.null a = raw (protectText l)
               | otherwise = raw (protectText a)

wasteToLaTeX :: LaTeXC l => WasteRecord -> l
wasteToLaTeX w@(WasteRecord n s l _) = optFixComm "entry" 1 [raw (slug w), raw n, subs <> mconcat (Prelude.map (comm1 "gls" . raw) l) <> mconcat (Prelude.map (optFixComm "index" 1 . (raw "locations" :) . pure . raw) l)]
  where subs | T.null s = ""
             | otherwise = textit (raw (protectText (T.cons '(' (s <> ") "))))

wasteToLaTeX' :: LaTeXC l => (WasteRecord, WasteRecord) -> l
wasteToLaTeX' (WasteRecord a _ _ _, w@(WasteRecord b _ _ _)) = f (wasteToLaTeX w)
    where f | T.take 1 (T.toUpper a) /= T.take 1 (T.toUpper b), (c:_) <- T.unpack b = (newLetter c <>)
            | otherwise = id

readCsvFile :: (FromRecord a, Show a) => FilePath -> IO (V.Vector a)
readCsvFile path = do
    csvData <- BL.readFile path
    case decode HasHeader csvData of
        Left err -> fail ("Failed to parse csv file " <> path <> ": " <> err)
        Right v -> pure v

parseCsvFile :: (FromRecord a, Show a) => (a -> b) -> FilePath -> IO (V.Vector b)
parseCsvFile f = fmap (V.map f) . readCsvFile

readLocations :: IO (V.Vector WasteLocation)
readLocations = parseCsvFile toWasteLocation "data/where.csv"

readWasteRecords :: IO (V.Vector WasteRecord)
readWasteRecords = parseCsvFile toWasteRecord "data/data.csv"

main :: IO ()
main = do
    wl <- readLocations
    wr <- readWasteRecords
    let _wr = (V.fromList . sort . V.toList) wr
        wr' = V.zip (V.cons (WasteRecord "" "" [] []) _wr) _wr
    execLaTeXT (_document wl wr') >>= TI.putStrLn . render -- (V.toList (V.map toWasteRecord v))
                  

_document :: Monad m => V.Vector WasteLocation -> V.Vector (WasteRecord, WasteRecord) -> LaTeXT_ m
_document locations entries = do
    documentclass [] "dictionary"
    usepackage [raw "dutch"] "babel"
    usepackage [] "index"
    usepackage [] "glossaries"
    usepackage [] "xcolor"
    usepackage [] "fancybox"
    comm0 "makeindex"
    comm0 "makeglossaries"
    comm4 "newindex" (raw "locations") (raw "adx") (raw "and") (raw "Locaties")
    mapM_ locationToLaTeX locations
    title "Afvalwoordenboek"
    author "Willem Van Onsem"
    document (V.mapM_ wasteToLaTeX' entries >> newpage >> comm0 "printindex" >> optFixComm "printindex" 1 [raw "locations"])
