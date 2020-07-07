{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bool(bool)
import qualified Data.ByteString.Lazy as BL
import Data.Char as C
import Data.Csv
import Data.Function(on)
import Data.List(sort)
import Data.Map(Map, update)
import qualified Data.Map as M
import Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V

import System.Console.GetOpt
import System.Environment

import Text.LaTeX.Base
import Text.LaTeX.Base.Class

type WasteIndex = Map (Text, Text) WasteRecord

titleFirst :: Text -> Text
titleFirst t = T.toUpper t1 <> t2
    where (t1, t2) = T.splitAt 1 t

newtype Tip = Tip {untip :: Text} deriving (Eq, Ord, Show)

data WasteRecord = WasteRecord { name :: Text, specs :: Text, location :: [Text], tips :: [Tip] } deriving (Eq, Show)

instance Ord WasteRecord where
    compare wa wb = on compare (T.toCaseFold . name) wa wb <> on go (T.toCaseFold . specs) wa wb
        where go sa sb | T.null sa && T.null sb = EQ
                       | T.null sa = GT
                       | T.null sb = LT
                       | otherwise = compare sa sb

data WasteLocation = WasteLocation { label :: Text, locName :: Text, locBgColor :: Text, logFgColor :: Text, contact :: Text} deriving (Eq, Ord, Show)

slug :: Text -> Text
slug = T.map f . T.toLower
    where f c | '0' <= c && c <= '9' = c
              | 'a' <= c && c <= 'z' = c
              | otherwise = '-'

slug' :: WasteRecord -> Text
slug' (WasteRecord n s _ _) = slug (n <> s)

slug'' :: WasteLocation -> Text
slug'' = slug . Main.label

toWasteLocation :: (Text, Text, Text, Text, Text) -> WasteLocation
toWasteLocation (l, n, cbg, cfg, t) = WasteLocation l n cbg cfg t

toTip :: (Text, Text, Text) -> (Text, Text, Tip)
toTip (k, s, t) = (k, s, Tip t)

toWasteRecord :: (Text, Text, Text, Text) -> WasteRecord
toWasteRecord (na, sp, lcs, _) = WasteRecord (titleFirst (strip na)) (strip sp) (Prelude.map strip (T.splitOn "/" lcs)) []

addTip' :: Tip -> WasteRecord -> WasteRecord
addTip' t w@WasteRecord{tips=ts} = w {tips=t:ts}

addTip :: WasteIndex -> (Text, Text, Tip) -> WasteIndex
addTip w0 (k, s, t) = update (Just <$> addTip' t) (k, s) w0

addTips :: Foldable f => WasteIndex -> f (Text, Text, Tip) -> WasteIndex
addTips = Prelude.foldl addTip

newLetter :: LaTeXC l => Char -> l
newLetter c = comm1 "dictchar" (raw dc) <> comm1 "lettergroup" (raw sc)
    where c' = C.toUpper c
          sc = T.singleton c'
          dc = T.cons c' (T.cons ' ' (T.singleton (C.toLower c)))

defcolor :: LaTeXC l => Bool -> Text -> Text -> Text -> (l -> l, l)
defcolor d _ "fg" "" = (id, bool "black" "white" d)
defcolor d _ "bg" "" = (id, bool "white" "black" d)
defcolor _ n t cl = ((comm3 "definecolor" colname "HTML" (raw (T.toUpper (T.drop 1 cl))) <>), colname)
    where colname = raw (T.filter (' ' /=) (n <> "-" <> t))

locationToLaTeX :: LaTeXC l => RenderOptions -> WasteLocation -> l
locationToLaTeX ro (WasteLocation l a bg fg _) = cfg (cbg (comm2 "newglossaryentry" (raw l) (raw "name={" <> comm1 "hspace*" "0.125cm" <> comm2 "colorbox" nbg (comm0 "strut" <> comm2 "textcolor" nfg (raw (protectText l))) <> raw "}, description={" <> text <> raw "}")))
    where d = dark ro
          (cfg, nfg) = defcolor d l "fg" fg
          (cbg, nbg) = defcolor d l "bg" bg
          text | T.null a = raw (protectText l)
               | otherwise = raw (protectText a)

locationToLaTeX2 :: LaTeXC l => RenderOptions -> WasteLocation -> l
locationToLaTeX2 ro wl = raw "" -- comm1 "label" (raw ("loc:" <> (slug'' wl))) <> section (raw (locName wl))

wasteToLaTeX :: LaTeXC l => WasteRecord -> l
wasteToLaTeX w@(WasteRecord n s l ts) = optFixComm "entry" 1 [raw (slug' w), raw n, subs <> raw " " <> mconcat (Prelude.map (comm1 "gls" . raw) l) <> mconcat (Prelude.map (optFixComm "index" 1 . (raw "locations" :) . pure . raw . protectText) l) <> raw "\\\\" <> Prelude.foldMap (comm1 "hint" . raw . protectText . untip) ts]
  where subs | T.null s = ""
             | otherwise = {- comm1 "hspace*" "0.25cm" <> -} textit (raw (protectText (T.cons '(' (s <> ") "))))

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

readTips :: IO (V.Vector (Text, Text, Tip))
readTips = parseCsvFile toTip "data/tips.csv"

newtype RenderOptions = RenderOptions { dark :: Bool }

headerCommands :: Monad m => RenderOptions -> LaTeXT_ m
headerCommands r
    | dark r = comm1 "pagecolor" "black" >> comm1 "color" "white"
    | otherwise = pure ()

options :: [OptDescr (RenderOptions -> RenderOptions)]
options = [
    Option ['d'] ["dark"] (NoArg (\o -> o{dark=True})) "Use a dark theme"
  ]

main :: IO ()
main = do
    argv <- getArgs
    ro <- case getOpt Permute options argv of
        (o, n, []) -> pure (Prelude.foldr ($) (RenderOptions False) o)
        _ -> fail "Invalid program parameters"
    wl <- readLocations
    wr <- readWasteRecords
    tp <- readTips
    let wrt = addTips (M.fromList (Prelude.map (\w -> ((name w, specs w), w)) (V.toList wr))) tp
    let _wr = (V.fromList . sort . M.elems) wrt
        wr' = V.zip (V.cons (WasteRecord "" "" [] []) _wr) _wr
    execLaTeXT (_document ro wl wr') >>= TI.putStrLn . render
                  

_document :: Monad m => RenderOptions -> V.Vector WasteLocation -> V.Vector (WasteRecord, WasteRecord) -> LaTeXT_ m
_document ro locations entries = do
    documentclass ["titlepage", "8pt"] "dictionary"
    usepackage [raw "dutch"] "babel"
    usepackage [] "index"
    -- usepackage [] "glossaries"
    usepackage [] "xcolor"
    usepackage [] "titleps"
    usepackage ["document"] "ragged2e"
    headerCommands ro
    comm0 "makeindex"
    comm0 "makeglossaries"
    comm4 "newindex" (raw "locations") (raw "adx") (raw "and") (raw "Locaties")
    mapM_ (locationToLaTeX ro) locations
    title "Afval-sorteer-woordenboek"
    author (raw "Willem Van Onsem \\and Lindsey Louwyck")
    document (env0 "dictionary" (mapM_ wasteToLaTeX' entries) >> newpage >> mapM_ (locationToLaTeX2 ro) locations >> optFixComm "printindex" 1 [raw "locations"])
