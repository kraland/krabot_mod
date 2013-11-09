{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Data.ByteString      as B
-- import qualified Data.ByteString.Lazy    as LB
import qualified Data.ByteString.UTF8          as UB
import qualified Data.ByteString.Lazy.Internal as LBI
-- Data.Text.Lazy
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
-- Data.Text
import qualified Data.Text.Encoding      as TE
-- Data.Time
import           Data.Time.Format
import qualified Data.Time.Clock         as DTC
import qualified Data.Time.Calendar      as DTC2

import Control.Monad
import           System.Locale
import           Data.Maybe
import           Data.List
import           Data.List.Utils
import           Data.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Conduit.Browser
import           Text.StringLike
import           Text.HTML.TagSoup

getToken :: (Show str, Eq str, StringLike str) => [Tag str] -> str
getToken xs =
  fromAttrib value . head $ dropWhile (~/= ("<input name=t>" :: String)) xs
  where value = fromString ("value" :: String)

sameDay :: DTC.UTCTime -> DTC.UTCTime -> Bool
sameDay d1 d2 =
  let fmt = formatTime defaultTimeLocale "%d/%m/%Y" in
  fmt d1 == fmt d2

shiftDay :: Integer -> DTC.UTCTime -> DTC.UTCTime
shiftDay diff day = day { DTC.utctDay = DTC2.addDays diff $ DTC.utctDay day }

data Kramail str = Kramail
  {  title  :: str
  ,  target :: str
  ,  date   :: DTC.UTCTime }

instance StringLike str => Show (Kramail str) where
  show kr =
    (toString $ title kr) ++
    "\nsent to: " ++ (toString $ target kr) ++
    " on " ++ show (date kr) ++
    "\n\n"

extractDate :: StringLike str => DTC.UTCTime -> str -> DTC.UTCTime
extractDate tdy str =
  let loc = defaultTimeLocale
      fmt = formatTime loc "%d/%m"
      rel = flip shiftDay tdy
      str' = replace "Aujourd'hui" (fmt tdy) $
             replace "Hier"        (fmt $ rel (-1)) $
             replace "Avant-Hier"  (fmt $ rel (-2)) $ toString str
  in readTime loc "%Y/%d/%m (%R)" $ "2013/" ++ str'

extractNumPages :: (StringLike str, Show str) => [Tag str] -> Int
extractNumPages =
  read . toString . fromTagText . head . filter isTagText .
  last . sections (~== ("<a>" :: String)) .
  takeWhile (~/= ("<select>" :: String)) .
  dropWhile (~/= ("<tr class=forum-c3>" :: String))

extractKramails :: (Show str, StringLike str) =>
                   DTC.UTCTime -> [Tag str] -> [Kramail str]
extractKramails tdy xs =
  let table = takeWhile (~/= ("<td colspan=6>" :: String)) $
              dropWhile (~/= ("<tr class=forum-c1>" :: String)) xs
      lines = map (takeWhile (~/= ("</tr>" :: String))) $
              sections (~== ("<tr>" :: String)) table
  in catMaybes $ map (extractKramail tdy) $ init . init $ lines

extractKramail :: (Show str, StringLike str) => DTC.UTCTime -> [Tag str] ->
                  Maybe (Kramail str)
extractKramail tdy xs = do
  let xxs    = sections (~== ("<td>" :: String)) $ iterate tail xs !! 8
      fields = map (takeWhile (~/= ("</td>" :: String))) xxs
  (obj : trg : prd : _) <- return $ map f fields
  return $ Kramail { title  = obj,
                     target = trg,
                     date   = extractDate tdy prd}
  where f = strConcat . map fromTagText . (filter isTagText)

parse :: Response LBI.ByteString -> [Tag String]
-- beware: TL.unpack is O(n) ; it is maybe possible to do better?
parse = parseTags . TL.unpack . TLE.decodeLatin1 . responseBody

-- The web request to log in to a service
connect :: StringLike str => str -> str -> Request (ResourceT IO)
connect login pass =
  let req = fromJust . parseUrl $
            "http://www.kraland.org/main.php?p=8_1_1259_1&a=100"
  in  urlEncodedBody
        [ (UB.fromString "p1", UB.fromString . castString $ login)
        , (UB.fromString "p2", UB.fromString . castString $ pass) ]
      req

select :: String -> Request (ResourceT IO)
select xs =
  let req = fromJust . parseUrl $ "http://www.kraland.org/main.php?p=8_1_1259_1"
  in urlEncodedBody [ (UB.fromString "t", UB.fromString xs)
                 , (UB.fromString "a", UB.fromString "1")
                 , (UB.fromString "p", UB.fromString "8_1_1259_1")
                 , (UB.fromString "p2", UB.fromString "50")
                 , (UB.fromString "p5", UB.fromString "1260")
                 ] req

-- extractAllKramails goes throught all the pages
extractAllKramails :: DTC.UTCTime -> Int ->
                      GenericBrowserAction (ResourceT IO) ([Kramail String])
extractAllKramails tdy n =
  fmap concat $ forM [1..n] $ \ page -> do
  url <- parseUrl $ "http://www.kraland.org/main.php?p=8_1_1259_1_" ++ show page
  req <- makeRequestLbs url
  return $ extractKramails tdy $ parse req

-- this is the main script:
--   starts by connecting to the interface
--   then selects the right options to minimize the number of requests
--   and finally pulls all the data available
getAllKramails tdy = do
  con <- makeRequestLbs $ connect ("hahaha"  :: String) ("ouécéça" :: String)
  sel <- makeRequestLbs $ select . getToken . parse $ con
  extractAllKramails tdy . extractNumPages . parse $ sel

main :: IO ()
main = do
  man <- newManager def
  tdy <- DTC.getCurrentTime
  query <- return $ getAllKramails tdy
  kramails <- runResourceT $ browse man query
--  putStrLn . UB.toString . B.concat . LB.toChunks . responseBody $ out
--  flip mapM_ kramails $ putStrLn . show
  mapM_ (putStr . measure kramails tdy) (reverse [(-7)..0])
  where
    measure kr tdy d =
      let thatDay           = shiftDay d tdy
          criterion crit kr = title kr == crit && sameDay thatDay (date kr)
          printEmAll crit   = show . length . filter (criterion crit) in
      concat
        [ DTC2.showGregorian $ DTC.utctDay $ thatDay, ": "
        , "Modérés: "  , printEmAll "[Modération] Message modéré"   kr, "; "
        , "Détruits: " , printEmAll "[Modération] Message détruit"  kr, "; "
        , "Restaurés: ", printEmAll "[Modération] Message restauré" kr, "; "
        , "Déplacés: " , printEmAll "[Modération] Sujet déplacé"    kr
        , "\n" ]
