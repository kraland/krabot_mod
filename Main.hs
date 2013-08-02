module Main where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UB
import qualified Data.Time.Clock      as DTC
import qualified Data.Time.Calendar   as DTC2
import           System.Locale
import           Data.Time.Format
import           Data.Maybe
import           Data.List.Utils
import           Data.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Conduit.Browser
import           Text.HTML.TagSoup

import System.IO.Unsafe

getToken :: [Tag String] -> String
getToken xs = fromAttrib "value" $ head $ dropWhile (~/= "<input name=t>") xs

sameDay :: DTC.UTCTime -> DTC.UTCTime -> Bool
sameDay d1 d2 =
  let fmt = formatTime defaultTimeLocale "%d/%m/%Y" in
  fmt d1 == fmt d2

shiftDay :: Integer -> DTC.UTCTime -> DTC.UTCTime
shiftDay diff day = day { DTC.utctDay = DTC2.addDays diff $ DTC.utctDay day }

data Kramail = Kramail
  {  title  :: String
  ,  target :: String
  ,  date   :: DTC.UTCTime }

instance Show Kramail where
  show kr =
    title kr ++
    "\nsent to: " ++ target kr ++
    " on " ++ show (date kr) ++
    "\n\n"

extractDate :: DTC.UTCTime -> String -> DTC.UTCTime
extractDate tdy str =
  let loc = defaultTimeLocale
      fmt = formatTime loc "%d/%m"
      rel = flip shiftDay tdy
      str' = replace "Aujourd'hui" (fmt tdy) $
             replace "Hier"        (fmt $ rel (-1)) $
             replace "Avant-Hier"  (fmt $ rel (-2)) str
  in readTime loc "%Y/%d/%m (%R)" $ "2013/" ++ str'

extractNumPages :: [Tag String] -> Int
extractNumPages =
  read . fromTagText . head . filter isTagText .
  last . sections (~== "<a>") .
  takeWhile (~/= "<select>") .
  dropWhile (~/= "<tr class=forum-c3>")

extractKramails :: DTC.UTCTime -> [Tag String] -> [Kramail]
extractKramails tdy xs =
  let table = takeWhile (~/= "<td colspan=6>") $
              dropWhile (~/= "<tr class=forum-c1>") xs
      lines = map (takeWhile (~/= "</tr>")) $  sections (~== "<tr>") table
  in catMaybes $ map (extractKramail tdy) $ init . init $ lines

extractKramail :: DTC.UTCTime -> [Tag String] -> Maybe Kramail
extractKramail tdy xs = do
  xxs <- return $ sections (~== "<td>") $ iterate tail xs !! 8
  fields <- return $ map (takeWhile (~/= "</td>")) xxs
  (obj : trg : prd : _) <- return $ map f fields
  return $ Kramail { title  = obj,
                     target = trg,
                     date   = extractDate tdy prd}
  where f = (concatMap fromTagText) . (filter isTagText)

parse = parseTags . UB.toString . B.concat . LB.toChunks . responseBody

-- The web request to log in to a service
connect :: String -> String -> Request (ResourceT IO)
connect login pass =
  let req = fromJust . parseUrl $
            "http://www.kraland.org/main.php?p=8_1_1259_1&a=100"
  in urlEncodedBody [ (UB.fromString "p1", UB.fromString login)
                 , (UB.fromString "p2", UB.fromString pass)
                 ] req

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
extractAllKramails tdy 0    = return []
extractAllKramails tdy page = do
  url <- parseUrl $ "http://www.kraland.org/main.php?p=8_1_1259_1_" ++ show page
  req <- makeRequestLbs url
  rec <- extractAllKramails tdy (page - 1)
  return $ (++) rec $ extractKramails tdy $ parse req

-- this is the main script:
--   starts by connecting to the interface
--   then selects the right options to minimize the number of requests
--   and finally pulls all the data available
getAllKramails tdy = do
  con <- makeRequestLbs $ connect "hahaha" "ouécéça"
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
  mapM_ (putStr . measure kramails tdy) (reverse [(-2)..0])
  where
    measure kr tdy d =
      concat [ DTC2.showGregorian $ DTC.utctDay $ shiftDay d tdy, ": ",
               "Modérés: ",
               show . length $ filter (modere $ shiftDay d tdy) $ kr, "; ",
               "Détruits: ",
               show . length $ filter (detruit $ shiftDay d tdy) $ kr, "\n" ]
    modere  tdy kr =
      title kr == "[Mod�ration] Message mod�r�" && sameDay tdy (date kr)
    detruit tdy kr =
      title kr == "[Mod�ration] Message d�truit" && sameDay tdy (date kr)
