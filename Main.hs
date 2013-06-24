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

-- jump to a specific tag following a path of tags
jumpTos :: [String] -> [Tag String] -> [Tag String]
jumpTos = flip $ foldl (\ xs s -> dropWhile (~/= s) xs)
jumpTo s = jumpTos [s]

getToken :: [Tag String] -> String
getToken xs = fromAttrib "value" $ head $ jumpTo "<input name=t>" xs

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
  show kr = title kr ++ "\nsent to: " ++ target kr ++ " on " ++ show (date kr)

extractDate :: DTC.UTCTime -> String -> DTC.UTCTime
extractDate tdy str =
  let loc = defaultTimeLocale
      fmt = formatTime loc "%d/%m"
      rel = flip shiftDay tdy
      str' = replace "Aujourd'hui" (fmt tdy) $
             replace "Hier"        (fmt $ rel (-1)) $
             replace "Avant-Hier"  (fmt $ rel (-2)) str
  in readTime loc "%Y/%d/%m (%R)" $ "2013/" ++ str'

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

bind :: Monad m => GenericBrowserAction m (Response LB.ByteString) ->
        ([Tag String] -> GenericBrowserAction m (Response LB.ByteString)) ->
        GenericBrowserAction m (Response LB.ByteString)
bind req1 req2 = do
  r1 <- req1
  req2 $ parse r1


-- The web request to log in to a service
connect :: Request m -> String -> String -> Request (ResourceT IO)
connect req login pass =
  urlEncodedBody [ (UB.fromString "p1", UB.fromString login)
                 , (UB.fromString "p2", UB.fromString pass)
                 ] req

select :: Request m -> [Tag String] -> Request (ResourceT IO)
select req xs =
  urlEncodedBody [ (UB.fromString "t", UB.fromString $ getToken xs)
                 , (UB.fromString "a", UB.fromString "1")
                 , (UB.fromString "p", UB.fromString "8_1_1259_1")
                 , (UB.fromString "p2", UB.fromString "50")
                 , (UB.fromString "p5", UB.fromString "1260")
                 ] req

main :: IO ()
main = do
  man <- newManager def
  urlcon <- parseUrl "http://www.kraland.org/main.php?p=8_1_1259_1&a=100"
  urlsel <- parseUrl "http://www.kraland.org/main.php?p=8_1_1259_1"
  sel <- return $
    bind (makeRequestLbs $ connect urlcon "username" "password")
         (makeRequestLbs . (select urlsel))
  out <- runResourceT $ browse man sel
  tdy <- DTC.getCurrentTime
--  putStrLn . UB.toString . B.concat . LB.toChunks . responseBody $ out
  kramails <- return $ extractKramails tdy $ parse out
  mapM_ (measure kramails tdy) [(-2)..0]
  where
    measure kr tdy d = do
      () <- putStrLn $ (++) "Modérés: " $ show . length $
        filter (modere $ shiftDay d tdy) $ kr
      putStrLn $ (++) "Détruits: " $ show . length $
        filter (detruit $ shiftDay d tdy) $ kr
    modere  tdy kr =
      title kr == "[Mod�ration] Message mod�r�" && sameDay tdy (date kr)
    detruit tdy kr =
      title kr == "[Mod�ration] Message d�truit" && sameDay tdy (date kr)
