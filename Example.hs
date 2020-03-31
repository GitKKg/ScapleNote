-- | 
{-# LANGUAGE OverloadedStrings ,DuplicateRecordFields #-}

--module Haskell.Scalpel.Example where
-- must comment out, or else stack build will complain "output was redirected with -o, but no output will be generated. because there is no Main module."

import Text.HTML.Scalpel
import Control.Applicative

import Network.HTTP.Client

import qualified Data.ByteString.Lazy.Char8 as L8

--import           Network.HTTP.Simple

import Text.HTML.TagSoup

import Network.Connection
import Network.Socket
import Network.HTTP.Client.TLS

import Network.HTTP.Types.Status (statusCode)
-- Scraper is just like a converting format  from some input to output
-- Selector is just like a selecting format to select DOM node
-- chroot accept an old Scraper and output a new Scraper,because it add a range by Selector for old Scraper

import Control.Exception
import Data.Either
import Data.Typeable

import Network.HTTP.Types.Header
import Data.CaseInsensitive

import Data.Text.Encoding

import Data.Maybe

import Db -- Db.hs in the same directory,could import directly

import Text.Regex.Base.RegexLike
import Text.Regex.Posix.String

import Data.Functor

import Data.Text


import qualified Data.Text.Encoding as T       -- text
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as ICU  -- text-icu   ,proxychains stack install text-icu
import qualified Data.Text.ICU as ICU

import Data.Text.IO as T (putStr)

hiStock = defaultStock {_date = 2020} :: Stock

type Author = String
data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

allComments :: IO (Maybe [Comment])
-- scrapeURL not support file:/// like protocol, raise Exception: InvalidUrlException "file:///home/kyle/Haskell/ScalpelNote/article.html" "Invalid scheme"
allComments = --scrapeURL "file:///home/kyle/Haskell/ScalpelNote/article.html" comments

  -- this one below is more understoobable
  -- return . flip scrapeStringLike comments =<< (readFile "article.html")
  
  -- (<&>) :: Functor f => f a -> (a -> b) -> f b , just reverted order of <$>
  -- below one pass too, but more obscure, $ is passive voice
  ((return .  scrapeStringLike)  =<< (readFile "article.html")) <&> ($ comments)
   where
       comments :: Scraper String [Comment]
       comments = chroots ("div" @: [hasClass "container"]) comment

       comment :: Scraper String Comment
       comment = textComment <|> imageComment


       textComment :: Scraper String Comment
       textComment = do
           --author      <- text $ "span" @: [hasClass "author"]
           author      <- text $ "span" @: [AttributeString "class" @= "comment author",notP $ AttributeString "ok" @= "bad" ]

       -- get all tags named "span" whose class just exactly is "comment author", same result
           -- author      <- text $ "span" @: ["class" @= "comment author"]
           commentText <- text $ "div"  @: [hasClass "text"]
           return $ TextComment author commentText

       imageComment :: Scraper String Comment
       imageComment = do
           author   <- text       $ "span" @: [hasClass "author"]
           imageURL <- attr "src" $ "img"  @: [hasClass "image"]
           return $ ImageComment author imageURL

s :: String
s = "<div id=\"out\"><div id=\"in\"></div></div>"
--result = scrapeStringLike s (attrs "id" "div")
results = scrapeStringLike s (attrs "id" ("div" @: [AttributeString "id" @= "out" ] ))
-- Just ["out", "in"]
-- attr just return attribute's value
result = scrapeStringLike s (attr "id" ("div" @: [AttributeString "id" @= "out" ] ))
-- Just "out"

s2 :: String
s2 = "<div><div>A</div></div>"
r2s = scrapeStringLike s2 (htmls "div")
r2 = scrapeStringLike s2 (html "div")

s3 :: String
s3 = "<div>Hello <div>World</div></div>"

r3s = scrapeStringLike s3 (texts "div")

s4 :: String
s4 = "<div><div>A</div></div>"  -- no need to write as \"A\" ,A Here is just string default
r4s = scrapeStringLike s4 (chroots "div" (pure 0))

sa :: Scraper String String
sa = return "A"

sa2 :: Scraper String String
sa2 = text "div"

r4ss = scrapeStringLike s4 (chroots "div" sa)
r4ss2 = scrapeStringLike s4 (chroots "div" sa2)

ss = "<div id=\"out\">hello<div id=\"in\"></div></div>" :: String
cc = parseTags ss
-- [TagOpen "div" [("id","out")],TagText "hello",TagOpen "div" [("id","in")],TagClose "div",TagClose "div"]

ccs = scrape (text anySelector) cc
-- Just "hello

dds = scrapeStringLike ss (text anySelector)
-- Just "hello"

bing = scrapeURL "www.bing.com" (text "div") :: IO (Maybe String)


-- use document.charset to check encoding format of html

-- newManager  managerSetProxy useProxy :: Proxy -> ProxyOverride
-- Network.HTTP.Client.TLS
-- tlsManagerSettings
-- type SockSettings = ProxySettings  SockSettingsSimple HostName PortNumber
-- TLSSettingsSimple Simple TLS settings. recommended to use

tlsSetting = TLSSettingsSimple False False False -- import Network.Connection
hostAddr = "127.0.0.1" :: HostName -- import Network.Socket
-- using 192.168.1.2 will Exception as ConnectionTimeout
portNumber = 5678 :: PortNumber
proxySetting = SockSettingsSimple hostAddr portNumber

-- import Network.HTTP.Client.TLS
--managerSetting = mkManagerSettings tlsSetting (Just proxySetting)
-- newManager :: ManagerSettings -> IO Manager
-- v2Manger = newManager managerSetting
--setGlobalManager :: Manager -> IO ()

bingHeaderName :: HeaderName
bingHeaderName = undefined
  
bingRqHeaders :: RequestHeaders
-- all content are from F12 Network document of Firefox when access www.bing.com
bingRqHeaders = [(hAccept,"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
                 --AcceptEncoding seems html document changed, so not set
                --(hAcceptEncoding,"gzip, deflate, br"),
                (hAcceptLanguage,"zh,en-US;q=0.7,en;q=0.3"),
                (hCacheControl,"max-age=0"),
                (hConnection,"keep-alive"),
                -- Cookie make Bing select show en not zh
                (hCookie,"MUID=38AA05FA31096B2508B509D630276A47; SRCHD=AF=NOFORM; SRCHUID=V=2&GUID=81C4D836F1224072B3983349F98FCEE6&dmnchg=1; SRCHUSR=DOB=20180714&T=1582533772000; SRCHHPGUSR=CW=25&CH=811&DPR=1&UTC=480&WTS=63719414472&HV=1583817677; ENSEARCH=BENVER=1; ULC=P=792C|86:@34&H=792C|86:34&T=792C|86:34:7; _ITAB=STAB=TR; MUIDB=38AA05FA31096B2508B509D630276A47; _UR=MC=1; _FP=hta=off; SNRHOP=I=&TS=; _EDGE_S=mkt=zh-cn&SID=0D23D493C3576A113AF8DA1AC2286B47; _SS=SID=0D23D493C3576A113AF8DA1AC2286B47&bIm=542; ipv6=hit=1583821277045&t=4"),
                (hHost,"cn.bing.com"),
                (hTE,"Trailers"),
                (hUpgrade,"1"),
                (hUserAgent,"Mozilla/5.0 (X11; Linux x86_64; rv:68.0) Gecko/20100101 Firefox/68.0")]

-- must take the whole op outside of where clause
getGoogle requestGoogle v2manager = do
            responseGoogleE <-
              try $  httpLbs requestGoogle v2manager :: IO (Either HttpException (Response L8.ByteString))
            case responseGoogleE of
              Left e  -> do
                print "failed to get google\n"
                print e
              Right responseGoogle -> do
                putStrLn $ "The Google status code was: " ++ (show $ statusCode $ responseStatus responseGoogle)
                print $ responseHeaders responseGoogle
                print $ scrapeStringLike (responseBody responseGoogle) (attr "itemtype" "html")

getGoogleCatch requestGoogle v2manager = do
  responseGoogle <- httpLbs requestGoogle v2manager
  putStrLn $ "The Google status code was: " ++ (show $ statusCode $ responseStatus responseGoogle)
  print $ scrapeStringLike (responseBody responseGoogle) (attr "itemtype" "html")
handleE (SomeException e) = do
  putStrLn $ "\n Caught exception of type "  ++ show (typeOf e) ++ "\n"
  putStrLn $ show e

getGoogleAndBing :: IO ()
getGoogleAndBing = do
  -- let managerSetting = mkManagerSettings tlsSetting (Just proxySetting)
  -- let v2Manger = newManager managerSetting
  -- manager <- v2Manger
  -- setGlobalManager manager
  -- string <-scrapeURL "https://www.google.com" (text "div") :: IO (Maybe String)
  -- print string
  let v2managerSetting = mkManagerSettings tlsSetting (Just proxySetting)
  v2manager <- newManager v2managerSetting
  systemManager <- newManager tlsManagerSettings
  -- manager <- newManager defaultManagerSettings
  requestBingNoHead <- parseRequest "https://www.bing.com"
  let requestBing = requestBingNoHead {requestHeaders = bingRqHeaders }
  print $ "Bing request header is"
  print $ requestHeaders requestBing -- Just [],should fill
  
  requestGoogle <- parseRequest "https://www.google.com/"
  print $ "Google request header is"
  print $ requestHeaders requestGoogle -- Just [],should fill
  getGoogle requestGoogle v2manager 
    -- where getGoogle requestGoogle v2manager = do
    --         responseGoogleE <-
    --           try $  httpLbs requestGoogle v2manager :: IO (Either HttpException (Response L8.ByteString))
    --         case responseGoogleE of
    --           Left e  -> print "failed to get google\n"
    --           Right responseGoogle -> do
    --             putStrLn $ "The Google status code was: " ++ (show $ statusCode $ responseStatus responseGoogle)
    --             print $ scrapeStringLike (responseBody responseGoogle) (attr "itemtype" "html")
  -- Note!!: no sentences could follow after where clause!
  -- print "could not follow getGoogle for getGoogle gets where"
  
  responseBing <- httpLbs requestBing systemManager
  putStrLn $ "The Bing status code was: " ++ (show $ statusCode $ responseStatus responseBing)
  print $ scrapeStringLike (responseBody responseBing) (attr "lang" "html")
  print $ responseHeaders responseBing

  -- F12 ,Network ,document,Headers,Response Headers
  -- from response header we know content-type is text/html; charset=utf-8,so must use decodeUtf8 to get human-readable text from responseBody
  print $ decodeUtf8 ( L8.toStrict  (responseBody responseBing)) -- import Data.Text.Encoding
  -- decodeUtf8 $ ((L8.toStrict) . (L8.pack)) "Chinese Characters"

  --print $ responseBody responseBing
  print "Google again by Catch"
  catch (getGoogleCatch requestGoogle v2manager) handleE
  print "over"
  -- IO can't be followed after catch,why?
  --print "not ok"

  
-- result in GHCI
--  getGoogleAndBing
-- The Google status code was: 200
-- Just "http://schema.org/WebPage"
-- The Bing status code was: 200
-- Just "zh"


-- main2 :: IO ()
-- main2 = do
--     response <- httpLBS "http://httpbin.org/get"

--     putStrLn $ "The status code was: " ++
--                show (getResponseStatusCode response)
--     print $ getResponseHeader "Content-Type" response
--     L8.putStrLn $ getResponseBody response

-- "div" @: [AttributeString "id" @= "out" ]

-- atDepth just describe relationship of a // b ,about b is how many layers in a
stockName :: Selector
stockName = "h1" @: [AttributeString "class" @= "title_01"]  // "span" @: [ hasClass "name"] `atDepth` 1 
-- hasClass not accept value with space seperated string
-- so space seperated string attribute value can only match with @=, see below

stockTab :: Selector
stockTab =  "div" @: [hasClass "inner_box"] // "table" @:[AttributeString "class" @= "table_bg001 border_box limit_sale"] `atDepth` 1

-- tab = "<table class=\"table_bg001 border_box limit_sale\">date </table>" :: String
-- sel = TagString "table"  @:[AttributeString "class" @= "table_bg001 border_box limit_sale"] :: Selector
-- scrapeStringLike tab (text sel)
-- Just "date"
-- use // and atDepth to go inside DOM node as selector, "div" // "a" atDepth 1


data1 :: Selector
data1 = stockTab // "tr" @: [hasClass ""] `atDepth` 1 // "td"
data2 :: Selector
data2 = stockTab // "tr" @: [hasClass "dbrow" ] `atDepth` 1 // "td"

--  ^ means match from start, $ means match end, ^$ means mathc null
-- honestly, regexLike relevant doc is pile of shit
data1OrData2 = makeRegex ("^$|dbrow" :: String) :: Regex -- must specify type notation "String" or else complier will complain
data12 :: Selector
data12 = stockTab // "tr" @: [AttributeString "class" @=~ data1OrData2] `atDepth` 1 // "td"



stock163URL = "http://quotes.money.163.com/trade/lsjysj_600000.html?year=2020&season=1" :: String

allStockData :: IO (Maybe [Stock])
allStockData = scrapeURL stock163URL stockScraper
  where
    stockScraper :: Scraper L8.ByteString [Stock]
    stockScraper = chroots (stockTab // "tr" @: [AttributeString "class" @=~ data1OrData2] `atDepth` 1)  $ oneDayScraper
    -- int save much more database space than float
    floorFloatToInt = (floor :: Float -> Int) . (1000 *) 
    dateToNum :: String -> String
    dateToNum = unpack . mconcat . splitOn (pack "-") . pack  -- 2020-01-01 to 20200101
    removeComma :: String -> String -- in case we get 120,500.56 such great number
    removeComma = unpack . mconcat . splitOn (pack ",") . pack 
    oneDayScraper :: Scraper L8.ByteString Stock
    oneDayScraper = inSerial $ do
      --date <-  (pack . L8.unpack) <$> (seekNext $ text "td")
      date <-  (read :: String -> Int) . dateToNum . L8.unpack  <$> (seekNext $ text "td")
      open <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td") 
      high <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
      low <-  floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
      close <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
      _ <- seekNext $ text "td"
      _ <- seekNext $ text "td"
      shares <- (floor :: Float -> Int) . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td") -- in 100, one hand stock
      value <- (floor :: Float -> Int) . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td") -- in 10 thousnad RMB Yuan
      return $ defaultStock {
        _date = date,
        _shares = shares,
        _open = open,
        _high = high,
        _close = close,
        _low = low,
        _value = value
                            }

printAllStock :: IO () -- (putStr .show )
--printAllStock =  allStockData >>= traverse print . fromJust >> putStr "\nover\n"
printAllStock =  allStockData >>= traverse (Prelude.putStr .show ) . fromJust >> Prelude.putStr "\nover\n"

-- some record syntax test
data T2 = T2
  {
    _num2 :: Int
  }

data T1 = T1
  {
    _num :: Int,
    _date :: Int,
    _name :: String,
    _t2 :: T2
  }

t2 = T2 2
t1 = T1 1 2 "hi" t2
t3 = t1 {_t2 = t2 {_num2 = _num2 t2 + 1 }}
num2 = _num2 . _t2 $ t3
-- 3

get163 :: IO ()
get163 = do
  systemManager <- newManager tlsManagerSettings
  request163NoHead <- parseRequest stock163URL
  response163NoHead <- httpLbs request163NoHead systemManager
  putStrLn $ "The Bing status code was: " ++ (show $ statusCode $ responseStatus response163NoHead)
  -- print is depended on show methord , The show function on String has a limited output character set, it dose not show utf8 chinese correcttly, use purStr or putStrLn
  -- only L8.putStrLn show utf8 Chinese correcttly,how to do when using Text?!
  L8.putStrLn.fromJust $ scrapeStringLike (responseBody response163NoHead) ( text stockName)
  putStrLn . L8.unpack.fromJust $ scrapeStringLike (responseBody response163NoHead) ( text stockName)
  T.putStrLn . T.pack.L8.unpack.fromJust $ scrapeStringLike (responseBody response163NoHead) ( text stockName)
  print $ scrapeStringLike (responseBody response163NoHead) ( attr "class"  stockTab)
  traverse L8.putStrLn . fromJust $ scrapeStringLike (responseBody response163NoHead) (texts data12)
  putStrLn "\nover"
  traverse L8.putStrLn . fromJust $ scrapeStringLike (responseBody response163NoHead) (texts data1 <|> texts data2)
  -- why <|> get no effect, just scrape data2 ?
  return ()

-- instal text-icu to support GBK of html
-- sudo dpkg -i libicu-dev_60.2-6ubuntu1_amd64.deb 
-- sudo apt-get install libicu-le-hb-dev
-- sudo apt-get install libicu-dev
-- sudo apt-get install icu-devtools
-- sudo apt --fix-broken install
-- proxychains sudo apt --fix-broken install
-- sudo apt --fix-broken install
-- proxychains stack install text-icu

-- just set in Emacs,or else GBK not show normal even if you call ICU.toUnicode  
-- (set-language-environment 'utf-8)
-- (set-default-coding-systems 'utf-8)
-- (set-terminal-coding-system 'utf-8)
sinaGBKURL = "http://money.finance.sina.com.cn/corp/go.php/vISSUE_ShareBonus/stockid/002166.phtml" :: String
getSinaGBK :: IO ()
getSinaGBK = do
  systemManager <- newManager tlsManagerSettings
  requestGBKNoHead <- parseRequest sinaGBKURL
  responseGBKNoHead <- httpLbs requestGBKNoHead systemManager
  gbk <- ICU.open "gbk" Nothing
  let txt :: T.Text
      txt = ICU.toUnicode gbk $ L8.toStrict $ responseBody responseGBKNoHead
  T.putStrLn txt


-- The Bing status code was: 200
-- 浦发银行(600000) 历史交易数据
-- Just "table_bg001 border_box limit_sale"
-- 2020-03-1210.7510.7510.6110.64-0.13-1.21326,32434,8161.300.122020-03-1010.7110.9610.7010.870.090.83447,61748,5902.410.162020-03-0611.2311.2811.1111.12-0.20-1.77415,87846,4401.500.152020-03-0411.0111.1010.9511.03-0.03-0.27346,28438,1011.360.122020-03-0210.9511.1010.9211.040.191.75392,45143,2701.660.142020-02-2711.2011.3011.1311.210.010.09330,29837,0551.520.122020-02-2511.0711.1511.0111.09-0.07-0.63483,24653,5241.250.172020-02-2111.2311.3911.2111.300.070.62389,49443,9711.600.142020-02-1911.0811.2411.0411.110.050.45251,00227,9381.810.092020-02-1710.8811.2110.8611.180.322.95413,43345,7643.220.152020-02-1310.8210.8810.7610.76-0.10-0.92226,30624,4791.110.082020-02-1110.8210.9510.7810.860.090.84348,56337,8581.580.122020-02-0710.7310.9110.6710.860.070.65318,97034,2582.220.112020-02-0510.6910.7810.5810.700.040.38450,46748,1641.880.162020-02-0310.2210.6910.2210.47-0.88-7.75953,67499,7694.140.342020-01-2211.7011.8411.6511.77-0.31-2.57781,33691,7201.570.282020-01-2012.2612.2712.1912.250.020.16237,34629,0300.650.082020-01-1612.2812.3112.1612.20-0.05-0.41224,54927,4131.220.082020-01-1412.4012.6912.3912.430.020.16299,94637,4972.420.112020-01-1012.3712.4212.3112.390.020.16183,21322,6580.890.072020-01-0812.4112.4512.2512.32-0.18-1.44352,40543,4981.600.132020-01-0612.5212.6512.4212.46-0.14-1.11410,01251,4431.830.152020-01-0212.4712.6412.4512.470.100.81516,29164,7451.540.18
-- over
  
main :: IO ()
main = printAllStock
