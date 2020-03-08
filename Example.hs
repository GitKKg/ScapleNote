-- | 
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Scalpel.Example where
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

type Author = String
data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

allComments :: IO (Maybe [Comment])
allComments = scrapeURL "file:///home/kyle/Haskell/ScalpelNote/article.html" comments
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
hostAddr = "192.168.1.2" :: HostName -- import Network.Socket
-- using 192.168.1.2 will Exception as ConnectionTimeout
portNumber = 5678 :: PortNumber
proxySetting = SockSettingsSimple hostAddr portNumber

-- import Network.HTTP.Client.TLS
--managerSetting = mkManagerSettings tlsSetting (Just proxySetting)
-- newManager :: ManagerSettings -> IO Manager
-- v2Manger = newManager managerSetting
--setGlobalManager :: Manager -> IO ()

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
  requestBing <- parseRequest "https://www.bing.com"
  responseBing <- httpLbs requestBing systemManager
  putStrLn $ "The Bing status code was: " ++ (show $ statusCode $ responseStatus responseBing)
  print $ scrapeStringLike (responseBody responseBing) (attr "lang" "html")
  print "ok"
  requestGoogle <- parseRequest "https://www.google.com/"
  catch (getGoogle requestGoogle v2manager) handleE
    where getGoogle requestGoogle v2manager = do
            responseGoogle <- httpLbs requestGoogle v2manager
            putStrLn $ "The Google status code was: " ++ (show $ statusCode $ responseStatus responseGoogle)
            print $ scrapeStringLike (responseBody responseGoogle) (attr "itemtype" "html")
          handleE (SomeException e) = do
            putStrLn $ "\n Caught exception of type "  ++ show (typeOf e) ++ "\n"
            putStrLn $ show e

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
