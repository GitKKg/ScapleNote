-- | 
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Scalpel.Example where
import Text.HTML.Scalpel
import Control.Applicative

import Network.HTTP.Client

import qualified Data.ByteString.Lazy.Char8 as L8

import           Network.HTTP.Simple

import Text.HTML.TagSoup

-- Scraper is just like a converting format  from some input to output
-- Selector is just like a selecting format to select DOM node
-- chroot accept an old Scraper and output a new Scraper,because it add a range by Selector for old Scraper


type Author = String
data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

allComments :: IO (Maybe [Comment])
allComments = scrapeURL "file:///home/kyle/Haskell/Scalpel/article.html" comments
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

main2 :: IO ()
main2 = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
