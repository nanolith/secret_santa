{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.ByteString as B
import Data.ByteString.Builder as Builder
import Snap.Core
import Snap.Http.Server
import System.IO
import System.IO.Streams as Streams

data Files = Files {
    mainHtml :: ByteString,
    mainCss :: ByteString,
    mainJs :: ByteString,
    axlsignJs :: ByteString
    }

main :: IO ()
main = do
    files <- readStaticFiles
    quickHttpServe $ site files

readStaticFile :: String -> IO ByteString
readStaticFile filename = do
    withFile filename ReadMode $ do
        \fileHandle ->
            B.hGetContents fileHandle

readStaticFiles :: IO Files
readStaticFiles = do
    mainHtmlFile <- readStaticFile "static/main.html"
    mainCssFile <- readStaticFile "static/main.css"
    mainJsFile <- readStaticFile "static/main.js"
    axlsignJsFile <- readStaticFile "static/axlsign.js"
    return $
        Files {
            mainHtml = mainHtmlFile,
            mainCss = mainCssFile,
            mainJs = mainJsFile,
            axlsignJs = axlsignJsFile }

site :: Files -> Snap ()
site files =
    ifTop (topLevel $ mainHtml files) <|>
    route [ ("user/create", createUser)
          , ("user/login", loginUser)
          , ("static/main.css", serveStatic "text/css" $ mainCss files)
          , ("static/main.js",
                serveStatic "application/javascript" $ mainJs files)
          , ("static/axlsign.js",
                serveStatic "application/javascript" $ axlsignJs files) ]

topLevel :: ByteString -> Snap ()
topLevel file =
    serveStatic "text/html" file

createUser :: Snap ()
createUser = writeBS "Create User"

loginUser :: Snap ()
loginUser = writeBS "Login User"

serveStatic :: ByteString -> ByteString -> Snap()
serveStatic contentType content =
    putResponse $
        setContentLength (fromIntegral $ B.length content) $
            setContentType contentType $
                flip setResponseBody emptyResponse $
                    \out -> do
                        Streams.write (Just $ Builder.byteString content) out
                        return out
