{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Char8 as B
import Data.ByteString.Lazy as Lazy
import Data.ByteString.Builder as Builder
import Snap.Core
import Snap.Http.Server
import System.IO
import System.IO.Streams as Streams
import UserLogin
import UserRegistration

data Files = Files {
    mainHtml :: B.ByteString,
    mainCss :: B.ByteString,
    mainJs :: B.ByteString,
    axlsignJs :: B.ByteString
    }

main :: IO ()
main = do
    files <- readStaticFiles
    quickHttpServe $ site files

minify :: B.ByteString -> B.ByteString
minify = B.pack . Prelude.unwords . Prelude.words . B.unpack

convertLazy :: Lazy.ByteString -> B.ByteString
convertLazy = B.concat . Lazy.toChunks

readStaticFile :: String -> IO B.ByteString
readStaticFile filename = do
    contents <- 
        withFile filename ReadMode $ do
            \fileHandle ->
                B.hGetContents fileHandle
    return $ minify contents

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
    route [ ("users/create",
                method POST $ (createUser =<< readRequestBody 16384))
          , ("users/login", method POST $ loginUser)
          , ("static/main.css", serveStatic "text/css" $ mainCss files)
          , ("static/main.js",
                serveStatic "application/javascript" $ mainJs files)
          , ("static/axlsign.js",
                serveStatic "application/javascript" $ axlsignJs files) ]

topLevel :: B.ByteString -> Snap ()
topLevel file =
    serveStatic "text/html" file

createUser :: Lazy.ByteString -> Snap ()
createUser requestBody = do
    let userRegistration =
            (decode requestBody) :: Maybe UserRegistration
    case userRegistration of
        Just reg -> do
            jsonResponse $ encode reg
        Nothing -> do
            basicResponse 400 "Bad Request."

loginUser :: Snap ()
loginUser = writeBS "Login User"

serveStatic :: B.ByteString -> B.ByteString -> Snap ()
serveStatic contentType content =
    putResponse $
        setContentLength (fromIntegral $ B.length content) $
            setContentType contentType $
                flip setResponseBody emptyResponse $
                    \out -> do
                        Streams.write (Just $ Builder.byteString content) out
                        return out

basicResponse :: Int -> B.ByteString -> Snap ()
basicResponse code contentString =
    putResponse $
        setContentType "text/plain" $
            setResponseCode code $
                flip setResponseBody emptyResponse $
                    \out -> do
                        Streams.write
                            (Just $ Builder.byteString contentString)
                            out
                        return out

jsonResponse :: Lazy.ByteString -> Snap ()
jsonResponse jsonString =
    putResponse $
        setContentType "application/json" $
            flip setResponseBody emptyResponse $
                \out -> do
                    Streams.write
                        (Just $ Builder.lazyByteString jsonString)
                        out
                    return out
