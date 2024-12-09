{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Char8 (pack)
import Network.Wreq
import Data.String.Conversions (cs)
import Control.Lens

main :: IO ()
main = do
    let commands = ["works?", "add TestGame", "buy 2 TestGame"]
    mapM_ sendCommand commands

sendCommand :: String -> IO ()
sendCommand cmd = do
    let rawRequest = pack cmd
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ "Client sent: " ++ cmd
    putStrLn $ "Server responded: " ++ cs (resp ^. responseBody)
