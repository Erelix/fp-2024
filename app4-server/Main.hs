{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    post "/" $ do
        b <- body
        liftIO $ putStrLn $ "Request received: " ++ cs b
        let response = processRequest (cs b)
        text $ cs response

-- Process the incoming DSL-like request
processRequest :: String -> String
processRequest "works?" = "Server acknowledges: works!"
processRequest cmd = "Server executed: " ++ cmd
