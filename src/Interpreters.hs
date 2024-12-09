{-# LANGUAGE OverloadedStrings #-}

module Interpreters (runHttpPerCommand) where

import AppDSL
import Lib2 (Product(..), Query(..))
import Control.Monad.Free (Free(..))
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Data.Maybe (fromMaybe)

-- Convert a single DSL step (AppF) into a textual command representing a Lib2 Query
toQueryString :: AppF a -> String
toQueryString (DoAddCommand products _) =
    "add " ++ renderProducts products
  where
    renderProducts = unwords . map show

toQueryString (DoBuyCommand qty p _) =
    let target = either (("Product(" ++) . (++ ")") . show) (("Index(" ++) . (++ ")") . show) p
    in "buy " ++ show qty ++ " " ++ target

toQueryString (DoGiveDiscountCommand p d _) =
    let target = either (("Product(" ++) . (++ ")") . show) (("Index(" ++) . (++ ")") . show) p
    in "giveDiscount " ++ target ++ " " ++ show d ++ "%"

toQueryString (DoCheckShippingCommand p _) =
    let target = either (("Product(" ++) . (++ ")") . show) (("Index(" ++) . (++ ")") . show) p
    in "checkShipping " ++ target

toQueryString (DoCompareCommand p1 p2 _) =
    let t1 = either (("Product(" ++) . (++ ")") . show) (("Index(" ++) . (++ ")") . show) p1
        t2 = either (("Product(" ++) . (++ ")") . show) (("Index(" ++) . (++ ")") . show) p2
    in "compare " ++ t1 ++ " " ++ t2

toQueryString (DoTotalCommand p _) =
    let target = either (("Product(" ++) . (++ ")") . show) (("Index(" ++) . (++ ")") . show) p
    in "total " ++ target

toQueryString (DoBlackFriday _) =
    "blackFriday"

toQueryString (DoView _) =
    "view"

-- Sends a single command to the server and returns the response as Maybe String
sendCommand :: String -> IO (Maybe String)
sendCommand cmd = do
    initReq <- parseRequest "http://localhost:8080/command"
    let req = setRequestBodyLBS (BL.fromStrict (BS.pack cmd)) (setRequestMethod "POST" initReq)
    resp <- httpBS req
    let body = getResponseBody resp
    return $ if BS.null body then Nothing else Just (BS.unpack body)


-- Interpreter that runs one command at a time, making an HTTP request for each
runHttpPerCommand :: App a -> IO a
runHttpPerCommand (Pure a) = return a
runHttpPerCommand (Free step) = do
    let cmd = toQueryString step
    mResp <- sendCommand cmd
    case step of
      DoAddCommand _ next -> runHttpPerCommand (next mResp)
      DoBuyCommand _ _ next -> runHttpPerCommand (next mResp)
      DoGiveDiscountCommand _ _ next -> runHttpPerCommand (next mResp)
      DoCheckShippingCommand _ next -> runHttpPerCommand (next mResp)
      DoCompareCommand _ _ next -> runHttpPerCommand (next mResp)
      DoTotalCommand _ next -> runHttpPerCommand (next mResp)
      DoBlackFriday next -> runHttpPerCommand (next mResp)
      DoView next -> runHttpPerCommand (next mResp)
