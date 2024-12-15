{-# LANGUAGE OverloadedStrings #-}
module Interpreters (runHttpPerCommand) where

import AppDSL
import Lib2 (Product(..))
import Control.Monad.Free (Free(..))
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Data.List (intercalate)

renderProductGrammar :: Product -> String
renderProductGrammar (BoardGame name price components) =
    name ++ " " ++ show price ++ "eur (contains: " ++ renderComponentsGrammar components ++ ")"
renderProductGrammar (AddOn name price) =
    name ++ " " ++ show price ++ "eur"
renderProductGrammar (Component qty cname) =
    show qty ++ " " ++ cname
renderProductGrammar (BoardGameWithAddOns name price comps addons) =
    let base = renderProductGrammar (BoardGame name price comps)
        addOnsStr = renderProductsGrammar addons
    in base ++ " [includes: " ++ addOnsStr ++ "]"

renderProductsGrammar :: [Product] -> String
renderProductsGrammar [] = ""
renderProductsGrammar [p] = renderProductGrammar p
renderProductsGrammar (p:ps) = renderProductGrammar p ++ ", " ++ renderProductsGrammar ps

renderComponentsGrammar :: [Product] -> String
renderComponentsGrammar [] = "" 
renderComponentsGrammar [c] = renderProductGrammar c
renderComponentsGrammar (c:cs) = renderProductGrammar c ++ ", " ++ renderComponentsGrammar cs

renderProdOrIndexGrammar :: Either Product Int -> String
renderProdOrIndexGrammar (Left prod) = renderProductGrammar prod
renderProdOrIndexGrammar (Right i) = show i

toQueryString :: AppF a -> String
toQueryString (DoAddCommand products _) =
    "add " ++ renderProductsGrammar products
toQueryString (DoBuyCommand qty p _) =
    "buy " ++ show qty ++ " " ++ renderProdOrIndexGrammar p
toQueryString (DoGiveDiscountCommand p d _) =
    "giveDiscount " ++ renderProdOrIndexGrammar p ++ " " ++ show d ++ "%"
toQueryString (DoCheckShippingCommand p _) =
    "checkShipping " ++ renderProdOrIndexGrammar p
toQueryString (DoCompareCommand p1 p2 _) =
    "compare " ++ renderProdOrIndexGrammar p1 ++ " " ++ renderProdOrIndexGrammar p2
toQueryString (DoTotalCommand p _) =
    "total " ++ renderProdOrIndexGrammar p
toQueryString (DoBlackFriday _) =
    "blackFriday"
toQueryString (DoView _) =
    "view"
toQueryString (DoSave _) =
    "save"
toQueryString (DoLoad _) =
    "load"

sendCommand :: String -> IO (Maybe String)
sendCommand cmd = do
    initReq <- parseRequest "http://localhost:8080/command"
    let req = setRequestBodyLBS (BL.fromStrict (BS.pack cmd)) (setRequestMethod "POST" initReq)
    resp <- httpBS req
    let body = getResponseBody resp
    return $ if BS.null body then Nothing else Just (BS.unpack body)

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
      DoSave next -> runHttpPerCommand (next mResp)
      DoLoad next -> runHttpPerCommand (next mResp)
