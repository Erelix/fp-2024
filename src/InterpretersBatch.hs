{-# LANGUAGE OverloadedStrings #-}
module InterpretersBatch (runHttpBatch) where

import AppDSL
import Lib2 (Product(..), Query(..))
import Control.Monad.Free (Free(..))
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

toQueryString :: AppF a -> String
toQueryString (DoAddCommand products _) =
    "add " ++ unwords (map show products)
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
toQueryString (DoBlackFriday _) = "blackFriday"
toQueryString (DoView _) = "view"
toQueryString (DoSave _) = "save"
toQueryString (DoLoad _) = "load"

runHttpBatch :: App a -> IO a
runHttpBatch app = do
    (res, cmds) <- collectCommands app []
    let batchBody = "BEGIN\n" ++ unlines (map (++ ";") (init cmds)) ++ last cmds ++ "\nEND"
    _ <- sendBatch batchBody
    return res
  where
    collectCommands :: App a -> [String] -> IO (a,[String])
    collectCommands (Pure a) acc = return (a, acc)
    collectCommands (Free step) acc = do
      let cmd = toQueryString step
      case step of
        DoAddCommand _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoBuyCommand _ _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoGiveDiscountCommand _ _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoCheckShippingCommand _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoCompareCommand _ _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoTotalCommand _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoBlackFriday next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoView next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoSave next -> collectCommands (next Nothing) (acc ++ ["save"])
        DoLoad next -> collectCommands (next Nothing) (acc ++ ["load"])

sendBatch :: String -> IO (Maybe String)
sendBatch batch = do
    initReq <- parseRequest "http://localhost:8080/command"
    let req = setRequestBodyLBS (BL.fromStrict (BS.pack batch)) (setRequestMethod "POST" initReq)
    resp <- httpBS req
    let body = getResponseBody resp
    return $ if BS.null body then Nothing else Just (BS.unpack body)
