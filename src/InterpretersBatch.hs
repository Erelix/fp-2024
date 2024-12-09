{-# LANGUAGE OverloadedStrings #-}

module InterpretersBatch (runHttpBatch) where

import AppDSL
import Lib2 (Product(..))
import Control.Monad.Free (Free(..))
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL


-- Reuse the toQueryString logic from previous interpreter or redefine minimally:
toQueryString :: AppF a -> String
toQueryString = error "Same as in the HTTP per command interpreter, or re-implement similarly."

runHttpBatch :: App a -> IO a
runHttpBatch app = do
    (res, cmds) <- collectCommands app []
    -- cmds now contains all textual commands
    -- send them all at once
    let batchBody = "BEGIN\n" ++ unlines (map (++ ";") (init cmds)) ++ last cmds ++ "\nEND"
    mResp <- sendBatch batchBody
    -- The last command's next function receives this response, but we actually don't have a "next"
    -- after Pure. So if you need a final response, you'd handle it differently. For now, we ignore.
    return res

  where
    collectCommands :: App a -> [String] -> IO (a,[String])
    collectCommands (Pure a) acc = return (a, acc)
    collectCommands (Free step) acc = do
      let cmd = toQueryString step
      -- Instead of calling next with response, we can't finalize intermediate steps here.
      -- The "smart" approach means we won't know intermediate responses until server runs them.
      -- So intermediate steps that rely on response must be considered:
      -- For simplicity, let's say responses are deferred and we treat them as Nothing.
      -- Another approach is to store placeholders or split your DSL.
      -- We'll assume responses are not critical per step. Or we can store them and apply after the batch returns.
      
      case step of
        DoAddCommand _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoBuyCommand _ _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoGiveDiscountCommand _ _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoCheckShippingCommand _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoCompareCommand _ _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoTotalCommand _ next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoBlackFriday next -> collectCommands (next Nothing) (acc ++ [cmd])
        DoView next -> collectCommands (next Nothing) (acc ++ [cmd])

sendBatch :: String -> IO (Maybe String)
sendBatch batch = do
    initReq <- parseRequest "http://localhost:8080/command"
    let req = setRequestBodyLBS (BL.fromStrict (BS.pack batch)) (setRequestMethod "POST" initReq)
    resp <- httpBS req
    let body = getResponseBody resp
    return $ if BS.null body then Nothing else Just (BS.unpack body)
