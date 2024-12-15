{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import Data.String.Conversions (cs)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan)
import qualified Lib2 as L2
import qualified Lib3 as L3
import Control.Monad (void)

main :: IO ()
main = do
    stateVar <- newTVarIO L2.emptyState
    ioChan <- newChan
    _ <- forkIO (L3.storageOpLoop ioChan)
    scotty 8080 $ do
        post "/command" $ do
            bodyBytes <- body
            let input = cs bodyBytes
            liftIO $ putStrLn ("Received: " ++ show input)

            parseResult <- liftIO $ atomically $ do
                st <- readTVar stateVar
                case L3.parseCommand input of
                    Right (cmd, _) ->
                        return (Left cmd)
                    Left _ ->
                        case L3.parseStatements input of
                            Right (stmts, _) ->
                                case L3.applyStatements st stmts of
                                    Right newSt -> do
                                        writeTVar stateVar newSt
                                        return (Right (Just "Batch processed"))
                                    Left err -> return (Right (Just err))
                            Left _ ->
                                case L2.parseQuery input of
                                    Right (q, _) ->
                                        case L2.stateTransition st q of
                                            Right (msg, newSt) -> do
                                                writeTVar stateVar newSt
                                                return (Right msg)
                                            Left err -> return (Right (Just err))
                                    Left err -> return (Right (Just err))

            finalResp <- case parseResult of
                Left cmd -> liftIO $ do
                    res <- L3.stateTransition stateVar cmd ioChan
                    return $ either Just id res
                Right msg -> return msg

            text $ cs $ maybe "" id finalResp
