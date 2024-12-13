{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import Data.String.Conversions (cs)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Lib2 (State, emptyState, stateTransition, parseQuery, Query(..))
import Lib3 (parseStatements, Statements(..), applyStatements)

main :: IO ()
main = do
    stateVar <- newTVarIO emptyState
    scotty 8080 $ do
        post "/command" $ do
            bodyBytes <- body
            let input = cs bodyBytes
            liftIO $ putStrLn $ "Received: " ++ show input

            response <- liftIO $ atomically $ do
                st <- readTVar stateVar
                let parsed = parseStatements input
                case parsed of
                    Right (stmts, _) -> 
                        case applyStatements st stmts of
                            Right newSt -> do
                                writeTVar stateVar newSt
                                return $ Just "Batch processed"
                            Left err -> return $ Just err
                    Left _ -> 
                        case parseQuery input of
                            Right (q, _) -> 
                                case stateTransition st q of
                                    Right (msg, newSt) -> do
                                        writeTVar stateVar newSt
                                        return msg
                                    Left err -> return $ Just err
                            Left err -> return $ Just err

            text $ cs $ maybe "" id response
