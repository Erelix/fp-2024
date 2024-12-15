{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import Data.String.Conversions (cs)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Lib2 (State, emptyState, stateTransition, parseQuery, Query(..))
import Lib3 (parseStatements, Statements(..), applyStatements)
import Control.Monad (void)

main :: IO ()
main = do
    stateVar <- newTVarIO emptyState
    scotty 8080 $ do
        post "/command" $ do
            bodyBytes <- body
            let input = cs bodyBytes
            liftIO $ putStrLn ("Received: " ++ show input)

            -- Process command inside STM transaction
            result <- liftIO $ atomically $ do
                st <- readTVar stateVar
                case parseStatements input of
                    Right (stmts, _) -> 
                        case applyStatements st stmts of
                            Right newSt -> do
                                writeTVar stateVar newSt
                                return (Right ("Batch processed", newSt))
                            Left err ->
                                return (Left err)

                    Left _ ->
                        case parseQuery input of
                            Right (q, _) ->
                                case stateTransition st q of
                                    Right (msg, newSt) -> do
                                        writeTVar stateVar newSt
                                        return (Right (maybe "(no message)" id msg, newSt))
                                    Left err -> return (Left err)
                            Left err -> return (Left err)

            -- Outside the transaction, log and return response
            case result of
                Right (msg, newSt) -> do
                    liftIO $ putStrLn ("SUCCESS: " ++ msg)
                    liftIO $ putStrLn ("New state: " ++ show newSt)
                    text (cs msg)
                Left err -> do
                    liftIO $ putStrLn ("ERROR: " ++ err)
                    text (cs ("Error: " ++ err))
