{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
      StorageOp (..),
      storageOpLoop,
      parseCommand,
      parseStatements,
      marshallState,
      renderStatements
    ) where

import Control.Concurrent.Chan (Chan, readChan, writeChan, newChan)
import Control.Exception (catch, IOException)
import System.IO (writeFile, readFile)

import Control.Concurrent.STM (atomically, readTVar, writeTVar, TVar)
import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Lib2

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save str responseChan -> do
      writeFile "storage.txt" str
      writeChan responseChan ()
    Load responseChan -> do
      contents <- readFile "storage.txt" `catch` handleReadException
      writeChan responseChan contents
  storageOpLoop chan

handleReadException :: IOException -> IO String
handleReadException _ = return ""

data Statements = Batch [Lib2.Query] |
                 Single Lib2.Query
                 deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input =
    let trimmedInput = dropWhile isSpace input
        (cmdWord, rest) = break isSpace trimmedInput
        rest' = dropWhile isSpace rest
    in case cmdWord of
        "load" -> 
            if null rest'
                then Right (LoadCommand, "")
                else Left "Load command does not take any arguments."
        "save" -> 
            if null rest'
                then Right (SaveCommand, "")
                else Left "Save command does not take any arguments."
        _ -> case parseStatements input of
                Right (stmts, rest'') -> 
                    if all isSpace rest''
                        then Right (StatementCommand stmts, "")
                        else Left "Extra input after valid statements."
                Left err -> Left err

-- | Parses Statements.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = 
    case parseQueries input of
        Right (queries, rest) ->
            if null queries
                then Left "No queries found."
                else if length queries == 1
                    then Right (Single (head queries), rest)
                    else Right (Batch queries, rest)
        Left err -> Left err

parseQueries :: String -> Either String ([Lib2.Query], String)
parseQueries input = 
    let parseMultipleQueries s acc =
            case Lib2.parseQuery s of
                Right ((query, rest)) -> 
                    let rest' = dropWhile isSpace rest
                    in parseMultipleQueries rest' (acc ++ [query])
                Left _ -> Right (acc, s)
    in parseMultipleQueries input []

-- | Converts program's state into Statements
marshallState :: Lib2.State -> Statements
marshallState state = 
    let queries = [Lib2.AddCommand (Lib2.products state)]
                ++ map (\(prod, disc) -> Lib2.GiveDiscountCommand (Left prod) disc) (Lib2.discounts state)
                ++ map (\(prodOrIndex, qty) -> Lib2.BuyCommand qty prodOrIndex) (Lib2.purchaseHistory state)
    in Batch queries

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = unlines (map renderQuery queries)

renderQuery :: Lib2.Query -> String
renderQuery query = 
    case query of
        Lib2.AddCommand products -> "add " ++ renderProducts products
        Lib2.GiveDiscountCommand prodOrIndex discount ->
            "giveDiscount " ++ renderProdOrIndex prodOrIndex ++ " " ++ show discount ++ "%"
        Lib2.BuyCommand qty prodOrIndex ->
            "buy " ++ show qty ++ " " ++ renderProdOrIndex prodOrIndex
        Lib2.ViewCommand -> "view"
        Lib2.BlackFridayCommand -> "blackFriday"
        Lib2.TotalCommand prodOrIndex -> "total " ++ renderProdOrIndex prodOrIndex
        Lib2.CheckShippingCommand prodOrIndex -> "checkShipping " ++ renderProdOrIndex prodOrIndex
        Lib2.CompareCommand p1 p2 -> "compare " ++ renderProdOrIndex p1 ++ " " ++ renderProdOrIndex p2

renderProducts :: [Lib2.Product] -> String
renderProducts = intercalate ", " . map renderProduct

renderProduct :: Lib2.Product -> String
renderProduct (Lib2.BoardGame name price components) =
    name ++ " " ++ show price ++ "eur (contains: " ++ renderProducts components ++ ")"
renderProduct (Lib2.AddOn name price) = name ++ " " ++ show price ++ "eur"
renderProduct (Lib2.Component qty name) = show qty ++ " " ++ name
renderProduct (Lib2.BoardGameWithAddOns name price components addons) =
    renderProduct (Lib2.BoardGame name price components) ++ " [includes: " ++ renderProducts addons ++ "]"

renderProdOrIndex :: Either Lib2.Product Int -> String
renderProdOrIndex (Left product) = renderProduct product
renderProdOrIndex (Right index) = show index

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar command ioChan = case command of
    StatementCommand stmts -> case stmts of
        Single query -> do
            state <- atomically $ readTVar stateVar
            case Lib2.stateTransition state query of
                Right (msg, newState) -> do
                    atomically $ writeTVar stateVar newState
                    return $ Right msg
                Left err -> return $ Left err
        Batch queries -> do
            state <- atomically $ readTVar stateVar
            let result = foldM (\s q -> Lib2.stateTransition s q >>= \(_, newState) -> Right newState) state queries
            case result of
                Right newState -> do
                    atomically $ writeTVar stateVar newState
                    return $ Right Nothing
                Left err -> return $ Left err
    SaveCommand -> do
        state <- atomically $ readTVar stateVar
        let stmts = marshallState state
            content = renderStatements stmts
        responseChan <- newChan
        writeChan ioChan (Save content responseChan)
        _ <- readChan responseChan
        return $ Right (Just "State saved successfully.")
    LoadCommand -> do
        responseChan <- newChan
        writeChan ioChan (Load responseChan)
        content <- readChan responseChan
        case parseStatements content of
            Right (stmts, _) -> do
                state <- atomically $ readTVar stateVar
                case applyStatements state stmts of
                    Right newState -> do
                        atomically $ writeTVar stateVar newState
                        return $ Right (Just "State loaded successfully.")
                    Left err -> return $ Left ("Failed to apply loaded state: " ++ err)
            Left err -> return $ Left ("Failed to parse saved state: " ++ err)

-- Helper function for LoadCommand
applyStatements :: Lib2.State -> Statements -> Either String Lib2.State
applyStatements s (Single q) = fmap snd (Lib2.stateTransition s q)
applyStatements s (Batch qs) = foldM (\s' q -> fmap snd (Lib2.stateTransition s' q)) s qs
