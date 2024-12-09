{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module AppDSL where

import Control.Monad.Free (Free(..), liftF)
import Lib2 (Query(..), Product)
import Data.Maybe (fromMaybe)

-- The Functor representing a single domain command step.
-- 'next' represents what to do after this step completes.
data AppF next
    = DoAddCommand [Product] (Maybe String -> next) 
    | DoBuyCommand Integer (Either Product Int) (Maybe String -> next)
    | DoGiveDiscountCommand (Either Product Int) Integer (Maybe String -> next)
    | DoCheckShippingCommand (Either Product Int) (Maybe String -> next)
    | DoCompareCommand (Either Product Int) (Either Product Int) (Maybe String -> next)
    | DoTotalCommand (Either Product Int) (Maybe String -> next)
    | DoBlackFriday (Maybe String -> next)
    | DoView (Maybe String -> next)
    deriving Functor

-- The Free monad over AppF.
type App = Free AppF

-- Smart constructors for each command. 
-- These correspond directly to the queries in Lib2.

addProducts :: [Product] -> App (Maybe String)
addProducts ps = liftF (DoAddCommand ps id)

buyProduct :: Integer -> Either Product Int -> App (Maybe String)
buyProduct qty target = liftF (DoBuyCommand qty target id)

giveDiscount :: Either Product Int -> Integer -> App (Maybe String)
giveDiscount p d = liftF (DoGiveDiscountCommand p d id)

checkShipping :: Either Product Int -> App (Maybe String)
checkShipping p = liftF (DoCheckShippingCommand p id)

compareProducts :: Either Product Int -> Either Product Int -> App (Maybe String)
compareProducts p1 p2 = liftF (DoCompareCommand p1 p2 id)

getTotal :: Either Product Int -> App (Maybe String)
getTotal p = liftF (DoTotalCommand p id)

blackFriday :: App (Maybe String)
blackFriday = liftF (DoBlackFriday id)

viewState :: App (Maybe String)
viewState = liftF (DoView id)

---------------------------------------
-- Example DSL Program

-- This might represent a simple DSL program:
-- 1. Add some products
-- 2. View the state
-- 3. Buy a specific product
-- 4. Check shipping for that product

-- exampleProgram :: App (Maybe String)
-- exampleProgram = do
--     _ <- addProducts [ProductExample]          -- Replace ProductExample with a real product from your code
--     _ <- viewState
--     _ <- buyProduct 2 (Right 1) -- buy product at index 1
--     checkShipping (Right 1)


-- Of course, you'll have to integrate with your actual product types from Lib2.
-- The DSL corresponds to queries and not necessarily to the exact input format.
-- Instead, the interpreters will turn these DSL steps into queries, and then queries into HTTP requests or in-memory modifications.

-- Later, you will write interpreters:
-- 1) One that executes each DSL command as an HTTP request to your server.
-- 2) One that batches multiple DSL commands before sending a request.
-- 3) One that runs them all in memory for testing (no HTTP calls).
