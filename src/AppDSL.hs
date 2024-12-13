{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module AppDSL where

import Control.Monad.Free (Free(..), liftF)
import Lib2 (Query(..), Product)
import Data.Maybe (fromMaybe)

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

type App = Free AppF


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
