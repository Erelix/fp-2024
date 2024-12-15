{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import AppDSL
import Interpreters (runHttpPerCommand)
import Lib2 (Product(..))

main :: IO ()
main = do
    let program = do
          resp1 <- addProducts [AddOn "playerBoard" 5.0]
          return ("Response to addProducts: " ++ show resp1)
          
          resp2 <- viewState
          return ("Response to viewState: " ++ show resp2)
          
          resp3 <- buyProduct 2 (Left (AddOn "playerBoard" 5.0))
          return ("Response to buyProduct: " ++ show resp3)

          resp4 <- addProducts [BoardGame "venusTMexp" 200.0 []]
          return ("Response to addProducts (venusTMexp): " ++ show resp4)

          resp5 <- viewState
          return ("Response to viewState: " ++ show resp5)

          resp6 <- giveDiscount (Right 2) 20
          return ("Response to giveDiscount: " ++ show resp6)

    result <- runHttpPerCommand program
    putStrLn ("Final result: " ++ show result)
