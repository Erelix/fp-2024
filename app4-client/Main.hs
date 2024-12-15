{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import AppDSL
import Interpreters (runHttpPerCommand)
import Lib2 (Product(..))

main :: IO ()
main = do
    let program = do
          respAdd <- addProducts [AddOn "playerBoard" 5.0]
          _ <- viewState

          respBuy <- buyProduct 2 (Left (AddOn "playerBoard" 5.0))
          _ <- viewState

          respSave <- saveState

          respAdd2 <- addProducts [BoardGame "venusTMexp" 200.0 []]
          _ <- viewState

          respLoad <- loadState
          respViewAfterLoad <- viewState

          return $
            "Responses: " ++
            "addProducts: " ++ show respAdd ++ " | " ++
            "buyProduct: " ++ show respBuy ++ " | " ++
            "saveState: " ++ show respSave ++ " | " ++
            "loadState: " ++ show respLoad ++ " | " ++
            "viewAfterLoad: " ++ show respViewAfterLoad ++ " | "

    result <- runHttpPerCommand program
    putStrLn ("Final result: " ++ result)
