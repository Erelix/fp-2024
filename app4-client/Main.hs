module Main (main) where

import AppDSL
import Interpreters (runHttpPerCommand) 
import Lib2 (Product(..))

main :: IO ()
main = do
    let program = do
          _ <- addProducts [AddOn "playerBoard" 5]
          _ <- viewState
          _ <- buyProduct 2 (Left (AddOn "playerBoard" 5))
          _ <- addProducts [BoardGame "venusTMexp" 200.0 []]
          _ <- viewState
          _ <- giveDiscount (Right 2) 20
          return "Finished running DSL program"
    
    result <- runHttpPerCommand program
    print result
