{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import AppDSL
import Interpreters (runHttpPerCommand)
import Lib2 (Product(..))

-- simple tests
-- main :: IO ()
-- main = do
--     let program = do
--           respAdd <- addProducts [AddOn "playerBoard" 5.0]
--           _ <- viewState

--           respBuy <- buyProduct 2 (Left (AddOn "playerBoard" 5.0))
--           _ <- viewState

--           respSave <- saveState

--           respAdd2 <- addProducts [BoardGame "venusTMexp" 200.0 []]
--           _ <- viewState

--           respLoad <- loadState
--           respViewAfterLoad <- viewState

--           return $
--             "Responses: " ++
--             "addProducts: " ++ show respAdd ++ " | " ++
--             "buyProduct: " ++ show respBuy ++ " | " ++
--             "saveState: " ++ show respSave ++ " | " ++
--             "loadState: " ++ show respLoad ++ " | " ++
--             "viewAfterLoad: " ++ show respViewAfterLoad ++ " | "

--     result <- runHttpPerCommand program
--     putStrLn ("Final result: " ++ result)


-- complex tests
main :: IO ()
main = do
    let program = do
          respAdd1 <- addProducts [AddOn "playerBoard" 5.0]
          _ <- viewState

          respBuy1 <- buyProduct 2 (Left (AddOn "playerBoard" 5.0))
          _ <- viewState

          respSave1 <- saveState

          let complexBoardGame =
                BoardGameWithAddOns "bigBoxTM" 300.0
                  [ Component 10 "tile"
                  , Component 2 "gameBoard"
                  , Component 4 "card"
                  ]
                  [ AddOn "miniature" 15.0
                  , AddOn "playerBoard" 5.0
                  ]

          let anotherBoardGame =
                BoardGame "preludeTMexp" 60.0
                  [ Component 3 "marker"
                  , AddOn "metalResource" 20.0
                  ]

          respAddComplex <- addProducts [complexBoardGame, anotherBoardGame]
          _ <- viewState

          respDiscountComplex <- giveDiscount (Left complexBoardGame) 30
          _ <- viewState

          respCheckShippingComplex <- checkShipping (Left complexBoardGame)

          respCompareComplex <- compareProducts (Left complexBoardGame) (Left (AddOn "playerBoard" 5.0))

          respTotalComplex <- getTotal (Left complexBoardGame)

          respBlackFriday <- blackFriday
          _ <- viewState

          respBuyComplex <- buyProduct 1 (Left complexBoardGame)
          _ <- viewState

          respSave2 <- saveState

          respLoad1 <- loadState
          respViewAfterLoad1 <- viewState

          return $
            "Responses:\n" ++
            "addProducts(simple): " ++ show respAdd1 ++ "\n" ++
            "buyProduct(playerBoard): " ++ show respBuy1 ++ "\n" ++
            "saveState #1: " ++ show respSave1 ++ "\n" ++
            "addProducts(complex): " ++ show respAddComplex ++ "\n" ++
            "giveDiscount(complex): " ++ show respDiscountComplex ++ "\n" ++
            "checkShipping(complex): " ++ show respCheckShippingComplex ++ "\n" ++
            "compare(complex vs playerBoard): " ++ show respCompareComplex ++ "\n" ++
            "total(complex): " ++ show respTotalComplex ++ "\n" ++
            "blackFriday: " ++ show respBlackFriday ++ "\n" ++
            "buyComplex(half price): " ++ show respBuyComplex ++ "\n" ++
            "saveState #2: " ++ show respSave2 ++ "\n" ++
            "loadState #1: " ++ show respLoad1 ++ "\n" ++
            "viewAfterLoad #1: " ++ show respViewAfterLoad1 ++ "\n"

    result <- runHttpPerCommand program
    putStrLn ("Final result: " ++ result)
