{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.List (isInfixOf)
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure, assertBool )
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Monadic (monadicIO, assert, pre)
import Test.QuickCheck
import Lib2 qualified
import Lib3 qualified
import GHC.Generics (Generic)
import InterpretersInMemory (runInMemory)
import AppDSL


arbitraryPositiveDouble :: Gen Double
arbitraryPositiveDouble = do
    n <- choose (1, 100000) :: Gen Integer
    return (fromIntegral n / 100.0)

arbitraryPositiveInteger :: Gen Integer
arbitraryPositiveInteger = fmap fromIntegral (choose (1, 100) :: Gen Int)

arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = choose (1, 100)

arbitraryProdOrIndex :: Gen (Either Lib2.Product Int)
arbitraryProdOrIndex = oneof 
    [ Left <$> resize 3 arbitrary,
      Right <$> arbitraryPositiveInt
    ]

addOnNames :: [String]
addOnNames = ["playerBoard", "miniature", "metalResource", "cardSleeve", "spaceInsert"]

boardGameNames :: [String]
boardGameNames = ["corporateCEOTM", "baseTM", "bigBoxTM", "venusTMexp", "turmoilTMexp"]

componentNames :: [String]
componentNames = ["tile", "card", "marker", "gameBoard", "playerBoard"]

instance Arbitrary Lib2.Product where
    arbitrary :: Gen Lib2.Product
    arbitrary = oneof [arbitraryBoardGame, arbitraryAddOn, arbitraryComponent]
      where
        arbitraryBoardGame = do
            name <- elements boardGameNames
            price <- arbitraryPositiveDouble
            components <- resize 10 (listOf arbitraryComponent)
            return $ Lib2.BoardGame name price components
        arbitraryAddOn = do
            name  <- elements addOnNames
            price <- arbitraryPositiveDouble
            return $ Lib2.AddOn name price
        arbitraryComponent = do
            qty <- arbitraryPositiveInteger
            name <- elements componentNames
            return $ Lib2.Component qty name

instance Arbitrary Lib2.Query where
    arbitrary :: Gen Lib2.Query
    arbitrary = oneof
        [ arbitraryAddCommand
        , arbitraryGiveDiscountCommand
        , arbitraryBuyCommand
        , return Lib2.ViewCommand
        , arbitraryTotalCommand
        , arbitraryCheckShippingCommand
        , arbitraryCompareCommand
        ]
      where
        arbitraryAddCommand = do
            products <- listOf1 arbitrary
            return $ Lib2.AddCommand products
        arbitraryGiveDiscountCommand = do
            prodOrIndex <- arbitraryProdOrIndex
            discount <- arbitraryPositiveInteger
            return $ Lib2.GiveDiscountCommand prodOrIndex discount
        arbitraryBuyCommand = do
            qty <- arbitraryPositiveInteger
            prodOrIndex <- arbitraryProdOrIndex
            return $ Lib2.BuyCommand qty prodOrIndex
        arbitraryTotalCommand = do
            prodOrIndex <- arbitraryProdOrIndex
            return $ Lib2.TotalCommand prodOrIndex
        arbitraryCheckShippingCommand = do
            prodOrIndex <- arbitraryProdOrIndex
            return $ Lib2.CheckShippingCommand prodOrIndex
        arbitraryCompareCommand = do
            p1 <- arbitraryProdOrIndex
            p2 <- arbitraryProdOrIndex
            return $ Lib2.CompareCommand p1 p2


instance Arbitrary Lib3.Statements where
    arbitrary :: Gen Lib3.Statements
    arbitrary = oneof
        [ Lib3.Single <$> arbitrary
        , Lib3.Batch <$> resize 5 (listOf arbitrary) 
        ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests, interpreterInMemoryTests]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [
    testProperty "Saving and loading preserves state" saveLoadPreservesState
  ]
  
saveLoadPreservesState :: Lib3.Statements -> Property
saveLoadPreservesState statements = monadicIO $ do
    let initialState = Lib2.emptyState
    let stateAfterStatements = applyStatements' initialState statements
    case stateAfterStatements of
        Left _ -> do
            pre False
        Right stateAfterStmts -> do
            if stateAfterStmts == initialState
                then pre False
                else do
                    let marshalledStatements = Lib3.marshallState stateAfterStmts
                    let rendered = Lib3.renderStatements marshalledStatements
                    case Lib3.parseStatements rendered of
                        Left parseErr -> fail $ "Parsing failed: " ++ parseErr
                        Right (parsedStmts, _) -> do
                            let stateAfterLoad = applyStatements' Lib2.emptyState parsedStmts
                            assert (stateAfterLoad == Right stateAfterStmts)
      where
        applyStatements' :: Lib2.State -> Lib3.Statements -> Either String Lib2.State
        applyStatements' s stmts = Lib3.applyStatements s stmts

unitTests :: TestTree
unitTests = testGroup "Lib2 and Lib3 tests"
  [ 
    testCase "Parsing empty query" $
     Lib2.parseQuery "" @?= Left "No parser matched",

  testGroup "Product Parsing"
      [ testCase "Valid board game with components" $
          case Lib2.parse Lib2.parseBoardGame "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)" of
              (Right product, "") -> product @?= Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"]
              (Left err, _) -> assertFailure $ "Unexpected parse error: " ++ err
              (_, leftover) -> assertFailure $ "Unexpected leftover: " ++ leftover,
        
        testCase "Parsing a board game with components" $
          case Lib2.parse Lib2.parseProduct "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)" of
              (Right product, "") -> product @?= Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"]
              (Left err, _) -> assertFailure ("Unexpected parse error: " ++ err)
              (_, leftover) -> assertFailure ("Unexpected leftover: " ++ leftover),
        
        testCase "Parsing a board game with add-ons" $
          case Lib2.parse Lib2.parseProduct "baseTM 158.99eur (contains: 2 tile, 1 gameBoard) [includes: playerBoard 10eur, metalResource 20eur]" of
              (Right product, "") -> product @?= Lib2.BoardGameWithAddOns "baseTM" 158.99 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"] [Lib2.AddOn "playerBoard" 10.0, Lib2.AddOn "metalResource" 20.0]
              (Left err, _) -> assertFailure ("Unexpected parse error: " ++ err)
              (_, leftover) -> assertFailure ("Unexpected leftover: " ++ leftover),
        
        testCase "Parsing an add-on" $
          case Lib2.parse Lib2.parseProduct "cardSleeve 5eur" of
              (Right product, "") -> product @?= Lib2.AddOn "cardSleeve" 5.0
              (Left err, _) -> assertFailure ("Unexpected parse error: " ++ err)
              (_, leftover) -> assertFailure ("Unexpected leftover: " ++ leftover),

        testCase "Parsing a component" $
          case Lib2.parse Lib2.parseProduct "3 marker" of
              (Right product, "") -> product @?= Lib2.Component 3 "marker" 
              (Left err, _) -> assertFailure ("Unexpected parse error: " ++ err)
              (_, leftover) -> assertFailure ("Unexpected leftover: " ++ leftover)
      ],

    testGroup "Batch Parsing Tests"
      [ 
        testCase "Parsing a valid batch of commands" $ do
          let input = "BEGIN add corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard); giveDiscount corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard) 10%; END"
          let expected = Right (Lib3.Batch 
                                [ Lib2.AddCommand [Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"]]
                                , Lib2.GiveDiscountCommand (Left (Lib2.BoardGame "corporateCEOTM" 100.0 [])) 10
                                ], "")
          Lib3.parseStatements input @?= expected,

        testCase "Parsing a batch with missing END" $ do
          let input = "BEGIN add corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard); giveDiscount corporateCEOTM 10%;"
          let expected = Left "No parser matched"
          Lib3.parseStatements input @?= expected,

        testCase "Parsing an empty batch" $ do
          let input = "BEGIN END"
          let expected = Right (Lib3.Batch [], "")
          Lib3.parseStatements input @?= expected
      ]
  ]

interpreterInMemoryTests :: TestTree
interpreterInMemoryTests = testGroup "InterpretersInMemory Tests"
  [
   testCase "Add product and view state" $ do
      let prod = Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"]
      (respAdd, respView) <- runInMemory $ do
        respAdd <- addProducts [prod]
        respView <- viewState
        return (respAdd, respView)

      respAdd @?= Just "New products added to the state."
      case respView of
        Just s -> assertBool ("View output should contain 'corporateCEOTM'") ("corporateCEOTM" `isInfixOf` s)
        Nothing -> assertFailure "View did not return anything.",

  testCase "Save and then load state" $ do
      let prod = Lib2.AddOn "cardSleeve" 5.0
      (saveMsg, loadMsg, viewMsg) <- runInMemory $ do
        _ <- addProducts [prod]
        sMsg <- saveState
        lMsg <- loadState
        vMsg <- viewState
        return (sMsg, lMsg, vMsg)

      saveMsg @?= Just "In-memory state saved."
      loadMsg @?= Just "In-memory state loaded."
      case viewMsg of
        Just s -> assertBool "View output should contain 'cardSleeve'" ("cardSleeve" `isInfixOf` s)
        Nothing -> assertFailure "View did not return anything.",

  testCase "Buy a product" $ do
      let prod = Lib2.Component 3 "marker"
      (buyMsg, viewAfter) <- runInMemory $ do
        _ <- addProducts [prod]
        bMsg <- buyProduct 1 (Left prod)
        vMsg <- viewState
        return (bMsg, vMsg)

      case buyMsg of
        Just s -> assertBool "Buy message should mention 'bought' or 'purchase history'." 
                   ("bought" `isInfixOf` s || "purchase history" `isInfixOf` s)
        Nothing -> assertFailure "No message returned after buying."
      case viewAfter of
        Just s -> assertBool "View should reflect the product 'marker' after purchase." 
                   ("marker" `isInfixOf` s)
        Nothing -> assertFailure "View did not return anything.",

  testCase "Apply Black Friday discount" $ do
      let prod = Lib2.BoardGame "baseTM" 200.0 []
      (bfMsg, totalBefore, totalAfter) <- runInMemory $ do
        _ <- addProducts [prod]
        before <- getTotal (Left prod)
        bf <- blackFriday
        after <- getTotal (Left prod)
        return (bf, before, after)
      bfMsg @?= Just "Black Friday started!!! All products now at half the price!"
      totalBefore @?= Just "Total price of the product: 200.0 eur."
      totalAfter @?= Just "Total price of the product: 100.0 eur."
  ]