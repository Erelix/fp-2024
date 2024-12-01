{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure )
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Monadic (monadicIO, assert, pre)
import Test.QuickCheck
import Lib2 qualified
import Lib3 qualified
import GHC.Generics (Generic)

arbitraryPositiveDouble :: Gen Double
arbitraryPositiveDouble = do
    n <- choose (1, 100000) :: Gen Integer
    return (fromIntegral n / 100.0)

arbitraryPositiveInteger :: Gen Integer
arbitraryPositiveInteger = fmap fromIntegral (choose (1, 100) :: Gen Int)

arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = choose (1, 100)

arbitraryProdOrIndex :: Gen (Either Lib2.Product Int)
arbitraryProdOrIndex = oneof [Left <$> arbitrary, Right <$> arbitraryPositiveInt]

addOnNames :: [String]
addOnNames = ["playerBoard", "miniature", "metalResource", "cardSleeve", "spaceInsert"]

boardGameNames :: [String]
boardGameNames = ["corporateCEOTM", "baseTM", "bigBoxTM", "venusTMexp", "turmoilTMexp"]

componentNames :: [String]
componentNames = ["tile", "card", "marker", "gameBoard", "playerBoard"]


-- **1. Define Arbitrary Instances at the Top Level**

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
{--
    shrink = shrinkProduct

shrinkProduct :: Lib2.Product -> [Lib2.Product]
shrinkProduct (Lib2.BoardGame name price components) =
    [Lib2.BoardGame name' price components | name' <- shrinkNonEmpty name] ++
    [Lib2.BoardGame name price' components | price' <- shrinkPositiveDouble price] ++
    [Lib2.BoardGame name price components' | components' <- shrink components]
shrinkProduct (Lib2.AddOn name price) =
    [Lib2.AddOn name' price | name' <- shrinkNonEmpty name] ++
    [Lib2.AddOn name price' | price' <- shrinkPositiveDouble price]
shrinkProduct (Lib2.Component qty name) =
    [Lib2.Component qty' name | qty' <- shrinkPositiveInteger qty] ++
    [Lib2.Component qty name' | name' <- shrinkNonEmpty name]
shrinkProduct _ = []

shrinkNonEmpty :: String -> [String]
shrinkNonEmpty s = filter (not . null) (shrink s)

shrinkPositiveDouble :: Double -> [Double]
shrinkPositiveDouble x = filter (> 0) (shrink x)

shrinkPositiveInteger :: Integer -> [Integer]
shrinkPositiveInteger x = filter (> 0) (shrink x)
--}
instance Arbitrary Lib2.Query where
    arbitrary :: Gen Lib2.Query
    arbitrary = oneof
        [ arbitraryAddCommand
        --, arbitraryGiveDiscountCommand
        , arbitraryBuyCommand
        , return Lib2.ViewCommand
        --, return Lib2.BlackFridayCommand
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
{--
    shrink = shrinkQuery

shrinkQuery :: Lib2.Query -> [Lib2.Query]
shrinkQuery (Lib2.AddCommand products) =
    [Lib2.AddCommand products' | products' <- shrink products, not (null products')]
shrinkQuery (Lib2.GiveDiscountCommand prodOrIndex discount) =
    [Lib2.GiveDiscountCommand prodOrIndex discount' | discount' <- filter (>= 0) (shrink discount)]
shrinkQuery (Lib2.BuyCommand qty prodOrIndex) =
    [Lib2.BuyCommand qty' prodOrIndex | qty' <- shrinkPositiveInteger qty]
shrinkQuery _ = []
--}
instance Arbitrary Lib3.Statements where
    arbitrary :: Gen Lib3.Statements
    arbitrary = oneof
        [ Lib3.Single <$> arbitrary
        --, Lib3.Batch <$> listOf1 arbitrary
        --]
        , Lib3.Batch <$> resize 5 (listOf arbitrary) -- Limit batch size for better parsability
        ]
{--
    shrink (Lib3.Single q) = [Lib3.Single q' | q' <- shrink q]
    shrink (Lib3.Batch qs) =
        [Lib3.Batch qs' | qs' <- shrink qs, not (null qs')]
--}
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 and Lib3 tests"
  [ 
    testCase "Parsing empty query" $
     Lib2.parseQuery "" @?= Left "No parser matched",

    testGroup "parseBoardGame tests"
      [ testCase "Parsing a valid board game with components" $
          Lib2.parseBoardGame "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)" 
          @?= Right (Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"], ""),
        
        testCase "Parsing a board game with decimal price and components" $
          Lib2.parseBoardGame "baseTM 99.99eur (contains: 3 card, 5 marker)" 
          @?= Right (Lib2.BoardGame "baseTM" 99.99 [Lib2.Component 3 "card", Lib2.Component 5 "marker"], ""),

        testCase "Parsing a board game with missing closing parenthesis" $
          Lib2.parseBoardGame "venusTMexp 150eur (contains: 4 tile, 1 playerBoard" 
          @?= Left "Cannot find ')'",

        testCase "Parsing a board game with missing 'contains:'" $
          Lib2.parseBoardGame "turmoilTMexp 180eur ()" 
          @?= Left "' (contains: ' is not found"
      ],

    testGroup "parseComponent tests"
      [ testCase "Parsing a valid component with quantity and name" $
          Lib2.parseComponent "2 tile" @?= Right (Lib2.Component 2 "tile", ""),
        
        testCase "Parsing a valid component with different component name" $
          Lib2.parseComponent "3 gameBoard" @?= Right (Lib2.Component 3 "gameBoard", ""),
        
        testCase "Parsing a valid component with single quantity and name" $
          Lib2.parseComponent "1 card" @?= Right (Lib2.Component 1 "card", ""),
        
        testCase "Parsing an invalid component with non-numeric quantity" $
          Lib2.parseComponent "six marker" @?= Left "Not a number",
        
        testCase "Parsing an invalid component with missing space" $
          Lib2.parseComponent "5card" @?= Left "' ' is not found",
        
        testCase "Parsing an invalid component with unrecognized component name" $
          Lib2.parseComponent "2 gdhjasdhga" @?= Left "No parser matched",

        testCase "Parsing an incomplete component with only quantity" $
          Lib2.parseComponent "7 " @?= Left "No parser matched"
      ],

    testGroup "parseAddOn tests"
      [ testCase "Parsing valid add-on with name 'cardSleeve' and price 5eur" $
          Lib2.parseAddOn "cardSleeve 5eur"
          @?= Right (Lib2.AddOn "cardSleeve" 5.0, ""),

        testCase "Parsing valid add-on with name 'metalResource' and price 15eur" $
          Lib2.parseAddOn "metalResource 15eur"
          @?= Right (Lib2.AddOn "metalResource" 15.0, ""),

        testCase "Parsing valid add-on with name 'spaceInsert' and price 7.50eur" $
          Lib2.parseAddOn "spaceInsert 7.50eur"
          @?= Right (Lib2.AddOn "spaceInsert" 7.50, "")
      ],

    testGroup "Basic Product Parsing Tests"
      [ testCase "Parsing a board game with components" $
          Lib2.parseProduct "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)"
          @?= Right (Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"], ""),

        testCase "Parsing an add-on" $
          Lib2.parseProduct "cardSleeve 5eur"
          @?= Right (Lib2.AddOn "cardSleeve" 5.0, ""),

        testCase "Parsing a component" $
          Lib2.parseProduct "3 marker"
          @?= Right (Lib2.Component 3 "marker", "")
      ],

    testGroup "Advanced Product Parsing with Add-Ons"
      [ testCase "Parsing a board game with add-ons" $
          Lib2.parseProduct 
          "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard) [includes: cardSleeve 5eur, miniature 10eur]"
          @?= Right (Lib2.BoardGameWithAddOns "corporateCEOTM" 100.0 
                [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"]
                [Lib2.AddOn "cardSleeve" 5.0, Lib2.AddOn "miniature" 10.0], ""),

        testCase "Parsing a board game with multiple add-ons" $
          Lib2.parseProduct 
          "baseTM 80eur (contains: 3 card, 5 marker) [includes: playerBoard 15eur, metalResource 20eur]"
          @?= Right (Lib2.BoardGameWithAddOns "baseTM" 80.0 
                [Lib2.Component 3 "card", Lib2.Component 5 "marker"]
                [Lib2.AddOn "playerBoard" 15.0, Lib2.AddOn "metalResource" 20.0], "")
      ],

    testGroup "Other Cases"
      [ testCase "Parsing an invalid product format" $
          Lib2.parseProduct "invalidProduct 100" 
          @?= Left "No parser matched",

        testCase "Parsing a board game missing [includes: ...]" $
          Lib2.parseProduct "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)" 
          @?= Right (Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"], "")
      ],

      testCase "Parsing a complex board game with add-ons and components" $
          Lib2.parseProduct "bigBoxTM 150eur (contains: 2 tile, 1 gameBoard, 5 marker) [includes: playerBoard 10eur, metalResource 20eur]" 
          @?= Right (Lib2.BoardGameWithAddOns "bigBoxTM" 150.0
                [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard", Lib2.Component 5 "marker"]
                [Lib2.AddOn "playerBoard" 10.0, Lib2.AddOn "metalResource" 20.0], "")
  ]


propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [
    testProperty "Saving and loading preserves state" prop_saveLoadPreservesState
  ]



prop_saveLoadPreservesState :: Lib3.Statements -> Property
prop_saveLoadPreservesState statements = monadicIO $ do
    -- Initialize empty state
    let initialState = Lib2.emptyState
    -- Apply statements to initial state
    let stateAfterStatements = applyStatements' initialState statements
    case stateAfterStatements of
        Left _ -> do
            -- Discard invalid sequences
            pre False
        Right stateAfterStmts -> do
            -- Check if the state has changed
            if stateAfterStmts == initialState
                then pre False
                else do
                    -- Marshall state to statements
                    let marshalledStatements = Lib3.marshallState stateAfterStmts
                    -- Render statements to string
                    let rendered = Lib3.renderStatements marshalledStatements
                    -- Parse rendered statements
                    case Lib3.parseStatements rendered of
                        Left parseErr -> fail $ "Parsing failed: " ++ parseErr
                        Right (parsedStmts, _) -> do
                            -- Apply parsed statements to empty state
                            let stateAfterLoad = applyStatements' Lib2.emptyState parsedStmts
                            -- Compare states
                            assert (stateAfterLoad == Right stateAfterStmts)
      where
        applyStatements' :: Lib2.State -> Lib3.Statements -> Either String Lib2.State
        applyStatements' s stmts = Lib3.applyStatements s stmts
