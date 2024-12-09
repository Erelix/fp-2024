{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib2
    ( Query(..),
      Product(..),
      Products(..),
      parseQuery,
      parseProduct,
      parseProducts,
      parseComponent,
      parseAddOnName,
      parseBoardGameName,
      parseComponentName,
      parseQuantity,
      parseDiscount,
      parsePrice,
      parseBoardGame,
      parseBoardGameWithAddOns,
      parseAddOn,
      parse,
      State(..),
      emptyState,
      stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Char (isDigit, isSpace)
import Data.List (find)

import Control.Monad.Trans.Except
import Control.Monad.Except (throwError, catchError)
import qualified Control.Monad.State as S
import Control.Monad.Trans (lift)


type Parser a = ExceptT String (S.State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = S.runState (runExceptT parser)


data Products = Products [Product] deriving (Eq, Show) 

data Product = BoardGame String Double [Product]
             | AddOn String Double
             | Component Integer String
             | BoardGameWithAddOns String Double [Product] [Product]
             deriving (Show, Generic)

instance Eq Product where
  (BoardGame name1 _ _) == (BoardGame name2 _ _) = name1 == name2
  (AddOn name1 _) == (AddOn name2 _) = name1 == name2
  (Component _ name1) == (Component _ name2) = name1 == name2
  (BoardGameWithAddOns name1 _ _ _) == (BoardGameWithAddOns name2 _ _ _) = name1 == name2
  _ == _ = False


-- Monadic AND combinators
and1' :: (a -> b) -> Parser a -> Parser b
and1' f p = do
  a <- p
  return (f a)

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f p1 p2 = do
  a <- p1
  b <- p2
  return (f a b)

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f p1 p2 p3 = do
  a <- p1
  b <- p2
  c <- p3
  return (f a b c)

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f p1 p2 p3 p4 = do
  a <- p1
  b <- p2
  c <- p3
  d <- p4
  return (f a b c d)

and5' :: (a -> b -> c -> d -> e -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser g
and5' f p1 p2 p3 p4 p5 = do
  a <- p1
  b <- p2
  c <- p3
  d <- p4
  e <- p5
  return (f a b c d e)

and6' :: (a -> b -> c -> d -> e -> g -> h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser g -> Parser h
and6' f p1 p2 p3 p4 p5 p6 = do
  a <- p1
  b <- p2
  c <- p3
  d <- p4
  e <- p5
  g <- p6
  return (f a b c d e g)


-- OR combinator that tries multiple parsers and returns first success
orX :: [Parser a] -> Parser a
orX [] = throwError "No parser matched"
orX (p:ps) = do
    s <- lift S.get
    p `catchError` \_ -> do
        lift (S.put s)
        if null ps
           then throwError "No parser matched"
           else orX ps


-- Basic parsers
parseNumber :: Parser Integer
parseNumber = do
    input <- S.get
    let (digits, rest) = span C.isDigit input
    if null digits
       then throwError "Not a number"
       else do
           S.put rest
           return (read digits)

parseChar :: Char -> Parser Char
parseChar a = do
    input <- S.get
    case input of
        [] -> throwError "Empty input"
        (x:xs) ->
            if x == a
                then do S.put xs; return x
                else throwError $ "Expected '" ++ [a] ++ "' but got '" ++ [x] ++ "'"

parseString :: String -> Parser String
parseString str = do
    input <- S.get
    if str `L.isPrefixOf` input
       then do
           S.put (drop (length str) input)
           return str
       else throwError ("Expected " ++ str)

parseDouble :: Parser Double
parseDouble = do
    input <- S.get
    let (digits, rest) = span (\c -> isDigit c || c == '.') input
    if null digits
       then throwError "Not a number"
       else case readMaybe digits :: Maybe Double of
              Just num -> do S.put rest; return num
              Nothing -> throwError "Invalid number format"


-- BNF implementation

-- <quantity> ::= <number>
parseQuantity :: Parser Integer
parseQuantity = parseNumber

-- <discount> ::= <number> "%"
parseDiscount :: Parser Integer
parseDiscount = and2' (\number _ -> number) 
                parseNumber 
                (parseChar '%')

-- <price> ::= <number> "eur" | <double> "eur"
parsePrice :: Parser Double
parsePrice = and2' (\num _ -> num) parseDouble (parseString "eur")


-- <boardgame_name> ::= one of several fixed strings
parseBoardGameName :: Parser String
parseBoardGameName = orX 
    [ parseString "corporateCEOTM"
    , parseString "baseTM"
    , parseString "bigBoxTM"
    , parseString "venusTMexp"
    , parseString "turmoilTMexp"
    , parseString "preludeTMexp"
    , parseString "prelude1TMexp"
    , parseString "prelude2TMexp"
    , parseString "coloniesTMexp"
    , parseString "ellas&hellasTMexp"
    , parseString "automaTMexp"
    , parseString "baseTMAE"
    , parseString "discoveryTMAEexp"
    , parseString "foundationsTMAEexp"
    , parseString "crisisTMAEexp"
    ]


-- <boardgame> ::= <boardgame_name> " " <price> " (contains: " <products> ")"
parseBoardGame :: Parser Product
parseBoardGame = and6' (\name _ price _ components _ -> BoardGame name price components)
                    parseBoardGameName
                    (parseChar ' ')
                    parsePrice
                    (parseString " (contains: ")
                    parseProducts
                    (parseChar ')')

-- <boardgame_with_addons> ::= <boardgame> "[includes: " <products> "]"
parseBoardGameWithAddOns :: Parser Product
parseBoardGameWithAddOns = and4' (\(BoardGame name price components) _ addons _ -> 
                                  BoardGameWithAddOns name price components addons)
                                  parseBoardGame
                                  (parseString " [includes: ")
                                  parseProducts
                                  (parseChar ']')

-- <component_name> ::= "tile" | "gameBoard" | ...
parseComponentName :: Parser String
parseComponentName = orX 
    [ parseString "tile"
    , parseString "gameBoard"
    , parseString "playerBoard"
    , parseString "card"
    , parseString "marker"
    , parseString "rules"
    ]

-- <component> ::= <quantity> " " <component_name>
parseComponent :: Parser Product
parseComponent = and3' (\quantity _ name-> Component quantity name) 
                       parseQuantity 
                       (parseChar ' ') 
                       parseComponentName

-- <add_on_name> ::= "playerBoard" | "miniature" | ...
parseAddOnName :: Parser String
parseAddOnName = orX 
    [ parseString "playerBoard"
    , parseString "miniature"
    , parseString "metalResource"
    , parseString "cardSleeve"
    , parseString "spaceInsert"
    ]

-- <add_on> ::= <add_on_name> " " <price>
parseAddOn :: Parser Product
parseAddOn = and3' (\name _ price -> AddOn name price)
                parseAddOnName
                (parseChar ' ')
                parsePrice

-- <product> ::= <boardgame_with_addons> | <boardgame> | <add_on> | <component>
parseProduct :: Parser Product
parseProduct = orX 
    [ parseBoardGameWithAddOns
    , parseBoardGame
    , parseAddOn
    , parseComponent
    ]

-- <products> ::= <product> | <product> ", " <products>
parseProducts :: Parser [Lib2.Product]
parseProducts = do
    input <- S.get
    -- If next char is ')' or ']' or input is empty, return empty list.
    if null input || head input == ')' || head input == ']'
       then return []
       else do
           first <- parseProduct
           (do _ <- parseString ", "
               rest <- parseProducts
               return (first : rest)) `catchError` \_ -> return [first]


returnNoProduct :: Parser Product
returnNoProduct = do
    input <- S.get
    -- If we can't parse a product and the next char is ')', allow empty
    if ")" `L.isPrefixOf` input
       then throwError "No product present but that's allowed here"
       else throwError "No parser matched"



-- <product_or_index> ::= <number> | <product>
parseProductOrIndex :: Parser (Either Product Int)
parseProductOrIndex = orX
    [ parseProductAsLeft
    , parseNumberAsIndex
    ]

parseProductAsLeft :: Parser (Either Product Int)
parseProductAsLeft = do
    product <- parseProduct
    return (Left product)

parseNumberAsIndex :: Parser (Either Product Int)
parseNumberAsIndex = do
    num <- parseNumber
    return (Right (fromInteger num))


-- Queries
data Query = CheckShippingCommand (Either Product Int)
           | AddCommand [Product] 
           | GiveDiscountCommand (Either Product Int) Integer
           | BuyCommand Integer (Either Product Int)
           | CompareCommand (Either Product Int) (Either Product Int)
           | ViewCommand
           | BlackFridayCommand
           | TotalCommand (Either Product Int)
           deriving (Generic)

instance Eq Query where
    (CheckShippingCommand p1) == (CheckShippingCommand p2) = p1 == p2
    (AddCommand ps1) == (AddCommand ps2) = ps1 == ps2
    (GiveDiscountCommand p1 d1) == (GiveDiscountCommand p2 d2) = p1 == p2 && d1 == d2
    (BuyCommand q1 p1) == (BuyCommand q2 p2) = q1 == q2 && p1 == p2
    (CompareCommand p1 q1) == (CompareCommand p2 q2) = p1 == p2 && q1 == q2
    (TotalCommand p1) == (TotalCommand p2) = p1 == p2
    ViewCommand == ViewCommand = True
    BlackFridayCommand == BlackFridayCommand = True
    _ == _ = False

instance Show Query where
    show (CheckShippingCommand p) = "CheckShippingCommand " ++ showEitherProductInt p
    show (AddCommand ps) = "AddCommand " ++ show ps
    show (GiveDiscountCommand p d) = "GiveDiscountCommand " ++ showEitherProductInt p ++ " " ++ show d
    show (BuyCommand q p) = "BuyCommand " ++ show q ++ " " ++ showEitherProductInt p
    show (CompareCommand p1 p2) = "CompareCommand " ++ showEitherProductInt p1 ++ " " ++ showEitherProductInt p2 
    show (TotalCommand p) = "TotalCommand " ++ showEitherProductInt p
    show ViewCommand = "ViewCommand"
    show BlackFridayCommand = "BlackFridayCommand"

showEitherProductInt :: Either Product Int -> String
showEitherProductInt (Left product) = "Product(" ++ show product ++ ")"
showEitherProductInt (Right index) = "Index(" ++ show index ++ ")"


-- <view_command> ::= "view"
parseViewCommand :: Parser Query
parseViewCommand = do
    _ <- parseString "view"
    return ViewCommand

-- <check_shipping_command> ::= "checkShipping " <product_or_index>
parseCheckShippingCommand :: Parser Query
parseCheckShippingCommand = and2' (\_ productOrIndex -> CheckShippingCommand productOrIndex)
                          (parseString "checkShipping ")
                          parseProductOrIndex

-- <add_command> ::= "add " <products>
parseAddCommand :: Parser Query
parseAddCommand = and2' (\_ ps -> AddCommand ps) 
                  (parseString "add ")
                  parseProducts

-- <discount_command> ::= "giveDiscount " <product_or_index> " " <discount>
parseGiveDiscountCommand :: Parser Query
parseGiveDiscountCommand = and4'
    (\_ productOrIndex _ discount -> GiveDiscountCommand productOrIndex discount)
    (parseString "giveDiscount ")
    parseProductOrIndex
    (parseChar ' ')
    parseDiscount

-- <buy_command> ::= "buy " <quantity> " " <product_or_index>
parseBuyCommand :: Parser Query
parseBuyCommand = and4' (\_ quantity _ productOrIndex -> BuyCommand quantity productOrIndex)
                          (parseString "buy ")
                          parseQuantity
                          (parseChar ' ')
                          parseProductOrIndex

-- <total_command> ::= "total " <product_or_index>
parseTotalCommand :: Parser Query
parseTotalCommand = and2' (\_ productOrIndex -> TotalCommand productOrIndex)
                            (parseString "total ")
                            parseProductOrIndex

-- <compare_command> ::= "compare " <product_or_index> " " <product_or_index>
parseCompareCommand :: Parser Query
parseCompareCommand = and4' (\_ p1 _ p2 -> CompareCommand p1 p2)
                        (parseString "compare ")
                        parseProductOrIndex
                        (parseChar ' ')
                        parseProductOrIndex

-- <black_friday_command> ::= "blackFriday"
parseBlackFridayCommand :: Parser Query
parseBlackFridayCommand = and1' (const BlackFridayCommand) 
                                (parseString "blackFriday")


-- | Parses user's input.
parseQuery :: String -> Either String (Query, String)
parseQuery s =
    case S.runState (runExceptT $ orX 
        [ parseCheckShippingCommand
        , parseAddCommand
        , parseGiveDiscountCommand
        , parseBuyCommand
        , parseCompareCommand
        , parseViewCommand
        , parseBlackFridayCommand
        , parseTotalCommand
        ]) s of
        (Left err, _) -> Left err
        (Right query, rest) -> Right (query, rest)


type PurchaseHistory = [(Either Product Int, Integer)]

data State = State
    { products :: [Product]
    , discounts :: [(Product, Integer)]
    , purchaseHistory :: PurchaseHistory
    } deriving (Eq, Show)


presetProducts :: [Product]
presetProducts = []

presetDiscounts :: [(Product, Integer)]
presetDiscounts = []

emptyState :: State
emptyState = State 
    { products = presetProducts,
      discounts = presetDiscounts,
      purchaseHistory = []
    }

viewState :: State -> String
viewState (State products discounts purchaseHistory) =
  "Current State:\n"
    ++ "Products:\n"
    ++ unlines (zipWith (\i p -> show i ++ ".  " ++ show p) [1..] products)
    ++ "Discounts:\n"
    ++ unlines (map (\(p, d) -> "  " ++ show p ++ " with " ++ show d ++ "% discount") discounts)
    ++ "Purchase History:\n"
    ++ unlines (map (\(p, q) -> "  " ++ show q ++ " units of " ++ show p) purchaseHistory)


stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
  AddCommand newProducts ->
    let updatedProducts = products state ++ newProducts
        newState = state { products = updatedProducts }
    in Right (Just "New products added to the state.", newState)

  GiveDiscountCommand productIdentifier discount ->
    case getProductFromEither (products state) productIdentifier of
        Just product -> 
          let updatedDiscounts = updateOrAddDiscount (discounts state) product discount
              newState = state { discounts = updatedDiscounts }
          in Right (Just $ "Discount applied to " ++ show product ++ ".", newState)
        Nothing -> Left "Error: Invalid product identifier provided."

  BlackFridayCommand ->
    let blackFridayDiscounts = [(product, 50) | product <- products state] 
        newState = state { discounts = blackFridayDiscounts }
    in Right (Just "Black Friday started!!! All products now at half the price!", newState)

  BuyCommand quantity productOrIndex ->
    case getProductFromEither (products state) productOrIndex of
        Just product ->
            let unitPrice = calculateTotalWithinProduct product (discounts state)
                totalPrice = fromIntegral quantity * unitPrice
                newPurchaseHistory = (productOrIndex, quantity) : purchaseHistory state
                newState = state { purchaseHistory = newPurchaseHistory }
            in Right (Just ("Product bought for " ++ show totalPrice ++ " eur and added to purchase history."), newState)
        Nothing ->
            Left "Error: Invalid product or index provided."

  TotalCommand productOrIndex ->
    case getProductFromEither (products state) productOrIndex of
        Just product ->
            let total = calculateTotalWithinProduct product (discounts state)
            in Right (Just $ "Total price of the product: " ++ show total ++ " eur.", state)
        Nothing ->
            Left "Error: Invalid product or index provided."

  CheckShippingCommand productOrIndex ->
    case getProductFromEither (products state) productOrIndex of
        Just product ->
            let total = calculateTotalWithinProduct product (discounts state)
                shippingCost = if total >= 70.0 then 0.0 else 3.99
                message = if shippingCost == 0.0
                            then "Shipping is free for this product."
                            else "Shipping cost for this product is 3.99 eur."
            in Right (Just message, state)
        Nothing ->
            Left "Error: Invalid product or index provided."

  CompareCommand productOrIndex1 productOrIndex2 ->
    case (getProductFromEither (products state) productOrIndex1, 
          getProductFromEither (products state) productOrIndex2) of
      (Just product1, Just product2) ->
        let total1 = calculateTotalWithinProduct product1 (discounts state)
            total2 = calculateTotalWithinProduct product2 (discounts state)
        in if total1 < total2
            then Right (Just $ show product1 ++ " is cheaper than " ++ show product2 ++ " by " ++ show (total2 - total1) ++ " eur.", state)
            else if total2 < total1
                then Right (Just $ show product2 ++ " is cheaper than " ++ show product1 ++ " by " ++ show (total1 - total2) ++ " eur.", state)
                else Right (Just $ "Both products have the same total price of " ++ show total1 ++ " eur.", state)
      _ -> Left "Error: One or both invalid product or index provided"

  ViewCommand ->
    Right (Just $ "State: " ++ viewState state, state)


getProductByIndex :: [Product] -> Int -> Maybe Product
getProductByIndex products index
  | index > 0 && index <= length products = Just (products !! (index - 1))
  | otherwise = Nothing

updateOrAddDiscount :: [(Product, Integer)] -> Product -> Integer -> [(Product, Integer)]
updateOrAddDiscount discounts product discount =
    let updatedDiscounts = map (\(p, d) ->
            if productName p == productName product
            then (product, discount) else (p, d)) discounts
    in if any (\(p, _) -> productName p == productName product) discounts
       then updatedDiscounts
       else (product, discount) : discounts


calculateTotalWithinProduct :: Product -> [(Product, Integer)] -> Double
calculateTotalWithinProduct product discounts = case product of
    BoardGame _ price components ->
        let discount = getDiscountForProduct product discounts
            discountedPrice = applyDiscount price discount
            componentsTotal = sum [calculateTotalWithinProduct c discounts | c <- components]
        in discountedPrice + componentsTotal
    
    AddOn _ price ->
        let discount = getDiscountForProduct product discounts
        in applyDiscount price discount

    Component _ _ -> 0.0

    BoardGameWithAddOns _ price components addons ->
        let discount = getDiscountForProduct product discounts
            discountedPrice = applyDiscount price discount
            componentsTotal = sum [calculateTotalWithinProduct c discounts | c <- components]
            addonsTotal = sum [calculateTotalWithinProduct a discounts | a <- addons]
        in discountedPrice + componentsTotal + addonsTotal

getDiscountForProduct :: Product -> [(Product, Integer)] -> Integer
getDiscountForProduct product discounts =
    case L.find (\(p, _) -> productName p == productName product) discounts of
        Just (_, discount) -> discount
        Nothing -> 0

productName :: Product -> String
productName (BoardGame name _ _) = name
productName (AddOn name _) = name
productName (Component _ name) = name
productName (BoardGameWithAddOns name _ _ _) = name

applyDiscount :: Double -> Integer -> Double
applyDiscount price discountPercent = price * (1 - fromIntegral discountPercent / 100.0)

getProductFromEither :: [Product] -> Either Product Int -> Maybe Product
getProductFromEither products (Left product) =
    find (\p -> productName p == productName product) products
getProductFromEither products (Right index) = getProductByIndex products index
