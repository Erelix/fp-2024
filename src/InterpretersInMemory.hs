module InterpretersInMemory (runInMemory) where

import AppDSL
import Lib2 (State(..), emptyState, stateTransition, Query(..), Product(..))
import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM)
import Data.IORef

toQuery :: AppF a -> Query
toQuery (DoAddCommand ps _) = AddCommand ps
toQuery (DoBuyCommand qty p _) = BuyCommand qty p
toQuery (DoGiveDiscountCommand p d _) = GiveDiscountCommand p d
toQuery (DoCheckShippingCommand p _) = CheckShippingCommand p
toQuery (DoCompareCommand p1 p2 _) = CompareCommand p1 p2
toQuery (DoTotalCommand p _) = TotalCommand p
toQuery (DoBlackFriday _) = BlackFridayCommand
toQuery (DoView _) = ViewCommand

runInMemory :: App a -> IO a
runInMemory app = do
    ref <- newIORef emptyState
    runInMemory' ref app

runInMemory' :: IORef State -> App a -> IO a
runInMemory' _ (Pure a) = return a
runInMemory' ref (Free step) = do
    let q = toQuery step
    st <- readIORef ref
    case stateTransition st q of
      Right (mResp, newSt) -> do
          writeIORef ref newSt
          case step of
            DoAddCommand _ next -> runInMemory' ref (next mResp)
            DoBuyCommand _ _ next -> runInMemory' ref (next mResp)
            DoGiveDiscountCommand _ _ next -> runInMemory' ref (next mResp)
            DoCheckShippingCommand _ next -> runInMemory' ref (next mResp)
            DoCompareCommand _ _ next -> runInMemory' ref (next mResp)
            DoTotalCommand _ next -> runInMemory' ref (next mResp)
            DoBlackFriday next -> runInMemory' ref (next mResp)
            DoView next -> runInMemory' ref (next mResp)
      Left err -> error $ "In-memory interpreter error: " ++ err
