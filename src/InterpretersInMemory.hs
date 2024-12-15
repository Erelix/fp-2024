module InterpretersInMemory (runInMemory) where

import AppDSL
import Lib2 (State(..), emptyState, stateTransition, Query(..), Product(..))
import Control.Monad.Free (Free(..))
import Control.Monad (foldM)
import Data.IORef

data Backup = Backup
    { savedState :: Maybe State
    }

runInMemory :: App a -> IO a
runInMemory app = do
    ref <- newIORef emptyState
    backupRef <- newIORef (Backup Nothing)
    runInMemory' ref backupRef app

runInMemory' :: IORef State -> IORef Backup -> App a -> IO a
runInMemory' _ _ (Pure a) = return a
runInMemory' stateRef backupRef (Free step) = do
    st <- readIORef stateRef
    case step of
      DoAddCommand ps next -> doCmd (AddCommand ps) next
      DoBuyCommand qty p next -> doCmd (BuyCommand qty p) next
      DoGiveDiscountCommand p d next -> doCmd (GiveDiscountCommand p d) next
      DoCheckShippingCommand p next -> doCmd (CheckShippingCommand p) next
      DoCompareCommand p1 p2 next -> doCmd (CompareCommand p1 p2) next
      DoTotalCommand p next -> doCmd (TotalCommand p) next
      DoBlackFriday next -> doCmd BlackFridayCommand next
      DoView next -> doCmd ViewCommand next
      DoSave next -> doSave next
      DoLoad next -> doLoad next
  where
    doCmd q next = do
        st <- readIORef stateRef
        case stateTransition st q of
          Right (mResp, newSt) -> do
            writeIORef stateRef newSt
            runInMemory' stateRef backupRef (next mResp)
          Left err -> error $ "In-memory interpreter error: " ++ err

    doSave next = do
        st <- readIORef stateRef
        modifyIORef' backupRef (\b -> b {savedState = Just st})
        runInMemory' stateRef backupRef (next (Just "In-memory state saved."))

    doLoad next = do
        backup <- readIORef backupRef
        case savedState backup of
          Just bSt -> do
            writeIORef stateRef bSt
            runInMemory' stateRef backupRef (next (Just "In-memory state loaded."))
          Nothing -> runInMemory' stateRef backupRef (next (Just "No saved state to load."))

