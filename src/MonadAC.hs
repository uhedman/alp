{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadAC (
  AC,
  runAC,
  runEmptyAC,
  printAC,
  printStrAC,
  setLastFile,
  getLastFile,
  getTable,
  getRows,
  getCols,
  getEpoch,
  getIsHalted,
  catchErrors,
  MonadAC,
  module Control.Monad.Except,
  module Control.Monad.State)
 where

import Global
import Errors ( Error(..) )
import Control.Monad.State
import Control.Monad.Except
import System.IO
import Lang

class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadAC m where

printAC :: MonadAC m => String -> m ()
printAC = liftIO . putStrLn

printStrAC :: MonadAC m => String -> m ()
printStrAC = liftIO . putStr

getTable :: MonadAC m => m Table
getTable = gets table

setCols :: MonadAC m => Int -> m ()
setCols n = modify (\s -> s {cols = n})

getCols :: MonadAC m => m Int
getCols = gets cols

setRows :: MonadAC m => Int -> m ()
setRows n = modify (\s -> s {rows = n})

getRows :: MonadAC m => m Int
getRows = gets rows

getEpoch :: MonadAC m => m Int
getEpoch = gets epoch

getIsHalted :: MonadAC m => m Bool
getIsHalted = gets isHalted

setLastFile :: MonadAC m => FilePath -> m ()
setLastFile filename = modify (\s -> s {lfile = filename})

getLastFile :: MonadAC m => m FilePath
getLastFile = gets lfile

catchErrors  :: MonadAC m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c)
                           (\e -> liftIO $ hPrint stderr e
                              >> return Nothing)

type AC = StateT GlEnv (ExceptT Error IO)

-- | Esta es una instancia vacía, ya que 'MonadAC' no tiene funciones miembro.
instance MonadAC AC

runAC' :: AC a -> IO (Either Error (a, GlEnv))
runAC' c = runExceptT $ runStateT c initialEnvEmpty

runAC:: AC a -> IO (Either Error a)
runAC c = fmap fst <$> runAC' c 

runEmptyAC' :: AC a -> IO (Either Error (a, GlEnv))
runEmptyAC' c = runExceptT $ runStateT c initialEnvEmpty

runEmptyAC:: AC a -> IO (Either Error a)
runEmptyAC c = fmap fst <$> runEmptyAC' c 
