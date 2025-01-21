{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadAC (
  AC,
  runAC,
  printAC,
  printStrAC,
  getGame,
  getTable,
  setBoard,
  clearBoard,
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
    ( initialEnv, GlEnv(isHalted, table, cols, rows, epoch, game) )
import Errors ( Error(..) )
import Control.Monad.State
import Control.Monad.Except
import System.IO ( stderr, hPrint )
import Lang ( Table, Game )

class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadAC m where

printAC :: MonadAC m => String -> m ()
printAC = liftIO . putStrLn

printStrAC :: MonadAC m => String -> m ()
printStrAC = liftIO . putStr

getGame :: MonadAC m => m Game
getGame = gets game

getTable :: MonadAC m => m Table
getTable = gets table

setBoard :: MonadAC m => (Table, Int, Int) -> m ()
setBoard (tab, r, c) = modify (\s -> s {table = tab, rows = r, cols = c, epoch = 0, isHalted = False})

clearBoard :: MonadAC m => m ()
clearBoard = modify (\s -> s {table = [], epoch = 0})

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

catchErrors  :: MonadAC m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c)
                           (\e -> liftIO $ hPrint stderr e
                              >> return Nothing)

type AC = StateT GlEnv (ExceptT Error IO)

-- | Esta es una instancia vacía, ya que 'MonadAC' no tiene funciones miembro.
instance MonadAC AC

runAC' :: AC a -> (Table, Int, Int) -> IO (Either Error (a, GlEnv))
runAC' c tab = runExceptT $ runStateT c (initialEnv tab)

runAC:: AC a -> (Table, Int, Int) -> IO (Either Error a)
runAC c tab = fmap fst <$> runAC' c tab
