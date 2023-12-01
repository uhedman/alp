{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadChess (
  Chess,
  setInter,
  getInter,
  runChess,
  printChess,
  printStrChess,
  setLastFile,
  getLastFile,
  getTablero,
  catchErrors,
  MonadChess,
  module Control.Monad.Except,
  module Control.Monad.State)
 where

import Global
import Errors ( Error(..) )
import Control.Monad.State
import Control.Monad.Except
import System.IO
import Lang (Tablero)

class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadChess m where

setInter :: MonadChess m => Bool -> m ()
setInter b = modify (\s-> s {inter = b})

getInter :: MonadChess m => m Bool
getInter = gets inter

printChess :: MonadChess m => String -> m ()
printChess = liftIO . putStrLn

printStrChess :: MonadChess m => String -> m ()
printStrChess = liftIO . putStr

getTablero :: MonadChess m => m Tablero
getTablero = gets tablero

setLastFile :: MonadChess m => FilePath -> m ()
setLastFile filename = modify (\s -> s {lfile = filename})

getLastFile :: MonadChess m => m FilePath
getLastFile = gets lfile

catchErrors  :: MonadChess m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c)
                           (\e -> liftIO $ hPrint stderr e
                              >> return Nothing)

type Chess = StateT GlEnv (ExceptT Error IO)

-- | Esta es una instancia vacía, ya que 'MonadChess' no tiene funciones miembro.
instance MonadChess Chess

runChess' :: Chess a -> IO (Either Error (a, GlEnv))
runChess' c = runExceptT $ runStateT c initialEnv

runChess:: Chess a -> IO (Either Error a)
runChess c = fmap fst <$> runChess' c
