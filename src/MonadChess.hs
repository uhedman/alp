{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadChess (
  Chess,
  runChess,
  runEmptyChess,
  printChess,
  printStrChess,
  setLastFile,
  getLastFile,
  getTurno,
  getEnroqueBlanco,
  getEnroqueNegro,
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
import Lang (Tablero, Jugador)

class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadChess m where

printChess :: MonadChess m => String -> m ()
printChess = liftIO . putStrLn

printStrChess :: MonadChess m => String -> m ()
printStrChess = liftIO . putStr

getEnroqueBlanco :: MonadChess m => m Bool
getEnroqueBlanco = gets enroqueBlanco

getEnroqueNegro :: MonadChess m => m Bool
getEnroqueNegro = gets enroqueNegro

getTurno :: MonadChess m => m Jugador
getTurno = gets turno

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
runChess' c = runExceptT $ runStateT c initialEnvChess

runChess:: Chess a -> IO (Either Error a)
runChess c = fmap fst <$> runChess' c

runEmptyChess' :: Chess a -> IO (Either Error (a, GlEnv))
runEmptyChess' c = runExceptT $ runStateT c initialEnvEmpty

runEmptyChess:: Chess a -> IO (Either Error a)
runEmptyChess c = fmap fst <$> runEmptyChess' c
