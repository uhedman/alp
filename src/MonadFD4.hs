{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

{-|
Module      : MonadFD4
Description : Mónada con soporte para estado, errores, e IO.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definimos la clase de mónadas 'MonadFD4' que abstrae las mónadas con soporte para estado, errores e IO,
y la mónada 'FD4' que provee una instancia de esta clase.
-}

module MonadFD4 (
  FD4,
  runFD4,
  printFD4,
  printStrFD4,
  setLastFile,
  getLastFile,
  setInter,
  getInter,
  failPosFD4,
  failFD4,
  catchErrors,
  MonadFD4,
  module Control.Monad.Except,
  module Control.Monad.State)
 where

import Common
import Global
import Errors ( Error(..) )
import Control.Monad.State
import Control.Monad.Except
import System.IO

-- * La clase 'MonadFD4'

{-| La clase de mónadas 'MonadFD4' clasifica a las mónadas con soporte para una configuración Global 'Global.Conf', 
    para operaciones @IO@, estado de tipo 'Global.GlEnv', y errores de tipo 'Errors.Error'.

Las mónadas @m@ de esta clase cuentan con las operaciones:
   - @ask :: m Conf@
   - @get :: m GlEnv@
   - @put :: GlEnv -> m ()@
   - @throwError :: Error -> m a@
   - @catchError :: m a -> (Error -> m a) -> m a@
   - @liftIO :: IO a -> m a@

y otras operaciones derivadas de ellas, como por ejemplo
   - @modify :: (GlEnv -> GlEnv) -> m ()@
   - @gets :: (GlEnv -> a) -> m a@  
-}
class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadFD4 m where

setInter :: MonadFD4 m => Bool -> m ()
setInter b = modify (\s-> s {inter = b})

getInter :: MonadFD4 m => m Bool
getInter = gets inter

printFD4 :: MonadFD4 m => String -> m ()
printFD4 = liftIO . putStrLn

printStrFD4 :: MonadFD4 m => String -> m ()
printStrFD4 = liftIO . putStr

setLastFile :: MonadFD4 m => FilePath -> m ()
setLastFile filename = modify (\s -> s {lfile = filename})

getLastFile :: MonadFD4 m => m FilePath
getLastFile = gets lfile

failPosFD4 :: MonadFD4 m => Pos -> String -> m a
failPosFD4 p s = throwError (ErrPos p s)

failFD4 :: MonadFD4 m => String -> m a
failFD4 = failPosFD4 NoPos

catchErrors  :: MonadFD4 m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c)
                           (\e -> liftIO $ hPrint stderr e
                              >> return Nothing)

----
-- Importante, no eta-expandir porque GHC no hace una
-- eta-contracción de sinónimos de tipos
-- y Main no va a compilar al escribir `InputT FD4 ()`

-- | El tipo @FD4@ es un sinónimo de tipo para una mónada construida usando dos transformadores de mónada sobre la mónada @IO@.
-- El transformador de mónad @ExcepT Error@ agrega a la mónada IO la posibilidad de manejar errores de tipo 'Errors.Error'.
-- El transformador de mónadas @StateT GlEnv@ agrega la mónada @ExcepT Error IO@ la posibilidad de manejar un estado de tipo 'Global.GlEnv'.
type FD4 = StateT GlEnv (ExceptT Error IO)

-- | Esta es una instancia vacía, ya que 'MonadFD4' no tiene funciones miembro.
instance MonadFD4 FD4

-- 'runFD4\'' corre una computación de la mónad 'FD4' en el estado inicial 'Global.initialEnv' 
runFD4' :: FD4 a -> IO (Either Error (a, GlEnv))
runFD4' c =  runExceptT $ runStateT c initialEnv

runFD4:: FD4 a -> IO (Either Error a)
runFD4 c = fmap fst <$> runFD4' c
