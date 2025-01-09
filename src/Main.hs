module Main where
import PPrint
import Parse
import MonadAC
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)
import Eval
import System.Environment (getArgs)

prompt :: String
prompt = "gameoflife> "

main :: IO ()
main = do
  args <- getArgs
  let createFlag = "--create" `elem` args || "-c" `elem` args
  if createFlag 
    then do runEmptyAC (runInputT defaultSettings setup)
            return ()
    else do runAC (runInputT defaultSettings setup)
            return ()

setup :: (MonadAC m, MonadMask m) => InputT m ()
setup = do
       liftIO $ putStrLn $ "Game of Life.\n"
                         ++ "Ingrese start para comenzar el juego.\n"
                         ++ "Ingrese R/K x y para revivir o matar una celula en (x, y).\n"
                         ++ "Ingrese q para salir\n"
       strTable <- lift pp
       lift $ printAC strTable
       loop
  where loop = do minput <- getInputLine prompt
                  case minput of
                    Nothing -> return ()
                    Just "" -> loop
                    Just "q" -> return ()
                    Just "start" -> game
                    Just cmd -> do commSet cmd
                                   strTable <- lift pp
                                   lift $ printAC strTable
                                   loop

game :: (MonadAC m, MonadMask m) => InputT m ()
game = do
       liftIO $ putStrLn $ "Game of Life.\n"
                         ++ "Ingrese R/K x y para revivir o matar una celula en (x, y).\n"
                         ++ "Ingrese enter para simular indefinidamente\n"
                         ++ "Ingrese step para simular una epoca\n"
                         ++ "Ingrese q para salir\n"
       str <- lift pp
       lift $ printAC str
       loop
  where loop = do minput <- getInputLine prompt
                  case minput of
                    Nothing -> return ()
                    Just "step" -> do lift execStep
                                      epoch <- lift getEpoch
                                      lift $ printAC $ "Epoch: " ++ show epoch
                                      str <- lift pp
                                      lift $ printAC str
                                      loop
                    Just "" -> inf 
                               where inf = do lift execStep
                                              epoch <- lift getEpoch
                                              lift $ printAC $ "Epoch: " ++ show epoch
                                              str <- lift pp
                                              lift $ printAC str
                                              h <- lift getIsHalted
                                              if h then do lift $ printAC "Halted table"
                                                           loop
                                                   else inf
                    Just "q" -> return ()
                    Just cmd -> do commSet cmd
                                   strTable <- lift pp
                                   lift $ printAC strTable
                                   loop

commSet :: MonadAC m => String -> InputT m ()
commSet phrase = case parseSet phrase of
                Left e -> lift $ printAC $ "Error: " ++ show e
                Right a -> lift $ execSet a
