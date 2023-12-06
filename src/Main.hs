module Main where
import PPrint ( pp )
import Parse (parseMovimiento, parseColocar)
import MonadChess ( MonadIO(liftIO), MonadTrans(lift), MonadChess, printChess, runChess, runEmptyChess, setTurno, getTurno )
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)
import Eval ( execMove, execPlace )
import Lang (Jugador(N, B))
import System.Environment (getArgs)

prompt :: String
prompt = "chess> "

main :: IO ()
main = do
  args <- getArgs
  let createFlag = "--create" `elem` args || "-c" `elem` args
  if createFlag 
    then do runEmptyChess (runInputT defaultSettings setup)
            return ()
    else do runChess (runInputT defaultSettings game)
            return ()

game :: (MonadChess m, MonadMask m) => InputT m ()
game = do
       liftIO $ putStrLn "Juego de ajedrez."
       str <- lift pp
       lift $ printChess str
       loop
  where loop = do minput <- getInputLine prompt
                  case minput of
                    Nothing -> return ()
                    Just "" -> loop
                    Just x -> do exec x
                                 p <- lift getTurno
                                 lift $ printChess $ "Turno de las " ++ (if p == B then "blancas." else "negras.")
                                 str <- lift pp
                                 lift $ printChess str
                                 loop

setup :: (MonadChess m, MonadMask m) => InputT m ()
setup = do
       liftIO $ putStrLn $ "Creacion de tablero.\n" ++
                           "Ingrese start para comenzar el juego.\n" ++
                           "Ingrese B o N para elegir que color comienza el turno."
       str <- lift pp
       lift $ printChess str
       loop
  where loop = do minput <- getInputLine prompt
                  case minput of
                    Nothing -> return ()
                    Just "" -> loop
                    Just "start" -> game
                    Just "B" -> do lift $ setTurno B
                                   lift $ printChess "Las blancas comienzan el juego."
                                   loop
                    Just "N" -> do lift $ setTurno N
                                   lift $ printChess "Las negras comienzan el juego."
                                   loop
                    Just x -> do execSetup x
                                 str <- lift pp
                                 lift $ printChess str
                                 loop

execSetup :: MonadChess m => String -> InputT m ()
execSetup phrase = case parseColocar phrase of
                Left e -> lift $ printChess $ "Error: " ++ show e
                Right (p, j, c) -> lift $ execPlace p j c

exec :: MonadChess m => String -> InputT m ()
exec phrase = case parseMovimiento phrase of
                Left e -> lift $ printChess $ "Error: " ++ show e
                Right mv -> do lift $ printChess (show mv)
                               lift $ execMove mv
