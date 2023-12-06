module Main where
import PPrint ( pp )
import Parse (parseMovimiento, parseColocar)
import MonadChess ( MonadIO(liftIO), MonadTrans(lift), MonadChess, printChess, runChess, runEmptyChess )
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)
import Eval ( execMove, execPlace )

prompt :: String
prompt = "chess> "

main :: IO ()
main = do -- runChess (runInputT defaultSettings game)
          runEmptyChess (runInputT defaultSettings setup)
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
                                 str <- lift pp
                                 lift $ printChess str
                                 loop

setup :: (MonadChess m, MonadMask m) => InputT m ()
setup = do
       liftIO $ putStrLn "Creacion de tablero."
       str <- lift pp
       lift $ printChess str
       loop
  where loop = do minput <- getInputLine prompt
                  case minput of
                    Nothing -> return ()
                    Just "" -> loop
                    Just "start" -> game
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
