module Main where
import PPrint ( pp )
import Parse (parse)
import MonadChess ( when, MonadIO(liftIO), MonadTrans(lift), MonadState(get), MonadChess, setInter, printChess, runChess )
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Global ( GlEnv(inter) )
import Control.Monad.Catch (MonadMask)

prompt :: String
prompt = "chess> "

main :: IO ()
main = do runChess (runInputT defaultSettings game)
          return ()

game :: (MonadChess m, MonadMask m) => InputT m ()
game = do
       lift $ setInter True
       s <- lift get
       when (inter s) $ liftIO $ putStrLn "Juego de ajedrez."
       loop
  where loop = do str <- lift pp
                  lift $ printChess str
                  minput <- getInputLine prompt
                  case minput of
                    Nothing -> return ()
                    Just "" -> loop
                    Just x -> do exec x
                                 loop

exec :: MonadChess m => String -> InputT m ()
exec phrase = do case parse phrase of
                   Left e -> lift $ printChess $ "Error: " ++ show e
                   Right mv -> lift $ printChess (show mv)
                 return ()