module Main where
import PPrint ( pp )
import Parse (parse)
import MonadChess ( when, MonadIO(liftIO), MonadTrans(lift), MonadState(get), MonadChess, setInter, printChess, runChess )
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Global ( GlEnv(inter) )
import Control.Monad.Catch (MonadMask)
import Eval ( execMove )

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

exec :: MonadChess m => String -> InputT m ()
exec phrase = case parse phrase of
                Left e -> lift $ printChess $ "Error: " ++ show e
                Right mv -> do lift $ printChess (show mv)
                               lift $ execMove mv
