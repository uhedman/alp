module Main where
import PPrint ( pp )
import Parse ( parseSet, runP, P, program )
import MonadAC
    ( getEpoch,
      getIsHalted,
      printAC,
      runAC,
      MonadIO(liftIO),
      MonadAC,
      MonadError(throwError),
      MonadTrans(lift), setBoard, clearBoard )
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)
import Eval ( execSet, execStep )
import System.Environment (getArgs)
import Lang ( Table )
import Control.Exception ( IOException, catch )
import Data.Char (isSpace)
import System.IO (hPutStrLn, stderr)
import Errors (Error(ParseErr))
import Global (emptyTable)
import Data.List (dropWhileEnd)
import Control.Concurrent (threadDelay)

prompt :: String
prompt = "gameoflife> "

main :: IO ()
main = do
  args <- getArgs
  runAC (runInputT defaultSettings setup) emptyTable
  return ()

setup :: (MonadAC m, MonadMask m) => InputT m ()
setup = do
       liftIO $ putStrLn $ unlines
        [ "   ______                     ____  ______    _ ____   "
        , "  / ____/___ _____ ___  ___  / __ \\/ __/ /   (_) __/__"
        , " / / __/ __ `/ __ `__ \\/ _ \\/ / / / /_/ /   / / /_/ _ \\"
        , "/ /_/ / /_/ / / / / / /  __/ /_/ / __/ /___/ / __/  __/"
        , "\\____/\\__,_/_/ /_/ /_/\\___/\\____/_/ /_____/_/_/  \\___/ "
        , "==============================================================="
        , "Comandos disponibles:"
        , "  start            - Inicia la simulación indefinida"
        , "  step             - Simula una época"
        , "  clear            - Limpia el tablero"
        , "  R/K x y          - Revive o mata una célula en (x, y)"
        , "  resize r c       - Cambia las dimensiones del tablero a r x c"
        , "  load archivo     - Carga un tablero"
        , "  quit             - Salir del juego"
        , "==============================================================="
        , "¡Diviértete jugando!"
        ]
       printTable
       loop
  where loop = do minput <- getInputLine prompt
                  case minput of
                    Nothing -> return ()
                    Just "" -> loop
                    Just "quit" -> return ()
                    Just "start" -> inf
                    Just "clear" -> do lift clearBoard
                                       printTable
                                       loop
                    Just "step" -> do step
                                      loop
                    Just cmd -> if take 5 cmd == "load "
                                then do
                                  let fileName = drop 5 cmd
                                  liftIO $ putStrLn $ "Cargando archivo: " ++ fileName
                                  result <- lift $ loadFile fileName
                                  case result of
                                    Left err -> liftIO $ hPutStrLn stderr $ "Error al cargar el archivo: " ++ err
                                    Right board -> do
                                      liftIO $ putStrLn $ "Archivo " ++ fileName ++ " cargado."
                                      lift $ setBoard board
                                      printTable
                                  loop
                                else do
                                  commSet cmd
                                  printTable
                                  loop
        inf = do liftIO $ putStr "\ESC[2J"
                 step
                 h <- lift getIsHalted
                 if h then do lift $ printAC "Halted table"
                              loop
                      else do liftIO $ threadDelay 80000
                              inf
        step = do lift execStep
                  epoch <- lift getEpoch
                  lift $ printAC $ "Epoch: " ++ show epoch
                  printTable
        printTable = do str <- lift pp
                        lift $ printAC str

commSet :: MonadAC m => String -> InputT m ()
commSet phrase = case parseSet phrase of
                Left e -> lift $ printAC $ "Error: " ++ show e
                Right a -> lift $ execSet a

loadFile :: MonadAC m => FilePath -> m (Either String (Table, Int, Int))
loadFile f = do
    let filename = dropWhileEnd isSpace f
    content <- liftIO $ catch (Right <$> readFile filename)
                               (\e -> return $ Left $ "No se pudo abrir el archivo " ++ filename ++ ": " ++ show (e :: IOException))
    case content of
      Left err -> return $ Left err
      Right x -> case runP program x filename of
                   Left e -> return $ Left $ "Error de parseo en " ++ filename ++ ": " ++ show e
                   Right r -> return $ Right r

parseIO :: MonadAC m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

-- Extraer el nombre del archivo
getFileName :: [String] -> Maybe String
getFileName [] = Nothing
getFileName ("--file":f:_) = Just f
getFileName ("-f":f:_) = Just f
getFileName (_:xs) = getFileName xs
