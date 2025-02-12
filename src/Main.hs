module Main where
import PPrint ( pp )
import Parse ( parsePut, parseResize, runP, P, program )
import MonadAC
    ( getEpoch,
      getIsHalted,
      printAC,
      runAC,
      MonadIO(liftIO),
      MonadAC,
      MonadError(throwError),
      MonadTrans(lift), clearBoard, when, catchErrors, setBoard, setGame, setBoundary, getGame, resize, setLastFile, getLastFile, getBoundary, unless )
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)
import Eval ( execSet, execStep )
import System.Environment (getArgs)
import Lang ( Table, Pos, Boundary (Closed, Open, Periodic, Reflective), Game )
import Control.Exception ( IOException, catch )
import Data.Char (isSpace)
import Errors (Error(ParseErr))
import Data.List (dropWhileEnd, isPrefixOf, intercalate)
import Control.Concurrent (threadDelay, forkIO, killThread)
import System.IO (hPutStrLn, stderr, hSetBuffering, stdin, BufferMode (NoBuffering))
import Games (gameOfLife, langton, rule18, seeds, briansBrain, dayAndNight, rule30, rule90, rule184, ghmodel, sand, maze, mazectric) 
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

prompt :: String
prompt = "ac> "

main :: IO ()
main = do
  args <- getArgs
  let settings = parseArgs args
  runAC (runInputT defaultSettings setup) settings
  return ()

setup :: (MonadAC m, MonadMask m) => InputT m ()
setup = do
       f <- lift getLastFile
       when (f /= "") (do b <- lift $ loadFile f
                          unless b (lift $ setLastFile ""))
       liftIO $ putStrLn $ unlines
        [ "      .o.                       .                                             .                      "
        , "     .888.                    .o8                                           .o8                      "
        , "    .8\"888.     oooo  oooo  .o888oo  .ooooo.  ooo. .oo.  .oo.    .oooo.   .o888oo  .oooo.    .oooo.o "
        , "   .8' `888.    `888  `888    888   d88' `88b `888P\"Y88bP\"Y88b  `P  )88b    888   `P  )88b  d88(  \"8 "
        , "  .88ooo8888.    888   888    888   888   888  888   888   888   .oP\"888    888    .oP\"888  `\"Y88b.  "
        , " .8'     `888.   888   888    888 . 888   888  888   888   888  d8(  888    888 . d8(  888  o.  )88b "
        , "o88o     o8888o  `V88V\"V8P'   \"888\" `Y8bod8P' o888o o888o o888o `Y888\"\"8o   \"888\" `Y888\"\"8o 8\"\"888P' "
        , "                                                                                                     "
        , "                                                                                                     "
        , "  .oooooo.             oooo              oooo                                                        "
        , " d8P'  `Y8b            `888              `888                                                        "
        , "888           .ooooo.   888  oooo  oooo   888   .oooo.   oooo d8b  .ooooo.   .oooo.o                 "
        , "888          d88' `88b  888  `888  `888   888  `P  )88b  `888\"\"8P d88' `88b d88(  \"8                 "
        , "888          888ooo888  888   888   888   888   .oP\"888   888     888ooo888 `\"Y88b.                  "
        , "`88b    ooo  888    .o  888   888   888   888  d8(  888   888     888    .o o.  )88b                 "
        , " `Y8bood8P'  `Y8bod8P' o888o  `V88V\"V8P' o888o `Y888\"\"8o d888b    `Y8bod8P' 8\"\"888P'                 "
        , "                                                                                                     "
        , "======================================================================================================"
        , "Comandos disponibles:"
        , "  start               - Inicia la simulación indefinida"
        , "  step                - Simula una época"
        , "  clear               - Limpia el tablero"
        , "  reset               - Reinicia el tablero"
        , "  put c x y           - Escribe un carácter c en (x, y)"
        , "  resize r c          - Cambia las dimensiones del tablero a r x c"
        , "  load archivo        - Carga un tablero"
        , "  set automaton tipo  - Selecciona el tipo de autómata celular:"
        , "    - gameOfLife (default)"
        , "    - briansBrain"
        , "    - dayAndNight"
        , "    - ghmodel"
        , "    - langton"
        , "    - maze"
        , "    - mazectric"
        , "    - rule18"
        , "    - rule30"
        , "    - rule90"
        , "    - rule184"
        , "    - sand"
        , "    - seeds"
        , "  set boundary tipo   - Selecciona el tipo de frontera:"
        , "    - open (default)"
        , "    - closed"
        , "    - periodic"
        , "    - reflective"
        , "  quit                - Salir del juego"
        , "======================================================================================================"
        , "¡Diviértete jugando!"
        ]
       lift printTable
       loop
       where loop = do minput <- getInputLine prompt
                       case minput of
                         Nothing -> return ()
                         Just "" -> loop
                         Just x -> do
                                  c <- liftIO $ interpretCommand x
                                  b <- lift $ catchErrors $ handleCommand c
                                  maybe loop (`when` loop) b

data Command = Start
             | Step
             | Clear
             | Reset
             | Put Char Pos
             | Resize Int Int
             | Load FilePath
             | SetAutomaton Game
             | SetBoundary Boundary
             | Help
             | Quit
             | Error String
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  do  let  (cmd,t')  =  break isSpace x
              t         =  dropWhile isSpace t'
         --  find matching commands
         let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
         case matching of
           []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                       return Noop
           [Cmd _ _ f _]
               ->  do  return (f t)
           _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   intercalate ", " ([ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                       return Noop


commands :: [InteractiveCommand]
commands 
  =  [ Cmd ["load"]        "<file>"  Load           "Cargar un programa desde un archivo",
       Cmd ["quit"]        ""        (const Quit)   "Salir del intérprete",
       Cmd ["start"]       ""        (const Start)  "Inicia la simulacion indefinidamente",
       Cmd ["step"]        ""        (const Step)   "Simula una epoca",
       Cmd ["clear"]       ""        (const Clear)  "Salir del intérprete",
       Cmd ["put"]         "<char> <int> <int>" commPut "Escribe un carácter c en (x, y)",
       Cmd ["reset"]       ""        (const Reset)   "Reinicia el tablero",
       Cmd ["resize"]      "<int> <int>" commResize "Cambia las dimensiones del tablero a r x c",
       Cmd ["setGame"]     "<game>"  commGame "Selecciona el tipo de autómata celular.",
       Cmd ["setBound"]    "<bound>" commBound  "Selecciona el tipo de frontera.",
       Cmd ["help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = intercalate ", " (map (++ if null a then "" else " " ++ a) c)
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadAC m => Command -> m Bool
handleCommand cmd = do
   case cmd of
       Quit   -> return False
       Noop   -> return True
       Help   -> printAC (helpTxt commands) >> return True
       Start  -> start >> return True
       Step   -> step >> return True
       Clear  -> clearBoard >> printTable >> return True
       Reset -> do f <- getLastFile
                   if f == ""
                   then printAC "Todavia no se cargo ningun archivo" >> return True
                   else loadFile f >>= \b -> when b printTable >> return True
       Load f -> loadFile f >>= \b -> when b printTable >> return True
       Put c p -> do (lang, _, g) <- getGame
                     if c `elem` lang
                     then execSet (p,c) >> printTable >> return True
                     else printAC (c : " no es un caracter aceptado en el lenguaje de " ++ g ++ ". Los caracteres aceptados son:" ++ show lang) >> return True
       Resize r c -> resize r c >> printTable >> return True
       SetAutomaton g -> setGame g >> clearBoard >> printTable >> return True
       SetBoundary b -> setBoundary b >> return True
       Error e -> printAC e >> return True

loadFile :: MonadAC m => FilePath -> m Bool
loadFile fileName = do result <- openFile fileName
                       case result of
                        Left err -> liftIO (hPutStrLn stderr $ "Error al cargar el archivo: " ++ err) >> return False
                        Right board -> do
                          printAC $ "Archivo " ++ fileName ++ " cargado."
                          setBoard board
                          setLastFile fileName
                          return True

openFile :: MonadAC m => FilePath -> m (Either String (Table, Int, Int))
openFile f = do
    let filename = dropWhileEnd isSpace f
    content <- liftIO $ catch (Right <$> readFile filename)
                               (\e -> return $ Left $ "No se pudo abrir el archivo " ++ filename ++ ": " ++ show (e :: IOException))
    (lang,_,_) <- getGame
    case content of
      Left err -> return $ Left err
      Right x -> case runP (program lang) x filename of
                   Left e -> return $ Left $ "Error de parseo en " ++ filename ++ ": " ++ show e
                   Right r -> return $ Right r

parseIO :: MonadAC m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

{-# NOINLINE stopSignalRef #-}
stopSignalRef :: IORef Bool
stopSignalRef = unsafePerformIO $ newIORef False

getStopSignal :: IO Bool
getStopSignal = readIORef stopSignalRef

setStopSignal :: Bool -> IO ()
setStopSignal = writeIORef stopSignalRef

listenForStop :: IO ()
listenForStop = do
  c <- getChar
  if c == 's'
    then setStopSignal True
    else listenForStop

start :: MonadAC m => m ()
start = do
  liftIO $ setStopSignal False 
  liftIO $ hSetBuffering stdin NoBuffering 
  tid <- liftIO $ forkIO listenForStop 
  runLoop tid
  
  where
    runLoop tid = do 
      printAC "\ESC[2J"
      step
      h <- getIsHalted
      stopSignal <- liftIO getStopSignal
      if h || stopSignal
        then do
          printAC "Simulación detenida."
          liftIO $ killThread tid
        else do
          liftIO $ threadDelay 80000
          runLoop tid

step :: MonadAC m => m ()
step = do execStep
          epoch <- getEpoch
          printAC $ "Epoch: " ++ show epoch
          printTable

printTable :: MonadAC m => m ()
printTable = do b <- getBoundary
                (lang, _, g) <- getGame
                printAC $ "Juego: " ++ g ++ ". Frontera: " ++ show b
                str <- pp
                printAC str

commPut :: String -> Command
commPut phrase = case parsePut phrase of
                Left e -> Error $ "Error: " ++ show e
                Right (pos, c) -> Put c pos

commResize :: String -> Command
commResize phrase = case parseResize phrase of
                Left e -> Error $ "Error: " ++ show e
                Right (x, y) -> Resize x y

commGame :: String -> Command
commGame s = case parseAutomaton s of
  Just g -> SetAutomaton g
  Nothing -> Error $ "Automata desconocido: " ++ s

commBound :: String -> Command
commBound s = case parseBoundary s of
  Just b -> SetBoundary b
  Nothing -> Error $ "Frontera desconocida: " ++ s

parseArgs :: [String] -> (Maybe FilePath, Maybe Game, Maybe Boundary)
parseArgs [] = (Nothing, Nothing, Nothing)
parseArgs ("-f":f:xs) = let (_, g, b) = parseArgs xs in (Just f, g, b)
parseArgs ("-g":g:xs) = let (f, _, b) = parseArgs xs in (f, parseAutomaton g, b)
parseArgs ("-b":b:xs) = let (f, g, _) = parseArgs xs in (f, g, parseBoundary b)
parseArgs (_:xs) = parseArgs xs

parseAutomaton :: String -> Maybe Game
parseAutomaton g = do
  case g of
    "gameOfLife" -> Just gameOfLife
    "briansBrain" -> Just briansBrain
    "dayAndNight" -> Just dayAndNight
    "ghmodel" -> Just ghmodel
    "langton" -> Just langton
    "maze" -> Just maze
    "mazectric" -> Just mazectric
    "rule18" -> Just rule18
    "rule30" -> Just rule30
    "rule90" -> Just rule90
    "rule184" -> Just rule184
    "sand" -> Just sand
    "seeds" -> Just seeds
    _ -> Nothing

parseBoundary :: String -> Maybe Boundary
parseBoundary b = do
  case b of
    "open" -> Just Open
    "closed" -> Just Closed
    "periodic" -> Just Periodic
    "reflective" -> Just Reflective
    _ -> Nothing