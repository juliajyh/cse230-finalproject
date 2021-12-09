module Backend(test, psCmd, runCmd) where

import System.Process
import Text.JSON
import Text.Parsec
import Text.Parsec.String
import Text.JSON.Parsec (p_object)
import Text.JSON.Types (JSString(JSONString))
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))

-- testing
test :: IO ()
test = runTest testPs

runTest :: (Show a) => IO (Either String a) -> IO ()
runTest f = do
    s <- f
    case s of
      Left ex -> putStr ex
      Right v -> putStr $ show v

-- ps Command
-- ("ID", "Names", "Image", "State", "Status", "Ports")
psCmd :: IO (Either String [(String, String, String, String, String, String)])
psCmd = do
    j <- runDockerPs
    case j of
      Left ex -> return $ Left ex
      Right jsonObjs ->
          return $ Right $ map getPsEntry jsonObjs

testPs :: IO (Either String [(String, String, String, String, String, String)])
testPs = psCmd

getPsEntry :: [(String, JSValue)] -> (String, String, String, String, String, String)
getPsEntry jsonObj =
    (id, names, image, state, status, ports)
    where
        id = take 12 $ getEntry "ID" jsonObj
        names = getEntry "Names" jsonObj
        image = getEntry "Image" jsonObj
        state = getEntry "State" jsonObj
        status = getEntry "Status" jsonObj
        ports = getEntry "Ports" jsonObj

-- Shell: docker ps
runDockerPs :: IO (Either String [[(String, JSValue)]])
runDockerPs = do
    j <- execShell "docker" ["ps", "--no-trunc", "--format", "'{{json .}}'"]
    case j of
        Left ex -> return $ Left ex
        Right jsonStr -> return $ parseJson jsonStr


-- run Command
-- (Image, Name, [(Host_Path, Guest_Path)], [(Host_Port, Guest_Port)], Command, Attach, Volatile, Daemon])
runCmd :: (String, String, [(String, String)], [(String, String)], String, Bool, Bool, Bool) -> IO (Either String String)
runCmd args = execShell "docker" ("run" : runArgs args)


testRun :: IO (Either String String)
testRun =
    runCmd ("hcyang99/snps16", "cad", [("/home/hcyang/Documents/source/repos/patternet", "/mnt/repos/patternet"), ("/home/hcyang/Documents/source/env/snps16/.vscode-server/", "/root/.vscode-server")], [("80", "80"), ("443", "443")], "uname -r", False, True, False)
    -- runCmd ("ubuntu", "frp", [("/home/hcyang/Documents/source/env/frp/", "/mnt/")], [("35600", "35600"), ("35622", "35622")], "/mnt/frps -c /mnt/frps.ini", False, True, True)


runArgs :: (String, String, [(String, String)], [(String, String)], String, Bool, Bool, Bool) -> [String]
runArgs (img, name, mounts, ports, cmd, attach, volatile, daemon) =
    runAttachArgs attach ++ runDaemonArgs daemon ++ runVolatileArgs volatile ++ runNameArgs name ++ runPathsArgs mounts ++ runPortsArgs ports ++ [img] ++ commands
    where
        commands = case parseFromString Backend.words cmd of
          Left _ -> []
          Right ss -> ss


runNameArgs :: String -> [String]
runNameArgs s
    | length s == 0 = []
    | otherwise = ["--name", s]

-- [(Host_Path, Guest_Path)]
runPathsArgs :: [(String, String)] -> [String]
runPathsArgs [] = []
runPathsArgs (x:xs) =
    case x of
        (hp, gp) -> "-v" : (hp ++ ":" ++ gp) : runPathsArgs xs

-- [(Host_Port, Guest_Port)]
runPortsArgs :: [(String, String)] -> [String]
runPortsArgs [] = []
runPortsArgs (x:xs) =
    case x of
        (hp, gp) -> "-p" : (hp ++ ":" ++ gp) : runPortsArgs xs

runAttachArgs :: Bool -> [String]
runAttachArgs p = ["-it" | p]

runVolatileArgs :: Bool -> [String]
runVolatileArgs p = ["--rm" | p]

runDaemonArgs :: Bool -> [String]
runDaemonArgs p = ["-d" | p]

-- stop Command
stopCmd :: String -> IO (Either String String)
stopCmd name = execShell "docker" ["stop", name]



-- Parser Related

parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p = runParser p () ""

isNotChar :: Char -> Char -> Bool
isNotChar c1 c2 = c2 /= c1

isNotChars :: [Char] -> Char -> Bool
isNotChars s c = case s of
    []      -> True
    x:xs    -> isNotChar x c && isNotChars xs c

isNotEndOfLine :: Char -> Bool
isNotEndOfLine = isNotChars ['\r', '\n']

notEndOfLine :: Parser Char
notEndOfLine = satisfy isNotEndOfLine

line :: Parser String
line = do
    v <- many notEndOfLine
    _ <- endOfLine
    return v

word ::Parser String
word = do
    _ <- spaces
    s <- many1 $ satisfy $ isNotChar ' '
    _ <- spaces
    return s

words :: Parser [String]
words = many word

-- >>> parseFromString Backend.words "/mnt/frps -c /mnt/frps.ini"
-- Right ["/mnt/frps","-c","/mnt/frps.ini"]


psline :: Parser [(String, JSValue)]
psline = do
    _ <- spaces
    _ <- char '\''
    s <- p_object
    _ <- char '\''
    _ <- spaces
    return s

psParser :: Parser [[(String, JSValue)]]
psParser = do
    many psline

-- JSON Related

parseJson :: String -> Either String [[(String, JSValue)]]
parseJson jsonStr =
    case parseFromString psParser jsonStr of
        Left _ -> undefined
        Right jsonObj -> Right jsonObj


getEntry :: String -> [(String, JSValue)] -> String
getEntry n l =
    case lookup n l of
        Nothing -> ""
        Just v ->
            case v of
                JSString (JSONString s) -> s
                _ -> undefined

-- Shell
execShell :: String -> [String] -> IO (Either String String)
execShell file args = do
    (code, out, err) <- readProcessWithExitCode file args ""
    case code of
      ExitSuccess -> return $ Right out
      ExitFailure n -> case parseFromString line err of
        Left _ -> return $ Left ("Code " ++ show n)
        Right s -> return $ Left ("Code " ++ show n ++ ": " ++ s)
