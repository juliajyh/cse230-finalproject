module Backend(test, psCmd) where

import System.Process
import Text.JSON
import Text.Parsec
import Text.Parsec.String
import Text.JSON.Parsec (p_object)
import Text.JSON.Types (JSString(JSONString))
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))

test :: IO ()
test = testRun

-- ps Command
-- ("ID", "Names", "Image", "State", "Status", "Ports")
psCmd :: IO (Either String [(String, String, String, String, String, String)])
psCmd = do
    j <- getJsonObj
    case j of
      Left ex -> return $ Left ex
      Right jsonObjs ->
          return $ Right $ map getPsEntry jsonObjs

testPs :: IO ()
testPs = do
    s <- psCmd
    case s of
      Left ex -> putStr ex
      Right v -> putStr $ show v

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
runDockerPs :: IO (Either String String)
runDockerPs = do
    (code, out, err) <- readProcessWithExitCode "docker" ["ps", "--no-trunc", "--format", "'{{json .}}'"] ""
    case code of
      ExitSuccess -> return $ Right out
      ExitFailure n -> case parseFromString line err of
        Left _ -> return $ Left ("Code " ++ show n)
        Right s -> return $ Left ("Code " ++ show n ++ ": " ++ s)


-- run Command
-- (Image, Name, [(Host_Path, Guest_Path)], [(Host_Port, Guest_Port)], Command, Attach, Volatile])
runCmd :: (String, Maybe String, [(String, String)], [(String, String)], String, Bool, Bool) -> IO (Either String String)
runCmd args@(img, name, mounts, ports, cmd, attach, volatile) = do
    (code, out, err) <- readProcessWithExitCode "docker" ("run" : runArgs args) ""
    case code of
      ExitSuccess -> return $ Right out
      ExitFailure n -> case parseFromString line err of
        Left _ -> return $ Left ("Code " ++ show n)
        Right s -> return $ Left ("Code " ++ show n ++ ": " ++ s)

testRun :: IO ()
testRun = do 
    s <- runCmd ("hcyang99/snps16", Just "cad", [("/home/hcyang/Documents/source/repos/patternet", "/mnt/repos/patternet"), ("/home/hcyang/Documents/source/env/snps16/.vscode-server/", "/root/.vscode-server")], [("80", "80"), ("443", "443")], "uname -r", False, True)
    case s of
      Left ex -> putStr ex
      Right v -> putStr $ show v

runArgs :: (String, Maybe String, [(String, String)], [(String, String)], String, Bool, Bool) -> [String]
runArgs (img, name, mounts, ports, cmd, attach, volatile) =
    runAttachArgs attach ++ runVolatileArgs volatile ++ runNameArgs name ++ runPathsArgs mounts ++ runPortsArgs ports ++ [img] ++ commands
    where
        commands = case parseFromString Backend.words cmd of
          Left _ -> []
          Right ss -> ss


runNameArgs :: Maybe String -> [String]
runNameArgs s =
    case s of
        Nothing -> []
        Just name -> ["--name", name]

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

getJsonObj :: IO (Either String [[(String, JSValue)]])
getJsonObj = do
    encoded <- runDockerPs
    case encoded of
      Left ex -> return $ Left ex
      Right jsonStr ->
        case parseFromString psParser jsonStr of
            Left _ -> undefined
            Right jsonObj -> do
                return $ Right jsonObj

getEntry :: String -> [(String, JSValue)] -> String
getEntry n l =
    case lookup n l of
        Nothing -> ""
        Just v ->
            case v of
                JSString (JSONString s) -> s
                _ -> undefined

getEntries :: String -> [[(String, JSValue)]] -> [String]
getEntries name = map (getEntry name)



getNames :: [[(String, JSValue)]] -> [String]
getNames = map (getEntry "Names")
