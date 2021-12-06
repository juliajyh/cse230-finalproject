module Lib where

import System.Process
import Text.JSON
import Text.Parsec
import Text.Parsec.String
import Text.JSON.Parsec (p_object)
import Text.JSON.Types (JSString(JSONString))
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))

test :: IO ()
test = do
    s <- psCmd
    case s of 
      Left ex -> putStr ex
      Right v -> putStr $ show v

-- ps Command
-- ("Names", "Image", "State", "Status", "Ports")
psCmd :: IO (Either String [(String, String, String, String, String)])
psCmd = do 
    j <- getJsonObj
    case j of 
      Left ex -> return $ Left ex
      Right jsonObjs -> 
          return $ Right $ map getPsEntry jsonObjs

getPsEntry :: [(String, JSValue)] -> (String, String, String, String, String)
getPsEntry jsonObj = 
    (names, image, state, status, ports)
    where 
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
