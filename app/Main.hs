-- {-# LANGUAGE OverloadedStrings #-}
import ExecPull(execPull)
import ExecStop (execStop)
import ExecContainerRm (execContainerRm)
import ExecImageRm (execImageRm)
import ResultDialog(resultDialog)
import UIDockerRun(uiDockerRun)
import qualified UIDockerPull (uiDockerPull, DockerImageInfo, initialDockerImageInfo, getImage, getCancel)
import qualified UIDockerImageRm (uiDockerImageRm, DockerImageInfo, initialDockerImageInfo, getImage, getCancel)
import qualified UIDockerContainerRm (uiDockerContainerRm, initialDockerContainerInfo, DockerContainerInfo, getContainer, getCancel)
import UIDockerStop (uiDockerStop)
import UIDockerExec (uiDockerExec)
import UIDockerStart (uiDockerStart)
import ExecRun (execRun)
import ExecStart (execStart)
import ExecExec (execExec)
import MainMenu(mainMenu)
import Backend
import Data.Text(unpack)

main :: IO ()
main = runMainMenu

runMainMenu :: IO ()
runMainMenu = do 
    containers <- psCmd
    images <- imageLsCmd
    volumes <- volumeLsCmd
    networks <- networkLsCmd
    case (containers, images, volumes, networks) of 
        (Left ex, _, _, _) -> resultDialog "Error" ex
        (_, Left ex, _, _) -> resultDialog "Error" ex
        (_, _, Left ex, _) -> resultDialog "Error" ex
        (_, _, _, Left ex) -> resultDialog "Error" ex
        (Right c, Right i, Right v, Right n) -> do 
            cmd <- mainMenu c i v n
            case cmd of 
                DockerMain -> runMainMenu
                DockerImagePull -> runDockerImagePull UIDockerPull.initialDockerImageInfo
                DockerImageRm  -> runDockerImageRm UIDockerImageRm.initialDockerImageInfo
                DockerRun -> runDockerRun
                DockerRm -> runDockerRm UIDockerContainerRm.initialDockerContainerInfo
                DockerExec -> runDockerExec
                DockerStart -> runDockerStart
                DockerStop -> runDockerStop
                DockerHalt -> return ()

runDockerImagePull :: UIDockerPull.DockerImageInfo -> IO ()
runDockerImagePull oldInfo = do 
    newInfo <- UIDockerPull.uiDockerPull oldInfo 
    case UIDockerPull.getCancel newInfo of
        True -> do 
            resultDialog "Pull Image" "Cancelled"
            runMainMenu
        False -> do 
            res <- execPull $ unpack $ UIDockerPull.getImage newInfo
            resultDialog "Pull Image" res 
            runMainMenu

runDockerImageRm :: UIDockerImageRm.DockerImageInfo -> IO ()
runDockerImageRm oldInfo = do 
    newInfo <- UIDockerImageRm.uiDockerImageRm oldInfo
    case UIDockerImageRm.getCancel newInfo of
        True -> do 
            resultDialog "Remove Image" "Cancelled"
            runMainMenu
        False -> do 
            res <- execImageRm $ unpack $ UIDockerImageRm.getImage newInfo
            resultDialog "Remove Image" res 
            runMainMenu

runDockerRm :: UIDockerContainerRm.DockerContainerInfo -> IO ()
runDockerRm oldInfo = do 
    newInfo <- UIDockerContainerRm.uiDockerContainerRm oldInfo 
    case UIDockerContainerRm.getCancel newInfo of 
        True -> do 
            resultDialog "Remove Container" "Cancelled"
            runMainMenu
        False -> do 
            res <- execContainerRm $ unpack $ UIDockerContainerRm.getContainer newInfo
            resultDialog "Remove Container" res 
            runMainMenu

runDockerExec :: IO ()
runDockerExec = undefined 

runDockerStart :: IO ()
runDockerStart = undefined

runDockerStop :: IO ()
runDockerStop = undefined 

runDockerRun :: IO ()
runDockerRun = undefined 

