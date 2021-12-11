-- {-# LANGUAGE OverloadedStrings #-}
import ExecPull(execPull)
import ExecStop (execStop)
import ExecContainerRm (execContainerRm)
import ExecImageRm (execImageRm)
import ResultDialog(resultDialog)
import qualified UIDockerRun as R
import qualified UIDockerPull (uiDockerPull, DockerImageInfo, initialDockerImageInfo, getImage, getCancel)
import qualified UIDockerImageRm (uiDockerImageRm, DockerImageInfo, initialDockerImageInfo, getImage, getCancel)
import qualified UIDockerContainerRm (uiDockerContainerRm, initialDockerContainerInfo, DockerContainerInfo, getContainer, getCancel)
import qualified UIDockerStop (uiDockerStop, initialDockerContainerInfo, DockerContainerInfo, getContainer, getCancel)
import qualified UIDockerExec (uiDockerExec, initialDockerExecInfo, DockerExecInfo, getContainer, getCommand, getCancel)
import qualified UIDockerStart (uiDockerStart, initialDockerContainerInfo, DockerContainerInfo, getContainer, getCancel)
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
                DockerRun -> runDockerRun R.initialDockerRunInfo
                DockerRm -> runDockerRm UIDockerContainerRm.initialDockerContainerInfo
                DockerExec -> runDockerExec UIDockerExec.initialDockerExecInfo
                DockerStart -> runDockerStart UIDockerStart.initialDockerContainerInfo
                DockerStop -> runDockerStop UIDockerStop.initialDockerContainerInfo
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

runDockerExec :: UIDockerExec.DockerExecInfo -> IO ()
runDockerExec oldInfo = do 
    newInfo <- UIDockerExec.uiDockerExec oldInfo 
    case UIDockerExec.getCancel newInfo of 
        True -> do 
            resultDialog "Execute Command" "Cancelled"
            runMainMenu
        False -> do 
            res <- execExec (unpack $ UIDockerExec.getContainer newInfo) (unpack $ UIDockerExec.getCommand newInfo)
            resultDialog "Execute Command" res 
            runMainMenu

runDockerStart :: UIDockerStart.DockerContainerInfo -> IO ()
runDockerStart oldInfo = do 
    newInfo <- UIDockerStart.uiDockerStart oldInfo 
    case UIDockerStart.getCancel newInfo of 
        True -> do 
            resultDialog "Start Container" "Cancelled"
            runMainMenu
        False -> do 
            res <- execStart $ unpack $ UIDockerStart.getContainer newInfo
            resultDialog "Start Container" res 
            runMainMenu

runDockerStop :: UIDockerStop.DockerContainerInfo -> IO ()
runDockerStop oldInfo = do 
    newInfo <- UIDockerStop.uiDockerStop oldInfo 
    case UIDockerStop.getCancel newInfo of 
        True -> do 
            resultDialog "Stop Container" "Cancelled"
            runMainMenu
        False -> do 
            res <- execStop $ unpack $ UIDockerStop.getContainer newInfo 
            resultDialog "Stop Container" res 
            runMainMenu

runDockerRun :: R.DockerRunInfo -> IO ()
runDockerRun oldInfo = do 
    newInfo <- R.uiDockerRun oldInfo 
    case R.getCancel newInfo of 
        True -> do 
            resultDialog "Run Container" "Cancelled"
            runMainMenu
        False -> do 
            let 
                image = unpack $ R.getImage newInfo 
                name = unpack $ R.getName newInfo 
                ports = parsePortMaps $ unpack $ R.getPorts newInfo 
                mounts = parseMountMaps $ unpack $ R.getMounts newInfo 
                command = unpack $ R.getCommand newInfo 
                attach = R.getAttach newInfo 
                volatile = R.getVolatile newInfo
                daemon = R.getDaemon newInfo 
                cancel = R.getCancel newInfo 
            case cancel of 
                True -> do 
                    resultDialog "Run Container" "Cancelled"
                    runMainMenu
                False -> 
                    case (ports, mounts) of 
                        (Left ex, _) -> do 
                            resultDialog "Run Container" ex 
                            runDockerRun newInfo 
                        (_, Left ex) -> do 
                            resultDialog "Run Container" ex 
                            runDockerRun newInfo 
                        (Right p, Right m) -> do 
                            res <- execRun (image, name, m, p, command, False, volatile, daemon)
                            case res of 
                                Left ex -> do 
                                    resultDialog "Run Container" ex 
                                    runDockerRun newInfo 
                                Right s -> do 
                                    resultDialog "Run Container" s 
                                    runMainMenu


