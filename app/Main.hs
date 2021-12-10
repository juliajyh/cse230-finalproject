-- {-# LANGUAGE OverloadedStrings #-}
import ExecPull(execPull)
import ExecStop (execStop)
import ExecContainerRm (execContainerRm)
import ExecImageRm (execImageRm)
import ResultDialog(resultDialog)
import UIDockerRun(uiDockerRun)
import UIDockerPull (uiDockerPull)
import UIDockerImageRm (uiDockerImageRm)
import UIDockerContainerRm (uiDockerContainerRm)
import UIDockerStop (uiDockerStop)
import UIDockerExec (uiDockerExec)
import ExecRun (execRun)
import ExecStart (execStart)
import ExecExec (execExec)
import UIDockerStart (uiDockerStart)
import MainMenu(mainMenu)
import Backend

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
                DockerImagePull -> runDockerImagePull
                DockerImageRm  -> runDockerImageRm
                DockerRun -> runDockerRun
                DockerRm -> runDockerRm
                DockerExec -> runDockerExec
                DockerStart -> runDockerStart
                DockerStop -> runDockerStop
                DockerHalt -> return ()

runDockerImagePull :: IO ()
runDockerImagePull = undefined 

runDockerImageRm :: IO ()
runDockerImageRm = undefined 

runDockerRun :: IO ()
runDockerRun = undefined 

runDockerRm :: IO ()
runDockerRm = undefined 

runDockerExec :: IO ()
runDockerExec = undefined 

runDockerStart :: IO ()
runDockerStart = undefined

runDockerStop :: IO ()
runDockerStop = undefined 

