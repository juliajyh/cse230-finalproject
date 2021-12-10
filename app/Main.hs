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
        (Left s, _, _, _) -> 
