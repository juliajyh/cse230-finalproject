{-# LANGUAGE OverloadedStrings #-}
import TestBrickStates(testBrickStates)
import TestThemes(testThemes)
import ExecPull(testExecPull)
import ExecStop (testExecStop)
import ExecContainerRm (testExecContainerRm)
import ExecImageRm (testExecImageRm)
import ResultDialog(testResultDialog)
import UIDockerRun(testUIDockerRun)
import UIDockerPull (testUIDockerPull)
import UIDockerImageRm (testUIDockerImageRm)
import UIDockerContainerRm (testUIDockerContainerRm)
import UIDockerStop (testUIDockerStop)
import UIDockerExec (testUIDockerExec)

main :: IO ()
main = testUIDockerExec