{-# LANGUAGE OverloadedStrings #-}
import TestBrickStates(testBrickStates)
import TestThemes(testThemes)
import ExecPull(testExecPull)
import ExecStop (testExecStop)
import ExecContainerRm (testExecContainerRm)
import ExecImageRm (testExecImageRm)
import ResultDialog(testResultDialog)

main :: IO ()
main = testExecPull 