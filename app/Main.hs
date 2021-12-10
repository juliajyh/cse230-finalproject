{-# LANGUAGE OverloadedStrings #-}
import TestBrickStates(testBrickStates)
import TestThemes(testThemes)
import ExecPull(testExecPull)

main :: IO ()
main = testExecPull