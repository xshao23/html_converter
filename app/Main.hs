module Main where

import IOHandler ( convert)
import Tests ( run )

main :: IO ()
main = convert

runAllTests :: IO ()
runAllTests = run
