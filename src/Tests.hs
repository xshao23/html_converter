module Tests where 

import TestConverter ( runTests )
import TestParser ( runTests )

run :: IO ()
run = do 
    putStrLn "============== Testing Converter =============="
    _ <- TestConverter.runTests
    putStrLn "===================== End ====================="
