import TestConverter ( runTests )
import TestParser ( runTests )

main :: IO ()
main = do 
    putStrLn "============ Testing Converter ============="
    _ <- TestConverter.runTests
    putStrLn "============== Testing Parser =============="
    _ <- TestParser.runTests
    putStrLn "=================== End ===================="
