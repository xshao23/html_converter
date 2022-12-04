module Tests where

import Test.HUnit (Test(..), (~?=), (~:), runTestTT)
import Test.QuickCheck ( quickCheck )
import HtmlConverter
    ( tTestBold,
      tTestItalic,
      tTestBoldItalic,
      tTestBacktick,
      tTestLink,
      tTestImage,
      tTestLineBreak,
      tTestLiteral,
      tTestHeading,
      tTestParagraph,
      tTestBlockquote,
      tTestOrderedList,
      tTestUnorderedList,
      tTestTaskList,
      tTestCodeBlock,
      tTestHorinzontalRule,
      tTestPlain,
      tTest1,
      tTest2,
      tTest3,
      prop_InsertStmt,
      prop_FindPostPresentStmt,
      prop_FindPostPresentCmpt,
      prop_InsertCmpt )

-- Unit tests for convStmt
tStmtTest :: Test
tStmtTest = "Statement test" ~: TestList [ 
  tTestBold,
  tTestItalic,
  tTestBoldItalic,
  tTestBacktick,
  tTestLink,
  tTestImage,
  tTestLineBreak,
  tTestLiteral
  ]

-- >>> runTestTT tStmtTest 
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}

tCmptTest :: Test 
tCmptTest = "Component test" ~: TestList [
  tTestHeading,
  tTestParagraph,
  tTestBlockquote,
  tTestOrderedList,
  tTestUnorderedList,
  tTestTaskList,
  tTestCodeBlock,
  tTestHorinzontalRule,
  tTestPlain
  ]
-- >>> runTestTT tCmptTest
-- Counts {cases = 9, tried = 9, errors = 0, failures = 0}

tMarkdownTest :: Test 
tMarkdownTest = "Markdown test" ~: TestList [
  tTest1, 
  tTest2, 
  tTest3
  ]
-- >>> runTestTT tMarkdownTest
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

runTests :: IO ()
runTests = do 
  _ <- runTestTT tStmtTest 
  _ <- runTestTT tCmptTest 
  _ <- runTestTT tMarkdownTest

  putStrLn "quickCheck prop_InsertStmt"
  quickCheck prop_InsertStmt 

  putStrLn "quickCheck prop_InsertCmpt"
  quickCheck prop_InsertCmpt

  putStrLn "quickCheck prop_FindPostPresentStmt"
  quickCheck prop_FindPostPresentStmt

  putStrLn "quickCheck prop_FindPostPresentCmpt"
  quickCheck prop_FindPostPresentCmpt