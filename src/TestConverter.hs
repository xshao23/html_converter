module TestConverter where 
  
import Test.HUnit (Test(..), (~?=), (~:), runTestTT)
import Test.QuickCheck ( quickCheck )
import HtmlConverter
    ( tTestBold,
      tTestItalic,
      tTestBoldItalic,
      tTestStrikethrough,
      tTestHighlight,
      tTestSub,
      tTestSup,
      tTestBacktick,
      tTestEmoji,
      tTestLink,
      tTestImage,
      tTestLineBreak,
      tTestLiteral,
      tTestHeading,
      tTestHeadingID,
      tTestParagraph,
      tTestBlockquote,
      tTestOrderedList,
      tTestUnorderedList,
      tTestTable,
      tTestDefinitionList,
      tTestCodeBlock,
      tTestHorinzontalRule,
      tTestPlain,
      tTest1,
      tTest2,
      tTest3,
      prop_InsertStmt,
      prop_FindPostPresentStmt,
      prop_FindPostPresentCmpt,
      prop_InsertCmpt,
      prop_InsertEmptyStmt,
      prop_InsertEmptyCmpt,
      prop_DeleteEmptyStmt,
      prop_DeleteEmptyCmpt 
      )

-- Unit tests for convStmt
tStmtTest :: Test
tStmtTest = "Statement test" ~: TestList [ 
  tTestBold,
  tTestItalic,
  tTestBoldItalic,
  tTestStrikethrough,
  tTestHighlight,
  tTestSub,
  tTestSup,
  tTestBacktick,
  tTestEmoji,
  tTestLink,
  tTestImage,
  tTestLineBreak,
  tTestLiteral
  ]

-- >>> runTestTT tStmtTest 
-- Counts {cases = 13, tried = 13, errors = 0, failures = 0}

tCmptTest :: Test 
tCmptTest = "Component test" ~: TestList [
  tTestHeading,
  tTestHeadingID,
  tTestParagraph,
  tTestBlockquote,
  tTestOrderedList,
  tTestUnorderedList,
  tTestTable,
  tTestDefinitionList,
  tTestCodeBlock,
  tTestHorinzontalRule,
  tTestPlain
  ]
-- >>> runTestTT tCmptTest
-- Counts {cases = 11, tried = 11, errors = 0, failures = 0}

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
  putStrLn "Test converting statements"
  _ <- runTestTT tStmtTest 
  putStrLn "Test converting components"
  _ <- runTestTT tCmptTest 
  putStrLn "Test converting markdown input"
  _ <- runTestTT tMarkdownTest

  putStrLn "quickCheck prop_FindPostPresentStmt"
  quickCheck prop_FindPostPresentStmt

  putStrLn "quickCheck prop_FindPostPresentCmpt"
  quickCheck prop_FindPostPresentCmpt

  putStrLn "quickCheck prop_InsertEmptyStmt"
  quickCheck prop_InsertEmptyStmt

  putStrLn "quickCheck prop_InsertEmptyCmpt"
  quickCheck prop_InsertEmptyCmpt

  putStrLn "quickCheck prop_DelettEmptyStmt"
  quickCheck prop_DeleteEmptyStmt

  putStrLn "quickCheck prop_DeleteEmptyCmpt"
  quickCheck prop_DeleteEmptyCmpt

  putStrLn "quickCheck prop_InsertStmt"
  quickCheck prop_InsertStmt 

  putStrLn "quickCheck prop_InsertCmpt"
  quickCheck prop_InsertCmpt
