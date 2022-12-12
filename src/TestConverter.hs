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
      tTestParagraph,
      tTestBlockquote,
      tTestOrderedList,
      tTestUnorderedList,
      tTestTaskList,
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
      prop_FindPostAbsentStmt,
      prop_FindPostAbsentCmpt,
      prop_InsertCmpt,
      prop_InsertEmptyStmt,
      prop_InsertEmptyCmpt,
      prop_DeleteEmptyStmt,
      prop_DeleteEmptyCmpt,
      prop_DeleteDeleteStmt,
      prop_DeleteDeleteCmpt )

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
  tTestParagraph,
  tTestBlockquote,
  tTestOrderedList,
  tTestUnorderedList,
  tTestTaskList,
  tTestTable,
  tTestDefinitionList,
  tTestCodeBlock,
  tTestHorinzontalRule,
  tTestPlain
  ]
-- >>> runTestTT tCmptTest
-- Counts {cases = 12, tried = 12, errors = 0, failures = 0}

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

  putStrLn "quickCheck prop_FindPostPresentStmt"
  quickCheck prop_FindPostPresentStmt

  putStrLn "quickCheck prop_FindPostPresentCmpt"
  quickCheck prop_FindPostPresentCmpt

  putStrLn "quickCheck prop_FindPostAbsentStmt"
  quickCheck prop_FindPostAbsentStmt

  putStrLn "quickCheck prop_FindPostAbsentCmpt"
  quickCheck prop_FindPostAbsentCmpt

  putStrLn "quickCheck prop_InsertEmptyStmt"
  quickCheck prop_InsertEmptyStmt

  putStrLn "quickCheck prop_InsertEmptyCmpt"
  quickCheck prop_InsertEmptyCmpt

  putStrLn "quickCheck prop_DelettEmptyStmt"
  quickCheck prop_DeleteEmptyStmt

  putStrLn "quickCheck prop_DeleteEmptyCmpt"
  quickCheck prop_DeleteEmptyCmpt

  putStrLn "quickCheck prop_DelettDeleteStmt"
  quickCheck prop_DeleteDeleteStmt

  putStrLn "quickCheck prop_DeleteDeleteCmpt"
  quickCheck prop_DeleteDeleteCmpt

  putStrLn "quickCheck prop_InsertStmt"
  quickCheck prop_InsertStmt 

  putStrLn "quickCheck prop_InsertCmpt"
  quickCheck prop_InsertCmpt
