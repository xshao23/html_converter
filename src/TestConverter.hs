module Tests where

import Test.HUnit (Test(..), (~?=), (~:), runTestTT)
import Data.Either (isLeft)
-- import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
--     oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
--     classify,  maxSuccess, listOf, resize, scale, (==>))


import MarkdownParser hiding (main)
import HtmlEditor 
import HtmlConverter

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

tCompTest :: Test 
tCompTest = "Component test" ~: TestList [
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
-- >>> runTestTT tCompTest
-- Counts {cases = 9, tried = 9, errors = 0, failures = 0}

tMarkdownTest :: Test 
tMarkdownTest = "Markdown test" ~: TestList [
  tTest1, 
  tTest2, 
  tTest3
  ]
-- >>> runTestTT tMarkdownTest

