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
  tTestBacktick,
  tTestLink,
  tTestImage,
  tTestLineBreak,
  tTestLiteral
  ]

tCompTest :: Test 
tCompTest = "Component test" ~: TestList [
  tTestHeading,
  tTestParagraph,
  tTestBlockquote,
  tTestOrderedList,
  tTestUnorderedList,
  tTestCodeBlock,
  tTestHorinzontalRule,
  tTestPlain
  ]

tMarkdownTest :: Test 
tMarkdownTest = "Markdown test" ~: TestList [
  tTest1, 
  tTest2, 
  tTest3
  ]