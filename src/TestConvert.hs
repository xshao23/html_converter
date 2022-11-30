module Tests where

import Test.HUnit (Test(..), (~?=), (~:), runTestTT)
import Data.Either (isLeft)
-- import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
--     oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
--     classify,  maxSuccess, listOf, resize, scale, (==>))


import MarkdownParser hiding (main)


tInlines :: Test
tInlines = TestList [  tBoldTest, tItalicsTest, tCodeTest, tCodeDouble, tLiteralTest, 
                        tLiteralUntilTest, tLinkTest, tAutoLinkTest, tImageTest, tHardBreakTest,
                        tTextBold, tTextBoldUnderline, tTextAutoLink, tTextLink, tTextHardBreak,
                        tTextBoldItalics, tTextNested, tStrongEmph, tTextNotClosed, tTextOneNotClosed ]

tBoldTest :: Test
tBoldTest = "bold test" ~: TestList [ 
        parse "**hello**" ~?= Right (Bold [Literal "hello"])
    ,   parse "**hel\nlo**" ~?= Right (Bold [Literal "hel\nlo"])
    ,   isLeft (parse "**hel\n\nlo**") ~?= True
    ]
    where parse = runParser $ boldP False

tItalicsTest :: Test
tItalicsTest = "italics test" ~: TestList [
        parse "*hello*" ~?= Right (Italics [Literal "hello"])
    ,   parse "*hel\nlo*" ~?= Right (Italics [Literal "hel\nlo"])
    ,   isLeft (parse "*hel\n\nlo*") ~?= True
    ]
    where parse = runParser $ italicsP False
    