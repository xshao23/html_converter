{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
module MarkdownParser where 

import Control.Applicative
import Data.Char (isNumber)
import Data.Char qualified as Char
import Data.Char qualified as P
import MarkdownSyntax
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC


-- | Skip whitespace 
wsP :: Parser a -> Parser a
-- wsP p = many P.space *> p
wsP p = p <* many P.space

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

-- | Parse only a particular string 
stringP :: String -> Parser ()
stringP s = wsP $ P.string s *> pure ()

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a"
        ~?= Right
          [(), ()]
    ]
-- | Combination string and whitespace parser
constP :: String -> a -> Parser a
constP s newVal = stringP s *> pure newVal

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]


componentP :: Parser Component
componentP  = undefined

blockP :: Parser Block 
blockP = undefined

statementP :: Parser Statement 
statementP = undefined


itemP :: Parser Item
itemP = undefined 

taskItemP :: Parser TaskItem
taskItemP = undefined


headerP :: Parser Header
headerP = undefined

headingP :: Parser Component 
headingP = undefined 

paragraphP :: Parser Component 
paragraphP = undefined

blockquoteP :: Parser Component 
blockquoteP = undefined

orderedListP :: Parser Component 
orderedListP = undefined

unorderedListP :: Parser Component 
unorderedListP = undefined

codeBlockP :: Parser Component 
codeBlockP = undefined

horizontalRuleP :: Parser Component 
horizontalRuleP = undefined

newlineP :: Parser Component 
newlineP = undefined

plainP :: Parser Component
plainP = undefined


boldP :: Parser Statement
boldP = undefined

italicP :: Parser Statement
italicP = undefined

backtickP :: Parser Statement
backtickP = undefined

linkP :: Parser Statement
linkP = undefined

imageP :: Parser Statement 
imageP = undefined

linebreakP :: Parser Statement 
linebreakP = undefined

literalP :: Parser Statement
literalP = undefined

emojiP :: Parser Statement
emojiP = undefined

strikethroughP :: Parser Statement
strikethroughP = undefined