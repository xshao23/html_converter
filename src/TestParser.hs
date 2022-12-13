{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

module TestParser where


-- import Lib (Parser (...), MarkdownSyntax)
-- import Cosntrol.Applicative
import Control.Applicative
import Data.Char (isNumber)
import Data.Char qualified as Char
import Data.Char qualified as P
import MarkdownParser
import MarkdownSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import HtmlConverter (tTestHighlight, t, tTestBacktick, tTestLineBreak)

-- test cases for Parser 
test_componentP :: Test
test_componentP =
  TestList
    [ doParse headingP "# H1 Heading"  ~?= Right (Heading H1 (Block [Literal "H1", Literal " ", Literal "Heading"])),
      doParse paragraphP "Hello world."  ~?= Right (Paragraph (Block [Literal "Hello", Literal " ", Literal "world."])),
      --doParse blockquoteP ">I love CIS552>\n>It's the best course!"  ~?= Right (Blockquote [Plain (Literal "I love CIS552"), Newline, Plain (Literal "It's the best course!")]),
      --doParse orderedListP "1. A\n2. B" ~?= Right (OrderedList [[Plain (Literal "first line")], [Plain (Literal "second line")]]),
      --doParse unorderedListP "- A\n- B" ~?= Right (UnorderedList [[Plain (Literal "first line")], [Plain (Literal "second line")]])
      --doParse codeBlockP "`getDate()`" ~?= Right (CodeBlock "getDate()") 
      ]


-- >>> doParse paragraphP "Hello world."
-- Right (Paragraph (Block [Literal "Hello",Literal " ",Literal "world."]))

-- >>> runTestTT test_statementP
-- Counts {cases = 14, tried = 14, errors = 0, failures = 0}

test_statementP :: Test
test_statementP = 
  TestList
    [
      tTestBoldP,
      tTestItalicP,
      tTestStrikethroughP,
      tTestHighlightP,
      tTestSubP,
      tTestSupP,      
      tTestBacktickP,
      tTestEmojiP,
      tTestLinkP,
      tTestLinkTitleP,
      tTestImageP,
      tTestImageTitleP,
      tTestLineBreak,
      tTestLiteralP
      
    ]

tTestBoldP :: Test 
tTestBoldP = doParse boldP "**bold**" ~?= Right (Bold $ Block [Literal "bold"])

tTestItalicP :: Test 
tTestItalicP = 
  doParse italicP "*italicized*"  ~?= 
    Right (Italic $ Block [Literal "italicized"])

tTestStrikethroughP :: Test 
tTestStrikethroughP = 
  doParse strikethroughP "~~abc~~" ~?= 
    Right (Strikethrough $ Block [Literal "abc"])

tTestHighlightP :: Test 
tTestHighlightP = 
  doParse highlightP "==important==" ~?= 
    Right (Highlight $ Block [Literal "important"])

tTestSubP :: Test 
tTestSubP = doParse subP "~2~" ~?= Right (Sub $ Block [Literal "2"])

tTestSupP :: Test 
tTestSupP = doParse supP "^abc^" ~?= Right (Sup $ Block [Literal "abc"])

tTestBacktickP :: Test 
tTestBacktickP = doParse backtickP "`backticked`" ~?= Right (Backtick "backticked")

tTestEmojiP :: Test 
tTestEmojiP = doParse emojiP ":joy:" ~?= Right (Emoji "128516")

tTestLinkP :: Test 
tTestLinkP = 
  doParse linkP "[Duck](https://duckduckgo.com)" ~?= 
    Right (Link (Block [Literal "Duck"]) "https://duckduckgo.com" Nothing)
  
tTestLinkTitleP :: Test 
tTestLinkTitleP = 
  doParse linkP "[Duck](https://duckduckgo.com \"This is duck\")" ~?= 
    Right (Link (Block [Literal "Duck"]) "https://duckduckgo.com" (Just "This is duck"))

tTestImageP :: Test 
tTestImageP = 
  doParse imageP "![Mountains](san-juan-mountains.jpg)" ~?= 
    Right (Image "Mountains" "san-juan-mountains.jpg" Nothing)

tTestImageTitleP :: Test 
tTestImageTitleP = 
  doParse imageP "![Mountains](san-juan-mountains.jpg \"San Juan Mountains\")" ~?= 
    Right (Image "Mountains" "san-juan-mountains.jpg" (Just "San Juan Mountains"))

tTestLineBreakP :: Test 
tTestLineBreakP = doParse lineBreakP "  \na" ~?= Right LineBreak

tTestLiteralP :: Test 
tTestLiteralP = doParse literalP "hello" ~?= Right (Literal "hello")
