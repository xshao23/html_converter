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
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

-- test cases for Parser 
test_componentP :: Test
test_componentP =
  TestList
    [ P.parse headingP "# H1 Heading"  ~?= Right (Heading H1 (Block [Literal "H1 Heading"])),
      P.parse paragraphP "Hello world."  ~?= Right (Paragraph (Block [Literal "Hello world."])),
      P.parse blockquoteP ">I love CIS552>\n>It's the best course!"  ~?= Right (Blockquote [Plain (Literal "I love CIS552"), Newline, Plain (Literal "It's the best course!")]),
      P.parse orderedListP "1. first line\n 2.second line" ~?= Right (OrderedList [[Plain (Literal "first line")], [Plain (Literal "second line")]]),
      P.parse unorderedListP "- first line\n - second line" ~?= Right (UnorderedList [[Plain (Literal "first line")], [Plain (Literal "second line")]]),
      P.parse codeBlockP "`getDate()`" ~?= Right (CodeBlock "getDate()") 
    ]

test_statementP :: Test
test_statementP = 
  TestList
    [
      P.parse boldP "**this is a bold text**"  ~?= Right (Bold $ Block [Literal "this is a bold text"]),
      P.parse italicP "**this is a italicized text**"  ~?= Right (Italic $ Block [Literal "this is a italicized text"]),
      P.parse backtickP "`a backticked string`" ~?= Right (Backtick "a backticked string"),
      P.parse linkP "[Duck Duck Go](https://duckduckgo.com)"~?= Right (Link (Block [Literal "Duck Duck Go"]) "https://duckduckgo.com" Nothing ),
      P.parse linkP "[Duck Duck Go](https://duckduckgo.com \"The best search engine for privacy\")"~?= Right (Link (Block [Literal "Duck Duck Go"]) "https://duckduckgo.com" (Just "The best search engine for privacy")),
      P.parse imageP "![The San Juan Mountains are beautiful!](/assets/images/san-juan-mountains.jpg)" ~?= Right (Image "The San Juan Mountains are beautiful!" "/assets/images/san-juan-mountains.jpg" Nothing ),
      P.parse imageP "![The San Juan Mountains are beautiful!](/assets/images/san-juan-mountains.jpg \"San Juan Mountains\")" ~?= Right (Image "The San Juan Mountains are beautiful!" "/assets/images/san-juan-mountains.jpg" (Just "San Juan Mountains") )
    ]

