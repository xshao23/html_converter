{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

module TestParser where

import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

import Test.QuickCheck qualified as QC

import MarkdownParser
    ( literalP,
      lineBreakP,
      linkP,
      imageP,
      backtickP,
      emojiP,
      supP,
      subP,
      highlightP,
      strikethroughP,
      italicP,
      boldP,
      doParse )
      
import MarkdownSyntax
    ( Statement(Literal, Bold, Italic, Strikethrough, Highlight, Sub,
                Sup, Backtick, Emoji, Link, Image, LineBreak),
      Block(Block),
      Component(Paragraph, Table, DefinitionList),
      DefItem(DI) )
import HtmlConverter 
    (
      tTestHighlight, 
      tTestBacktick, 
      tTestLineBreak, 
      tTestHorinzontalRule, 
      tTestBlockquote, 
      tTestTable )

-- test cases for Parser 
test_componentP :: Test
test_componentP =
  TestList
    [ 
      tTestHeadingP,
      tTestParagraphP,
      tTestBlockquoteP,
      tTestUnorderedListP,
      tTestOrderedListP,
      tTestTableP,
      tTestDefinitionListP,
      tTestCodeBlockP,
      tTestHorinzontalRuleP
      ]

tTestHeadingP :: Test 
tTestHeadingP = undefined

tTestParagraphP :: Test 
tTestParagraphP = undefined

tTestBlockquoteP :: Test 
tTestBlockquoteP = undefined

tTestUnorderedListP :: Test 
tTestUnorderedListP = undefined 

tTestOrderedListP :: Test 
tTestOrderedListP = undefined

tTestTableP :: Test 
tTestTableP = undefined

tTestDefinitionListP :: Test 
tTestDefinitionListP = undefined

tTestCodeBlockP :: Test 
tTestCodeBlockP = undefined

tTestHorinzontalRuleP :: Test 
tTestHorinzontalRuleP = undefined

-- >>> doParse paragraphP "Hello world."
-- Right (Paragraph (Block [Literal "Hello",Literal " ",Literal "world."]))


-- >>> doParse blockP "This is love"
-- Right (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "love"])

-- >>> doParse blockquoteP' "> This is love\n"
-- Right (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "love"])

-- >>> doParse blockquoteP "> ***This is love***\n> ~~Yep, it is~~ \n"
-- Right (Blockquote [Block [Bold (Block [Italic (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "love"])])],Block [Strikethrough (Block [Literal "Yep,",Literal " ",Literal "it",Literal " ",Literal "is"]),Literal " "]])

-- >>> doParse blockquoteP "> **A**\n> B\n"
-- Right (Blockquote [Block [Bold (Block [Literal "A"])],Block [Literal "B"]])


-- >>> doParse ulP "- Hello world\n- B\n  - C\n  - D\n    - E\n      - F\n-"
-- Right (UnorderedList [[Paragraph (Block [Literal "Hello",Literal " ",Literal "world"])],[Paragraph (Block [Literal "B"]),UnorderedList [[Paragraph (Block [Literal "C"])],[Paragraph (Block [Literal "D"]),UnorderedList [[Paragraph (Block [Literal "E"]),UnorderedList [[Paragraph (Block [Literal "F"])]]]]]]]])

-- >>> doParse (orderedListP 0 [] []) "1. Hello world\n2. B\n  3. C\n  4. D\n    5. E\n      6. F\n***"


-- >>> doParse codeblockP "```\nabc  \n   def()```"
-- Right (CodeBlock (Block [Literal "abc  ",Literal "   def()"]))


tt = "|A|B|\n|--|---|\n|Paragraph|Text|\n"

-- >>> doParse tableP tt
-- Right (Table [[Plain (Block [Literal "A"]),Plain (Block [Literal "B"])],[Plain (Block [Literal "Paragraph"]),Plain (Block [Literal "Text"])]])
  
tab = Table [
  [Paragraph (Block [Literal "A"]),Paragraph (Block [Literal "B"])],
  [Paragraph (Block [Literal "Paragraph"]),Paragraph (Block [Literal "Text"])]]

-- >>> doParse defP ": This is first definition\n: This is second def\n"
-- Right (Paragraph (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "first",Literal " ",Literal "definition"]))

defTest = "First Term\n: This is first definition.\nSecond Term\n: hello world\n: Yes there you go\n"

-- >>> doParse defListP defTest
-- Right (
  
defAns = DefinitionList [
    DI (Paragraph (Block [Literal "First",Literal " ",Literal "Term"])) [
      Paragraph (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "first",Literal " ",Literal "definition."])
      ],
    DI (Paragraph (Block [Literal "Second",Literal " ",Literal "Term"])) [
      Paragraph (Block [Literal "hello",Literal " ",Literal "world"]),
      Paragraph (Block [Literal "Yes",Literal " ",Literal "there",Literal " ",Literal "you",Literal " ",Literal "go"])
      ]
    ]

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

runTests :: IO ()
runTests = do 
  _ <- runTestTT test_statementP
  _ <- runTestTT test_componentP
  return ()