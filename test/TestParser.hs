{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

module TestParser where

import Data.Either (isLeft)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

import MarkdownParser
    ( doParse,
      boldP,
      italicP,
      strikethroughP,
      highlightP,
      subP,
      supP,
      backtickP,
      emojiP,
      linkP,
      imageP,
      lineBreakP,
      literalP,
      headingP,
      paragraphP,
      blockquoteP,
      ulP,
      olP,
      tableP,
      defListP,
      codeBlockP,
      hrP )
      
import MarkdownSyntax
    ( Block(Block),
      Statement(Literal, Bold, Italic, Strikethrough, Highlight, Sub,
                Sup, Backtick, Emoji, Link, Image, LineBreak),
      Header(H1, H2, H4),
      DefItem(DI),
      Component(Heading, Blockquote, UnorderedList, HorizontalRule,
                OrderedList, Table, DefinitionList, Paragraph, CodeBlock) )

-- >>> runTestTT test_componentP
-- Counts {cases = 21, tried = 21, errors = 0, failures = 0}

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
tTestHeadingP = "parsing headings" ~: TestList [
   doParse headingP "## Heading level2  \n" ~?= Right (
    Heading H2 (
      Block [Literal "Heading", Literal " ", Literal "level2"]
      ) Nothing),
    doParse headingP "#### Sonnets {#id-1}  \n" ~?= Right (
      Heading H4 (Block [Literal "Sonnets"]) (Just "id-1")
    ),
    doParse headingP "#H1 {id2}\n" ~?= Right (
      Heading H1 (Block [Literal "H1", Literal " ", Literal "{id2}"]) Nothing
    ),
    isLeft (doParse headingP ">H1\n") ~?= True
  ]

tTestParagraphP :: Test 
tTestParagraphP = 
  doParse paragraphP "Hello world." ~?= Right (
    Paragraph (Block [Literal "Hello",Literal " ",Literal "world."])
    )

tTestBlockquoteP :: Test 
tTestBlockquoteP = 
  doParse blockquoteP "> ***This is love***\n> ~~Yep, it is~~ \n" ~?=
    Right (
      Blockquote [
        Block [Bold (Block [Italic (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "love"])])],
        Block [Strikethrough (Block [Literal "Yep,",Literal " ",Literal "it",Literal " ",Literal "is"]),Literal " "]
      ])

tTestUnorderedListP :: Test 
tTestUnorderedListP = "parsing unordered list" ~: TestList [
  doParse ulP "- A\n- B\n  - C\n  - D\n***" ~?= Right expectedUnorderedListP,
  doParse ulP "* A\n* B\n  * C\n  * D\n***" ~?= Right expectedUnorderedListP,
  doParse ulP"+ A\n+ B\n  + C\n  + D\n***" ~?= Right expectedUnorderedListP,
  isLeft (doParse ulP "# A\n+ B\n  + C\n  + D\n***") ~?= True
  ]
  where 
    expectedUnorderedListP :: Component 
    expectedUnorderedListP = UnorderedList [
        [Paragraph (Block [Literal "A"])],
        [Paragraph (Block [Literal "B"]), UnorderedList [
          [Paragraph (Block [Literal "C"])],
          [Paragraph (Block [Literal "D"])]
        ]]
      ]

tTestOrderedListP :: Test 
tTestOrderedListP = "parsing ordered list" ~: TestList [
  doParse olP "1. A\n2. B\n  1. C\n  2. D\n***" ~?= Right expectedOrderedListP,
  isLeft (doParse olP "-. A\n2. B\n  1. C\n  2. D\n***") ~?= True
  ]
  where 
    expectedOrderedListP :: Component 
    expectedOrderedListP = OrderedList [
        [Paragraph (Block [Literal "A"])],
        [Paragraph (Block [Literal "B"]), OrderedList [
          [Paragraph (Block [Literal "C"])],
          [Paragraph (Block [Literal "D"])]
        ]]
      ]

tTestTableP :: Test 
tTestTableP = TestList [
  doParse tableP testTableP ~?= Right expectedTableP,
  isLeft (doParse tableP "|A|B|") ~?= True
  ]
  where 
    testTableP = "|A|B|\n|--|---|\n|Paragraph|Text|\n"
    expectedTableP = Table [
      [Paragraph (Block [Literal "A"]),Paragraph (Block [Literal "B"])],
      [Paragraph (Block [Literal "Paragraph"]),Paragraph (Block [Literal "Text"])]
      ]

tTestDefinitionListP :: Test 
tTestDefinitionListP = doParse defListP "Best PL\n: Haskell \n: Python\n\n" ~?= Right (
  DefinitionList [
    DI (Paragraph $ Block [Literal "Best", Literal " ", Literal "PL"]) 
    [
      Paragraph $ Block [Literal "Haskell"],
      Paragraph $ Block [Literal "Python"]
      ]
    ]
  )

tTestCodeBlockP :: Test 
tTestCodeBlockP = "parsing code blocks" ~: TestList [
  doParse codeBlockP "```\nabc  \n   def()```" ~?= Right expectedCodeBlockP, 
  doParse codeBlockP "~~~\nabc  \n   def()~~~" ~?= Right expectedCodeBlockP,
  isLeft (doParse codeBlockP "^^^\nabc  \n  def()^^^") ~?= True,
  isLeft (doParse codeBlockP "```abc  \n  def()^^^") ~?= True
  ]
  where 
    expectedCodeBlockP = CodeBlock (Block [Literal "abc  ",Literal "   def()"])
  
tTestHorinzontalRuleP :: Test 
tTestHorinzontalRuleP = "parsing horizontal rules" ~: TestList [
  doParse hrP "***\n" ~?= Right HorizontalRule,
  doParse hrP "---\n" ~?= Right HorizontalRule,
  doParse hrP "___\n" ~?= Right HorizontalRule,
  doParse hrP "________     \n" ~?= Right HorizontalRule,
  isLeft (doParse hrP "__  \n") ~?= True,
  isLeft (doParse hrP "_ __  \n") ~?= True,
  isLeft (doParse hrP "+++\n") ~?= True
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
      tTestLineBreakP,
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
  putStrLn "Test parsing statements"
  _ <- runTestTT test_statementP
  putStrLn "Test parsing components"
  _ <- runTestTT test_componentP
  return ()
