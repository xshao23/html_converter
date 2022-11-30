module HtmlConverter where 

import MarkdownSyntax
import HtmlEditor 
import Test.HUnit (Test(..), (~?=), (~:), runTestTT)

toHTML :: Markdown -> HTML
toHTML = undefined -- use insert here 

outputHTML :: HTML -> IO ()
outputHTML = undefined

-- Some simple unit tests for converting markdown to html

test1 :: Markdown
test1 = Markdown [
  Heading H1 (Block [Literal "Test Example 1"]),
  Newline,
  HorizontalRule, 
  Newline,
  UnorderedList [
    [Plain "hello, world!"], 
    [Plain "unordered", OrderedList [[Plain "first line"], [Plain "second line"]]]
    ]
  ]

expected1 :: HTML 
expected1 = Html [
  "<h1>Test Example 1</h1>",
  "<hr/>",
  "<ul><li>hello, world!</li><li>unordered<ol><li>first line</li><li>second line</li></ol></li></ul>"
  ]

tTest1 :: Test 
tTest1 = toHTML test1 ~?= expected1

test2 :: Markdown 
test2 = Markdown [
  Heading H1 (Block [Literal "H1 Heading"]),
  Heading H3 (Block [Literal "H3 Heading"]),
  Paragraph (Block [
    Italic (Block [Literal "Italicized test"]),
    Bold (Block [Literal "Love is bold"]),
    Italic (Block [Bold (Block [Literal "Bold and Italic"])])
  ])
  ]

expected2 :: HTML 
expected2 = Html [
  "<h1>H1 Heading</h1>",
  "<h3>H3 Heading</h3>",
  "<p><em>Italicized test</em><strong>Love is bold</strong><em><strong>Bold and Italic</strong></em></p>"
  ]

tTest2 :: Test 
tTest2 = toHTML test2 ~?= expected2

tHeadTest :: Test
tHeadTest = "heading test" ~: TestList [
  toHTML test1 ~?= expected1,
  toHTML test2 ~?= expected2
 ]

test3 :: Markdown 
test3 = Markdown [
  Heading H5 (Block [Literal "H5 Heading"]),
  CodeBlock "getDate()"
  ]

expected3 :: HTML 
expected3 = Html [
  "<h5>H5 Heading</h5>",
  "<code>getDate()</code>"
  ]

tTest3 :: Test 
tTest3 = toHTML test3 ~?= expected3