module HtmlConverter where 

import MarkdownSyntax
import HtmlEditor 
import Data.Semigroup ( Endo(Endo, appEndo) )
import Test.HUnit (Test(..), (~?=), (~:), runTestTT)

newtype HTMLFile a
  = Root [SimpleHTML a] 
  deriving (Eq, Show, Foldable)

htmlFile2String :: HTMLFile String -> String 
htmlFile2String (Root elems)  = 
  "<!DOCTYPE html>\n" ++ "<html>\n" ++ concatMap html2string elems ++ "</html>"
      
toHTML :: Markdown -> HTMLFile String
toHTML = undefined -- use insert here 
 
outputHTML :: HTMLFile String -> IO ()
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

expected1 :: HTMLFile String 
expected1 = Root [
  Element "h1" [] [PCDATA "Test Example 1"],
  Element "hr" [] [],
  Element "ul" [] [
    Element "li" [] [
      PCDATA "hello, world!"
      ],
    Element "li" [] [
      PCDATA "unordered",
      Element "ol" [] [
        Element "li" [] [PCDATA "first line"],
        Element "li" [] [PCDATA "second line"]
        ]
      ]  
  ]
  ]
  -- "<h1>Test Example 1</h1>",
  -- "<hr/>",
  -- "<ul><li>hello, world!</li> <li>unordered <ol><li>first line</li><li>second line</li></ol></li></ul>"


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

expected2 :: HTMLFile String
expected2 = 
  Root [
    Element "h1" [] [PCDATA "H1 Heading"],
    Element "h3" [] [PCDATA "H3 Heading"],
    Element "p" [] [
      Element "em" [] [PCDATA "Italicized test"],
      Element "strong" [] [PCDATA "Love is bold"],
      Element "em" [] [Element "strong" [] [PCDATA "Bold and Italic"]]
    ]
  ]
  -- -- Html [
  -- "<h1>H1 Heading</h1>",
  -- "<h3>H3 Heading</h3>",
  -- "<p><em>Italicized test</em><strong>Love is bold</strong><em><strong>Bold and Italic</strong></em></p>"
  -- ]



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

expected3 :: HTMLFile String
expected3 = Root [Element "h5" [] [PCDATA  "H5 Heading"], Element "code" [] [PCDATA "getDate()"]]

tTest3 :: Test 
tTest3 = toHTML test3 ~?= expected3