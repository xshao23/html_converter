module HtmlEditor where 

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Test.HUnit (Test(..), (~?=), (~:), runTestTT)

import MarkdownSyntax

type CompStr = String 

data HTML
  = E -- empty html file
  | Html [CompStr] -- <h1>This is CIS 552</h1>
  deriving (Eq, Show)

empty :: HTML 
empty = E 

-- insert (Heading H1 (Block [Literal "CIS 552"])) E -> Heading H1 (Block [Literal "CIS 552"])
insert :: CompStr -> HTML -> HTML
insert = undefined

convComp :: Component -> [CompStr] 
convComp = undefined

convStmt :: Statement -> CompStr 
convStmt = undefined

-- Unit test cases

tTestBold :: Test
tTestBold = 
  convStmt (Bold (Block [Literal "Love is bold"])) ~?= 
    "<strong>Love is bold</strong>"

tTestItalic :: Test 
tTestItalic = 
  convStmt (Italic (Block [Literal "This is italic text"])) ~?= 
    "<em>This is italic text</em>"

tTestBacktick :: Test 
tTestBacktick = 
  convStmt (Backtick "thisIsCode()") ~?= 
    "<code>thisIsCode()</code>"

tTestLink :: Test
tTestLink = 
  convStmt (
    Link (Block [Literal "CIS 552"]) 
    "https://www.seas.upenn.edu/~cis5520/22fa" 
    Nothing
    ) ~?= 
      "<a href='https://www.seas.upenn.edu/~cis5520/22fa'>CIS 552</a>"

tTestImage :: Test
tTestImage = 
  convStmt (
    Image "Upenn" "image.jpg" (Just "image title")
    ) ~?= 
      "<a><img title='image title' alt='Upenn' src='image.jpg'></a>"

tTestLineBreak :: Test 
tTestLineBreak = convStmt LineBreak ~?= "<br>"

tTestLiteral = 
  convStmt (Literal "This is just a plain text") ~?= 
    "This is just a plain text"

tTestHeading :: Test
tTestHeading = 
  convComp (
    Heading H1 (Block [Literal "H1 Heading", LineBreak, Literal "Continues"])
    ) ~?= ["<h1>", "H1 Heading", "<br>", "Continues", "</h1>"]

tTestParagraph :: Test
tTestParagraph = 
  convComp (
    Paragraph (Block [
      Bold (Block [Literal "Hello "]), Italic (Block [Literal "World"])
      ])
    ) ~?= [
      "<p>", 
        "<strong>", "Hello ", "</strong>", 
        "<em>", "World", "</em>", 
      "</p>"
    ]

tTestBlockquote :: Test
tTestBlockquote = convComp (
  Blockquote [
    Plain "I love CIS552", 
    Newline, 
    Plain "It's best course!"
    ]) ~?= 
    ["<blockquote>", 
        "I love CIS552", 
        "<br>", 
        "It's the best course!", 
      "</blockquote>"
      ]

testOrderedList :: Component 
testOrderedList = OrderedList [
  [Plain "first line"], 
  [Plain "methods: ", UnorderedList [
      [CodeBlock "getDate()"],
      [CodeBlock "getTime()"],
      [CodeBlock "getMinutes()"]
      ]
    ]
  ]

expectedOrderedList :: [CompStr]
expectedOrderedList = [
  "<ol>", 
    "<li>", "first line", "</li>",
    "<li>", "methods: ",
      "<ul>", 
        "<li>", "<code>", "getDate()", "</code>", "</li>",
        "<li>", "<code>", "getTime()", "</code>", "</li>",
        "<li>", "<code>", "getMinutes()", "</code>", "</li>",
      "</ul>",
    "</li>",
  "</ol>"
  ]

tTestOrderedList :: Test 
tTestOrderedList = convComp testOrderedList ~?= expectedOrderedList

testUnorderedList :: Component 
testUnorderedList = UnorderedList [
  [
    Heading H4 (Block [Literal "H4 Heading"]), 
    Paragraph (Block [Literal "I love Haskell"])
  ],
  [
    Heading H5 (Block [Literal "H5 Heading"]), 
    Paragraph (Block [Literal "and FP in general"])
  ] 
  ]

expectedUnorderedList :: [CompStr]
expectedUnorderedList = [
  "<ul>",
    "<li>", "<h4>", "H4 Heading", "</h4>", "<p>", "I love Haskell", "</p>", "</li>",
    "<li>", "<h5>", "H5 Heading", "</h5>", "<p>", "and FP in general", "</p>", "</li>",
  "</ul>"
  ]

tTestUnorderedList :: Test 
tTestUnorderedList = convComp testUnorderedList ~?= expectedUnorderedList

tTestCodeBlock :: Test
tTestCodeBlock = 
  convComp (CodeBlock "a + b = c") ~?= ["<code>", "a + b = c", "</code>"]

tTestHorinzontalRule :: Test
tTestHorinzontalRule = convComp HorizontalRule ~?= ["<hr>"]

tTestPlain :: Test
tTestPlain = 
  convComp (Plain "This is just a plain text") ~?= ["This is just a plain text"]

-- | Delete all matched component found 
delete :: CompStr -> HTML -> HTML
delete = undefined

member :: CompStr -> HTML -> Bool 
member = undefined

elements :: HTML -> [CompStr]
elements (Html cs) = cs
elements _ = []

-- Below is some properties we would like to check for our html editor

-- Post-Condiiton Property
prop_FindPostPresent :: CompStr -> HTML -> Bool 
prop_FindPostPresent k h = member k (insert k h)

prop_FindPostAbsent :: CompStr -> HTML -> Bool 
prop_FindPostAbsent k h = not (member k (delete k h))

-- Metamorphic Property 
prop_InsertEmpty :: CompStr -> Bool 
prop_InsertEmpty k = elements (insert k empty) == [k]

prop_InsertInsert :: CompStr -> CompStr -> HTML -> Bool 
prop_InsertInsert x y h = insert x (insert y h) == insert y (insert x h)

prop_InsertDelete :: CompStr -> CompStr -> HTML -> Bool 
prop_InsertDelete k k0 h = 
  insert k (delete k0 h) == 
    if k == k0 then insert k h else delete k0 (insert k h)

prop_MemberInsert :: CompStr -> CompStr -> HTML -> Bool 
prop_MemberInsert k k0 h = 
  member k0 (insert k h) == (k == k0 || member k0 h)

prop_DeleteEmpty :: CompStr -> Bool 
prop_DeleteEmpty k = delete k empty == empty 

prop_DeleteInsert :: CompStr -> CompStr -> HTML -> Bool 
prop_DeleteInsert k k0 h = 
  delete k (insert k0 h) == if k == k0 then 
    if member k0 h then delete k h else h
    else insert k0 (delete k h)

prop_DeleteDelete :: CompStr -> CompStr -> HTML -> Bool 
prop_DeleteDelete x y h = 
  delete x (delete y h) == delete y (delete x h)

-- Model-based Property 
prop_InsertModel :: CompStr -> HTML -> Bool 
prop_InsertModel k h = 
  elements (insert k h) == List.insert k (List.delete k $ elements h)

prop_DeleteModel :: CompStr -> HTML -> Bool 
prop_DeleteModel k h = 
  elements (delete k h) == List.delete k (elements h)

