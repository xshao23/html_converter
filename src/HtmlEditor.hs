module HtmlEditor where 

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Semigroup ( Endo(Endo, appEndo) )
import Test.HUnit (Test(..), (~?=), (~:), runTestTT)

import qualified Data.Foldable as Foldable

import MarkdownSyntax

data SimpleHTML a
  = PCDATA a
  | Element a [(a, a)] [SimpleHTML a] -- Element ElementName Attri [..]
  deriving (Eq, Show, Foldable)

-- | Convert a SimpleHTML value to a string
html2string :: SimpleHTML String -> String
html2string html = go html "" where
  go :: SimpleHTML String -> String -> String
  go (PCDATA s) = (s ++)
  go (Element tag _ []) = (("<" ++ tag ++ "/>") ++ )
  -- TODO: Image does not have a closing tag
  go (Element tag attri body) = \s ->
      "<" ++ tag ++ readAttri attri ++ ">" ++ 
        appEndo (foldMap (Endo . go) body) ("</" ++ tag ++ ">" ++ s)

readAttri :: [(String, String)] -> String 
readAttri = Prelude.foldr (\(k, v) acc -> k ++ "=\"" ++ v++ "\" " ++ acc) ""

t :: [(String, String)]
t = [("src", "http://"), ("title", "Upenn"), ("alt", "Flowers in Chania")]

-- >>> readAttri t
-- "src=\"http://\" title=\"Upenn\" alt=\"Flowers in Chania\" "

title :: Maybe String -> String 
title Nothing = "" 
title (Just s) = " title=" ++ s


empty :: SimpleHTML String
empty = PCDATA ""

-- insert (Heading H1 (Block [Literal "CIS 552"])) E -> Heading H1 (Block [Literal "CIS 552"])
htmlInsert :: SimpleHTML String -> SimpleHTML String -> SimpleHTML String
htmlInsert = undefined

convComp :: Component -> SimpleHTML String
convComp = undefined

convStmt :: Statement -> SimpleHTML String
convStmt = undefined


-- Unit test cases
tTestBold :: Test
tTestBold = 
  convStmt (Bold (Block [Literal "Love is bold"])) ~?= 
    Element "strong" [] [PCDATA "Love is bold"]

tTestItalic :: Test 
tTestItalic = 
  convStmt (Italic (Block [Literal "This is italic text"])) ~?= 
    Element "em" [] [PCDATA "This is italic text"]

tTestBacktick :: Test 
tTestBacktick = 
  convStmt (Backtick "thisIsCode()") ~?= 
    Element "code" [] [PCDATA "thisIsCode()"]

tTestLink :: Test
tTestLink = 
  convStmt (
    Link (Block [Literal "CIS 552"]) 
    "https://www.seas.upenn.edu/~cis5520/22fa" 
    Nothing
    ) ~?= 
      Element "a" [("href", "https://www.seas.upenn.edu/~cis5520/22fa")] [PCDATA "CIS 552"]
      -- "<a href='https://www.seas.upenn.edu/~cis5520/22fa'>CIS 552</a>"

tTestImage :: Test
tTestImage = 
  convStmt (
    Image "Upenn" "image.jpg" (Just "image title")
    ) ~?= 
      Element "img" [("alt", "Upenn"), ("src", "image.jpg"), ("title", "image title")] []
      -- "<a><img title='image title' alt='Upenn' src='image.jpg'></a>"

tTestLineBreak :: Test 
tTestLineBreak = convStmt LineBreak ~?= Element "br" [] []

tTestLiteral = 
  convStmt (Literal "This is just a plain text") ~?= 
    PCDATA "This is just a plain text"

tTestHeading :: Test
tTestHeading = 
  convComp (
    Heading H1 (Block [Literal "H1 Heading", LineBreak, Literal "Continues"])
    ) ~?= Element "h1" [] [
      PCDATA "H1 Heading", 
      Element "br" [] [], 
      PCDATA "Continues"
      ]
      --["<h1>", "H1 Heading", "<br>", "Continues", "</h1>"] 

tTestParagraph :: Test
tTestParagraph = 
  convComp (
    Paragraph (Block [
      Bold (Block [Literal "Hello "]), Italic (Block [Literal "World"])
      ])
    ) ~?=  Element "p" [] [
      Element "strong" [] [PCDATA "Hello "], 
      Element "em" [] [PCDATA "World"]
      ]
      -- "<p>", 
      --   "<strong>", "Hello ", "</strong>", 
      --   "<em>", "World", "</em>", 
      -- "</p>"
    
tTestBlockquote :: Test
tTestBlockquote = convComp (
  Blockquote [
    Plain "I love CIS552", 
    Newline, 
    Plain "It's best course!"
    ]) ~?= Element "blockquote" [] [
      PCDATA "I love CIS552", 
      Element "br" [] [], 
      PCDATA "It's the best course!"
      ]
  {-
  ["<blockquote>", 
          "I love CIS552", 
          "<br>", 
          "It's the best course!", 
        "</blockquote>"
        ]
  -}

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

expectedOrderedList :: SimpleHTML String
expectedOrderedList = Element "ol" [] [
  Element "li" [] [PCDATA "first line"], 
  Element "li" [] [
    PCDATA "methods: ", 
    Element "ul" [] [
      Element "li" [] [Element "code" [] [PCDATA "getDate()"]],
      Element "li" [] [Element "code" [] [PCDATA "getTime()"]],
      Element "li" [] [Element "code" [] [PCDATA "getMinutes()"]]
      ]
    ]
  ]
  -- [
  -- "<ol>", 
  --   "<li>", "first line", "</li>",
  --   "<li>", "methods: ",
  --     "<ul>", 
  --       "<li>", "<code>", "getDate()", "</code>", "</li>",
  --       "<li>", "<code>", "getTime()", "</code>", "</li>",
  --       "<li>", "<code>", "getMinutes()", "</code>", "</li>",
  --     "</ul>",
  --   "</li>",
  -- "</ol>"
  -- ]

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

expectedUnorderedList :: SimpleHTML String
expectedUnorderedList = Element "ul" [] [
  Element "li" [] [
    Element "h4" [] [PCDATA "H4 Heading"], Element "p" [] [PCDATA "I love Haskell"]
    ], 
  Element "li" [] [
    Element "h5" [] [PCDATA "H5 Heading"], Element "p" [] [PCDATA "and FP in general"]
    ]
  ]
{-
  [
  "<ul>",
    "<li>", "<h4>", "H4 Heading", "</h4>", "<p>", "I love Haskell", "</p>", "</li>",
    "<li>", "<h5>", "H5 Heading", "</h5>", "<p>", "and FP in general", "</p>", "</li>",
  "</ul>"
  ]
  -}

tTestUnorderedList :: Test 
tTestUnorderedList = convComp testUnorderedList ~?= expectedUnorderedList

tTestCodeBlock :: Test
tTestCodeBlock = 
  convComp (CodeBlock "a + b = c") ~?= 
    Element "code" [] [PCDATA "a + b = c"] --["<code>", "a + b = c", "</code>"]

tTestHorinzontalRule :: Test
tTestHorinzontalRule = convComp HorizontalRule ~?= Element "hr" [] [] --["<hr>"]

tTestPlain :: Test
tTestPlain = 
  convComp (Plain "This is just a plain text") ~?= 
    PCDATA "This is just a plain text"

-- | Delete all matched component found 
htmlDelete :: SimpleHTML String -> SimpleHTML String -> SimpleHTML String
htmlDelete = undefined

isMember :: SimpleHTML String -> SimpleHTML String -> Bool 
isMember = undefined

elements :: SimpleHTML String -> [String]
elements = Foldable.toList

-- >>> elements (HLink "https://www.seas.upenn.edu/~cis5520/22fa" (PCDATA "CIS 552") (Just "this is title"))
-- ["https://www.seas.upenn.edu/~cis5520/22fa","CIS 552","this is title"]


-- Below is some properties we would like to check for our html editor

-- Post-Condiiton Property
prop_FindPostPresent :: SimpleHTML String -> SimpleHTML String-> Bool 
prop_FindPostPresent k h = isMember k (htmlInsert k h)

prop_FindPostAbsent :: SimpleHTML String -> SimpleHTML String -> Bool 
prop_FindPostAbsent k h = not (isMember k (htmlDelete k h))

-- Metamorphic Property 
prop_InsertEmpty :: SimpleHTML String -> Bool 
prop_InsertEmpty k = elements (htmlInsert k empty) == elements k

prop_InsertInsert :: SimpleHTML String-> SimpleHTML String -> SimpleHTML String-> Bool 
prop_InsertInsert x y h = htmlInsert x (htmlInsert y h) == htmlInsert y (htmlInsert x h)

prop_InsertDelete :: SimpleHTML String -> SimpleHTML String -> SimpleHTML String -> Bool 
prop_InsertDelete k k0 h = 
  htmlInsert k (htmlDelete k0 h) == 
    if k == k0 then htmlInsert k h else htmlDelete k0 (htmlInsert k h)

prop_MemberInsert :: SimpleHTML String -> SimpleHTML String-> SimpleHTML String-> Bool 
prop_MemberInsert k k0 h = 
  isMember k0 (htmlInsert k h) == (k == k0 || isMember k0 h)

prop_DeleteEmpty :: SimpleHTML String -> Bool 
prop_DeleteEmpty k = htmlDelete k empty == empty 

prop_DeleteInsert :: SimpleHTML String -> SimpleHTML String -> SimpleHTML String -> Bool 
prop_DeleteInsert k k0 h = 
  htmlDelete k (htmlInsert k0 h) == if k == k0 then 
    if isMember k0 h then htmlDelete k h else h
    else htmlInsert k0 (htmlDelete k h)

prop_DeleteDelete :: SimpleHTML String -> SimpleHTML String -> SimpleHTML String -> Bool 
prop_DeleteDelete x y h = 
  htmlDelete x (htmlDelete y h) == htmlDelete y (htmlDelete x h)

-- Model-based Property 
prop_InsertModel :: SimpleHTML String -> SimpleHTML String -> Bool 
prop_InsertModel k h = 
  elements (htmlInsert k h) == elements h ++ elements k
