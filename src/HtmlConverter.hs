module HtmlConverter where 

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

-- Part 0 : Convert a SimpleHTML value to a string
html2string :: SimpleHTML String -> String
html2string html = go html "" where
  go :: SimpleHTML String -> String -> String
  go (PCDATA s) = (s ++)
  go (Element tag attri []) = (("<" ++ tag ++ readAttris attri ++ "/>") ++ )
  -- TODO: Image does not have a closing tag
  go (Element tag attri body) = \s ->
      "<" ++ tag ++ readAttris attri ++ ">" ++ 
        appEndo (foldMap (Endo . go) body) ("</" ++ tag ++ ">" ++ s)

readAttris :: [(String, String)] -> String 
readAttris = Prelude.foldr (\(k, v) acc -> " " ++ k ++ "=\"" ++ v++ "\"" ++ acc) ""

t :: [(String, String)]
t = [("src", "http://"), ("title", "Upenn"), ("alt", "Flowers in Chania")]

boldAndItalic = convStmt $ Bold (Block [
    Literal "Love is bold", 
    Italic (Block [Literal " and italic"])
  ])

-- >>> html2string boldAndItalic 
-- "<strong>Love is bold<em> and italic</em></strong>"

-- >>> html2string (convComp testOrderedList)
-- "<ol><li>first line</li><li>methods: <ul><li><code>getDate()</code></li><li><code>getTime()</code></li><li><code>getMinutes()</code></li></ul></li></ol>"

-- >>> html2string (convComp testTaskList)
-- "<ul><li class=\"checked\">Pay bills</li><li>Submit assignment</li><li class=\"checked\"><strong>Exercise</strong></li></ul>"


-- >>> readAttris t
-- "src=\"http://\" title=\"Upenn\" alt=\"Flowers in Chania\" "

-- Part 1 : Convert the given Markdown file to an HTML file
convert :: Markdown -> SimpleHTML String 
convert (Markdown cs) = render (map convComp cs)

render :: [SimpleHTML String] -> SimpleHTML String 
render ss = Element "html" [] [Element "body" [] ss]

-- Part 2 : Convert each component of the Markdown to a SimpleHTML
convComp :: Component -> SimpleHTML String
convComp (Heading h b) = Element (show h) [] (convBlock b)
convComp (Paragraph b) = Element "p" [] (convBlock b) 
convComp (Blockquote cs) = Element "blockquote" [] (map convComp cs)
convComp (OrderedList ol) = Element "ol" [] (map convItem ol)
convComp (UnorderedList ul) = Element "ul" [] (map convItem ul)
convComp (TaskList ti) = Element "ul" [] (map convTaskItem ti)
convComp (CodeBlock s) = Element "code" [] [PCDATA s]
convComp HorizontalRule = Element "hr" [] [] 
convComp Newline = Element "br" [] []
convComp (Plain s) = convStmt s

convItem :: Item -> SimpleHTML String 
convItem item = Element "li" [] (map convComp item) 

convTaskItem :: TaskItem -> SimpleHTML String 
convTaskItem (TI True item) = Element "li" (addAttris [("class", Just "checked")]) (map convComp item) 
convTaskItem (TI False item) = convItem item

-- Part 3 : Convert each statement within the component to a SimpleHTML
convBlock :: Block -> [SimpleHTML String]
convBlock (Block ss) = map convStmt ss

convStmt :: Statement -> SimpleHTML String
convStmt (Literal s) = PCDATA s  
convStmt LineBreak = Element "br" [] []
convStmt (Emoji s) = PCDATA ("&#" ++ s ++ ";") 
convStmt (Backtick s) = Element "code" [] [PCDATA s] 
convStmt (Image alt src title) = Element "img" (
  addAttris [("alt", Just alt), ("src", Just src), ("title", title)]
  ) []
convStmt (Bold b) = Element "strong" [] (convBlock b)
convStmt (Italic b) = Element "em" [] (convBlock b)
convStmt (Strikethrough b) = Element "s" [] (convBlock b)
convStmt (Link b href title) = Element "a" (
  addAttris [("href", Just href), ("title", title)]
  ) (convBlock b)

addAttris :: [(String, Maybe String)] -> [(String, String)]
addAttris = foldr (\(k, v) acc -> addAttri k v acc) []
  where 
    addAttri k Nothing xs = xs 
    addAttri k (Just v) xs = (k, v) : xs

-- Unit tests of statement
tTestBold :: Test
tTestBold = 
  convStmt (Bold (Block [Literal "Love is bold"])) ~?= 
    Element "strong" [] [PCDATA "Love is bold"]

tTestItalic :: Test 
tTestItalic = 
  convStmt (Italic (Block [Literal "This is italic text"])) ~?= 
    Element "em" [] [PCDATA "This is italic text"]

tTestBoldItalic :: Test 
tTestBoldItalic = 
  convStmt (Bold (Block [
    Literal "Love is bold", 
    Italic (Block [Literal " and italic"])
  ])) ~?= Element "strong" [] [
    PCDATA "Love is bold", 
    Element "em" [] [PCDATA " and italic"]
  ]

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

-- Unit tests of components
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
    Plain (Literal "I love CIS552"), 
    Newline, 
    Plain (Literal "It's the best course!")
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
  [Plain (Literal "first line")], 
  [Plain (Literal "methods: "), UnorderedList [
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
  {- <ol>
      <li>first line</li>
      <li>methods: 
        <ul>
          <li><code>getDate()</code></li>
          <li><code>getTime()</code></li>
          <li><code>getMinutes()</code></li>
        </ul>
      </li>
    </ol>
  -}

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

testTaskList :: Component 
testTaskList = TaskList [
  TI True [Plain (Literal "Pay bills")],
  TI False [Plain (Literal "Submit assignment")],
  TI True [Plain (Bold (Block [Literal "Exercise"]))]
  ]

expectedTaskList :: SimpleHTML String
expectedTaskList = Element "ul" [] [
  Element "li" [("class", "checked")] [PCDATA "Pay bills"],
  Element "li" [] [PCDATA "Submit assignment"],
  Element "li" [("class", "checked")] [Element "strong" [] [PCDATA "Exercise"]]
  ]

tTestTaskList :: Test 
tTestTaskList = convComp testTaskList ~?= expectedTaskList

tTestCodeBlock :: Test
tTestCodeBlock = 
  convComp (CodeBlock "a + b = c") ~?= 
    Element "code" [] [PCDATA "a + b = c"] --["<code>", "a + b = c", "</code>"]

tTestHorinzontalRule :: Test
tTestHorinzontalRule = convComp HorizontalRule ~?= Element "hr" [] [] --["<hr>"]

tTestPlain :: Test
tTestPlain = 
  convComp (Plain (Literal "This is just a plain text")) ~?= 
    PCDATA "This is just a plain text"

-- Unit tests of Markdown
test1 :: Markdown
test1 = Markdown [
  Heading H1 (Block [Literal "Test Example 1"]),
  Newline,
  HorizontalRule, 
  Newline,
  UnorderedList [
    [Plain (Literal "hello, world!")], 
    [
      Plain (Literal "unordered"), 
      OrderedList [
        [Plain (Literal "first line")], 
        [Plain (Literal "second line")]
      ]]
    ]
  ]

expected1 :: SimpleHTML String 
expected1 = render [
  Element "h1" [] [PCDATA "Test Example 1"],
  Element "br" [] [],
  Element "hr" [] [],
  Element "br" [] [],
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
tTest1 = convert test1 ~?= expected1

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
expected2 :: SimpleHTML String
expected2 = 
  render [
    Element "h1" [] [PCDATA "H1 Heading"],
    Element "h3" [] [PCDATA "H3 Heading"],
    Element "p" [] [
      Element "em" [] [PCDATA "Italicized test"],
      Element "strong" [] [PCDATA "Love is bold"],
      Element "em" [] [Element "strong" [] [PCDATA "Bold and Italic"]]
    ]
  ]

tTest2 :: Test 
tTest2 = convert test2 ~?= expected2

test3 :: Markdown 
test3 = Markdown [
  Heading H5 (Block [Literal "H5 Heading"]),
  CodeBlock "getDate()"
  ]
expected3 :: SimpleHTML String
expected3 = render [Element "h5" [] [PCDATA  "H5 Heading"], Element "code" [] [PCDATA "getDate()"]]

tTest3 :: Test 
tTest3 = convert test3 ~?= expected3

-- TODO: Think of a property test

-- | Operations to build a whole HTML document
empty :: SimpleHTML String
empty = render []

-- insert (Heading H1 (Block [Literal "CIS 552"])) E -> Heading H1 (Block [Literal "CIS 552"])
insert :: SimpleHTML String -> [SimpleHTML String] -> [SimpleHTML String]
insert = undefined

-- | Delete all matched component found 

htmlDelete :: SimpleHTML String -> SimpleHTML String -> SimpleHTML String
htmlDelete = undefined

isMember :: SimpleHTML String -> [SimpleHTML String] -> Bool 
isMember = undefined

elements :: SimpleHTML String -> [String]
elements = Foldable.toList

-- >>> elements (HLink "https://www.seas.upenn.edu/~cis5520/22fa" (PCDATA "CIS 552") (Just "this is title"))
-- ["https://www.seas.upenn.edu/~cis5520/22fa","CIS 552","this is title"]


-- Below is some properties we would like to check for our html editor
{-
-- Post-Condiiton Property
prop_FindPostPresent :: SimpleHTML String -> [SimpleHTML String] -> Bool 
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
-}
