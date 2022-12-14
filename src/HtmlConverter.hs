{-# LANGUAGE DeriveFoldable #-}
module HtmlConverter where 

import Data.Maybe as Maybe
import Data.Semigroup ( Endo(Endo, appEndo) )
import Test.HUnit (Test(..), (~?=), (~:), runTestTT)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
  )

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Test.QuickCheck as QC

import MarkdownSyntax
import qualified GHC.Generics as Maybe

data SimpleHTML a
  = E
  | PCDATA a
  | Element a [(a, a)] [SimpleHTML a] -- Element ElementName Attri [..]
  deriving (Eq, Show, Foldable)

convertMarkdownContent :: Markdown -> String 
convertMarkdownContent = html2string . convert 

error :: String 
error = html2string (render [PCDATA "Error Parsing the File"])

-- Part 0 : Convert a SimpleHTML value to a string
html2string :: SimpleHTML String -> String
html2string html = go html "" where
  go :: SimpleHTML String -> String -> String
  go E = ("" ++)
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

-- >>> html2string (convCmpt testOrderedList)
-- "<ol><li>first line</li><li>methods: <ul><li><code>getDate()</code></li><li><code>getTime()</code></li><li><code>getMinutes()</code></li></ul></li></ol>"

-- >>> readAttris t
-- "src=\"http://\" title=\"Upenn\" alt=\"Flowers in Chania\" "

-- Part 1 : Convert the given Markdown file to an HTML file
convert :: Markdown -> SimpleHTML String 
convert (Markdown cs) = render (map convCmpt cs)

render :: [SimpleHTML String] -> SimpleHTML String 
render ss = Element "html" [] [Element "body" [] ss]

tableBorder :: Maybe String 
tableBorder = Just "border-spacing: 40px 0"

-- Part 2 : Convert each component of the Markdown to a SimpleHTML
convCmpt :: Component -> SimpleHTML String
convCmpt (Heading h b hid) = Element (show h) (addAttris [("id", hid)]) (convBlock b)
convCmpt (Paragraph b) = Element "p" [] (convBlock b) 
convCmpt (Blockquote bs) = Element "blockquote" [] (convBlocks bs)
convCmpt (OrderedList ol) = Element "ol" [] (map convItem ol)
convCmpt (UnorderedList ul) = Element "ul" [] (map convItem ul)
convCmpt (Table tr) = Element "table" (addAttris [("style", tableBorder)]) (convRows True tr)
convCmpt (DefinitionList dl) = Element "dl" [] (concatMap convDefItem dl)
convCmpt (CodeBlock b) = convBr "code" b
convCmpt HorizontalRule = Element "hr" [] [] 
convCmpt Newline = Element "br" [] []
convCmpt (Plain (Block ss)) = 
  PCDATA $ foldr (\s acc -> f s ++ acc) [] ss 
  where 
    f :: Statement -> String 
    f (Literal s) = s  
    f _ = ""

convBr :: String -> Block -> SimpleHTML String 
convBr tag (Block ss) = Element tag [] f
  where 
    f :: [SimpleHTML String]
    f = foldr (\s acc -> if null acc 
      then convBrStmt s : acc
      else convBrStmt s : Element "br" [] [] : acc) [] ss 

convBrStmt :: Statement -> SimpleHTML String 
convBrStmt (Literal s) = PCDATA (convLiteral s)
convBrStmt _ = E

convLiteral :: String -> String 
convLiteral = foldr (\c acc -> convChar c ++ acc) []

convChar :: Char -> String
convChar ' ' = "&nbsp;"
convChar c = [c]

convItem :: Item -> SimpleHTML String 
convItem item = Element "li" [] (map convCmpt item)

convRows :: Bool -> [Row] -> [SimpleHTML String]
convRows _ [] = [] 
convRows isTitle (r : rs) = Element "tr" [] (
  map (if isTitle then convRowTitle else convCol) r
  ) : convRows False rs

convRowTitle :: Component -> SimpleHTML String 
convRowTitle title = Element "th" [] [convCmpt title]

convCol :: Component -> SimpleHTML String 
convCol col = Element "td" [] [convCmpt col]

convDefItem :: DefItem -> [SimpleHTML String]
convDefItem (DI c cs) = Element "dt" [] [convCmpt c] : getDefs cs
  where
    getDefs :: [Component] -> [SimpleHTML String]
    getDefs = map (\c -> Element "dd" [] [convCmpt c])

-- Part 3 : Convert each statement within the component to a SimpleHTML
convBlocks :: [Block] -> [SimpleHTML String]
convBlocks = foldr (\b acc -> 
  if null acc 
    then convBlock b ++ acc 
    else convBlock b ++ [Element "br" [] []] ++ acc) []

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
convStmt (Highlight b) = Element "mark" [] (convBlock b)
convStmt (Sub b) = Element "sub" [] (convBlock b) 
convStmt (Sup b) = Element "sup" [] (convBlock b)
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

tTestStrikethrough :: Test 
tTestStrikethrough = 
  convStmt (Strikethrough (Block [
    Literal "This is outdated",
    Bold (Block [Literal " long time ago"])
  ])) ~?= 
    Element "s" [] [
      PCDATA "This is outdated",
      Element "strong" [] [PCDATA " long time ago"]
    ]

tTestHighlight :: Test 
tTestHighlight = 
  convStmt (Highlight (Block [Literal "This is very essential"])) ~?= 
    Element "mark" [] [PCDATA "This is very essential"]

tTestSub :: Test 
tTestSub = 
  convCmpt (Paragraph (Block [
    Literal "H", 
    Sub (Block [Literal "2"]),
    Literal "O"
    ])) ~?= 
      Element "p" [] [PCDATA "H", Element "sub" [] [PCDATA "2"], PCDATA "O"]

tTestSup :: Test 
tTestSup = 
  convCmpt (Paragraph (Block [
    Literal "X",
    Sup (Block [Literal "2"])
  ])) ~?= Element "p" [] [PCDATA "X", Element "sup" [] [PCDATA "2"]]

tTestBacktick :: Test 
tTestBacktick = 
  convStmt (Backtick "thisIsCode()") ~?= 
    Element "code" [] [PCDATA "thisIsCode()"]

tTestEmoji :: Test 
tTestEmoji = convStmt (Emoji "129409") ~?= 
  PCDATA "&#129409;"

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
  convCmpt (
    Heading H1 (Block [Literal "H1 Heading", LineBreak, Literal "Continues"]) Nothing
    ) ~?= Element "h1" [] [
      PCDATA "H1 Heading", 
      Element "br" [] [], 
      PCDATA "Continues"
      ]
      --["<h1>", "H1 Heading", "<br>", "Continues", "</h1>"] 

tTestHeadingID :: Test 
tTestHeadingID = 
  convCmpt (
    Heading H1 (Block [Literal "H1 Heading", LineBreak, Literal "Continues"]) (Just "heading1")
    ) ~?= Element "h1" [("id", "heading1")] [
      PCDATA "H1 Heading", 
      Element "br" [] [], 
      PCDATA "Continues"
      ]
      
tTestParagraph :: Test
tTestParagraph = 
  convCmpt (
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
tTestBlockquote = convCmpt (
  Blockquote [Block [
    Literal "I love CIS552", 
    LineBreak, 
    Literal "It's the best course!"]]
    ) ~?= Element "blockquote" [] [
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
  [Plain (Block [Literal "first line"])], 
  [Plain (Block [Literal "methods: "]), UnorderedList [
      [CodeBlock (Block [Literal "getDate()"])],
      [CodeBlock (Block [Literal "getTime()"])],
      [CodeBlock (Block [Literal "getMinutes()"])]
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
tTestOrderedList = convCmpt testOrderedList ~?= expectedOrderedList

testUnorderedList :: Component 
testUnorderedList = UnorderedList [
  [
    Heading H4 (Block [Literal "H4 Heading"]) Nothing, 
    Paragraph (Block [Literal "I love Haskell"])
  ],
  [
    Heading H5 (Block [Literal "H5 Heading"]) Nothing, 
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

tab = Table [[Plain (Block [Literal "A"]),Plain (Block [Literal "B"])],[Plain (Block [Literal "Paragraph"]),Plain (Block [Literal "Text"])]]

-- >>> html2string $ convCmpt tab 
-- "<table><tr><th>A</th><th>B</th></tr><tr><td>Paragraph</td><td>Text</td></tr></table>"

ul = UnorderedList [[Plain (Block [Literal "Hello",Literal " ",Literal "world"])],[Plain (Block [Literal "B"]),UnorderedList [[Plain (Block [Literal "C"])],[Plain (Block [Literal "D"]),UnorderedList [[Plain (Block [Literal "E"]),UnorderedList [[Plain (Block [Literal "F"])]]]]]]]]

-- >>> html2string $ convCmpt ul
-- "<ul><li>Hello world</li><li>B<ul><li>C</li><li>D<ul><li>E<ul><li>F</li></ul></li></ul></li></ul></li></ul>"

tTestUnorderedList :: Test 
tTestUnorderedList = convCmpt testUnorderedList ~?= expectedUnorderedList

testTable :: Component
testTable = Table [
  [
    Plain (Block [Literal "Company"]), 
    Plain (Block [Literal "Contact"]), 
    Plain (Block [Literal "Country"])
    ],
  [
    Plain (Block [Literal "Alfreds Futterkiste"]), 
    Plain (Block [Literal "Maria Anders"]), 
    Plain (Block [Literal "Germany"])
    ],
  [
    Plain (Block [Literal "Centro comercial Moctezuma"]), 
    Plain (Block [Literal "Francisco Chang"]), 
    Plain (Block [Literal "Mexico"])
    ]
  ]

expectedTable :: SimpleHTML String 
expectedTable = Element "table" [("style","border-spacing: 40px 0")] [
  Element "tr" [] [
    Element "th" [] [PCDATA "Company"],
    Element "th" [] [PCDATA "Contact"],
    Element "th" [] [PCDATA "Country"]
    ],

  Element "tr" [] [
    Element "td" [] [PCDATA "Alfreds Futterkiste"],
    Element "td" [] [PCDATA "Maria Anders"],
    Element "td" [] [PCDATA "Germany"]
    ],

  Element "tr" [] [
    Element "td" [] [PCDATA "Centro comercial Moctezuma"],
    Element "td" [] [PCDATA "Francisco Chang"],
    Element "td" [] [PCDATA "Mexico"]
    ]
  ]

tTestTable :: Test
tTestTable = convCmpt testTable ~?= expectedTable

{-
<table>
  <tr>
    <th><strong>Company</strong></th>
    <th><strong>Contact</strong></th>
    <th><strong>Country</strong></th>
  </tr>
  <tr>
    <td>Alfreds Futterkiste</td>
    <td>Maria Anders</td>
    <td>Germany</th></td>
  <tr>
    <td>Centro comercial Moctezuma</td>
    <td>Francisco Chang</td>
    <td>Mexico</td>
  </tr>
</table>"
-}

testDefinitionList :: Component 
testDefinitionList = DefinitionList [
  DI (Plain (Block [Literal "First Term"])) [
    Plain (Block [Literal "This is the definition of the first term"])
    ], 
  DI (Plain (Block [Literal "Second Term"])) [
    Plain (Block [Literal "This is the definition of the second term"]), 
    Plain (Block [Literal "This is another definition of the second term"])
    ]
  ]

expectedDefinitionList :: SimpleHTML String 
expectedDefinitionList = Element "dl" [] [
  Element "dt" [] [PCDATA "First Term"],
  Element "dd" [] [PCDATA "This is the definition of the first term"],
  Element "dt" [] [PCDATA "Second Term"],
  Element "dd" [] [PCDATA "This is the definition of the second term"],
  Element "dd" [] [PCDATA "This is another definition of the second term"]
  ]
  {-
  <dl>
    <dt>First Term</dt>
    <dd>This is the definition of the first term</dd>
    <dt>Second Term</dt>
    <dd>This is the definition of the second term</dd>
    <dd>This is another definition of the second term</dd>
  </dl>"
  -}
tTestDefinitionList :: Test 
tTestDefinitionList = convCmpt testDefinitionList ~?= expectedDefinitionList 

defAns = DefinitionList [
    DI (Paragraph (Block [Literal "First",Literal " ",Literal "Term"])) [
      Paragraph (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "first",Literal " ",Literal "definition."])
      ],
    DI (Paragraph (Block [Literal "Second",Literal " ",Literal "Term"])) [
      Paragraph (Block [Literal "hello",Literal " ",Literal "world"]),
      Paragraph (Block [Literal "Yes",Literal " ",Literal "there",Literal " ",Literal "you",Literal " ",Literal "go"])
      ]
    ]

-- >>> convCmpt defAns
-- Element "dl" [] [Element "dt" [] [Element "p" [] [PCDATA "First",PCDATA " ",PCDATA "Term"]],Element "dd" [] [Element "p" [] [PCDATA "This",PCDATA " ",PCDATA "is",PCDATA " ",PCDATA "first",PCDATA " ",PCDATA "definition."]],Element "dt" [] [Element "p" [] [PCDATA "Second",PCDATA " ",PCDATA "Term"]],Element "dd" [] [Element "p" [] [PCDATA "hello",PCDATA " ",PCDATA "world"]],Element "dd" [] [Element "p" [] [PCDATA "Yes",PCDATA " ",PCDATA "there",PCDATA " ",PCDATA "you",PCDATA " ",PCDATA "go"]]]

-- >>> html2string $ convCmpt defAns
-- "<dl><dt><p>First Term</p></dt><dd><p>This is first definition.</p></dd><dt><p>Second Term</p></dt><dd><p>hello world</p></dd><dd><p>Yes there you go</p></dd></dl>"


tTestCodeBlock :: Test
tTestCodeBlock = 
  convCmpt (CodeBlock (Block [Literal "def()"])) ~?= 
    Element "code" [] [PCDATA "def()"] --["<code>", "def()", "</code>"]

tTestHorinzontalRule :: Test
tTestHorinzontalRule = convCmpt HorizontalRule ~?= Element "hr" [] [] --["<hr>"]

tTestPlain :: Test
tTestPlain = 
  convCmpt (Plain (Block [Literal "This is just a plain text"])) ~?= 
    PCDATA "This is just a plain text"

-- Unit tests of Markdown
test1 :: Markdown
test1 = Markdown [
  Heading H1 (Block [Literal "Test Example 1"]) Nothing,
  Newline,
  HorizontalRule, 
  Newline,
  UnorderedList [
    [Plain (Block [Literal "hello, world!"])], 
    [
      Plain (Block [Literal "unordered"]), 
      OrderedList [
        [Plain (Block [Literal "first line"])], 
        [Plain (Block [Literal "second line"])]
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
  Heading H1 (Block [Literal "H1 Heading"]) Nothing,
  Heading H3 (Block [Literal "H3 Heading"]) Nothing,
  Paragraph (Block [
    Italic (Block [Literal "Italicized test"]),
    Bold (Block [Literal "Love is bold"]),
    Italic (Block [Bold (Block [Literal "Bold and Italic"])])
  ]),
  Paragraph (Block [
    Link (Block [Literal "Go to Heading 1"]) "#heading1" Nothing,
    Link (Block [Literal "Go to Heading 3"]) "#heading3" Nothing
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
    ],
    Element "p" [] [
      Element "a" [("href", "#heading1")] [PCDATA "Go to Heading 1"],
      Element "a" [("href", "#heading3")] [PCDATA "Go to Heading 3"]
    ]
  ]

tTest2 :: Test 
tTest2 = convert test2 ~?= expected2

test3 :: Markdown 
test3 = Markdown [
  Heading H5 (Block [Literal "H5 Heading"]) Nothing,
  CodeBlock (Block [Literal "getDate()"])
  ]
expected3 :: SimpleHTML String
expected3 = render [Element "h5" [] [PCDATA  "H5 Heading"], Element "code" [] [PCDATA "getDate()"]]

tTest3 :: Test 
tTest3 = convert test3 ~?= expected3

-- Below are some operations for property (HTML structure) tests
empty :: SimpleHTML String 
empty = E 

insert :: SimpleHTML String -> SimpleHTML String -> Maybe (SimpleHTML String)
insert k E = Just k
insert k h@(PCDATA s) = Nothing
insert k h@(Element tag attri ss) = Just $ Element tag attri (ss ++ [k])

delete :: SimpleHTML String -> SimpleHTML String -> SimpleHTML String
delete k h = 
  if k == h then E 
  else case h of 
    Element tag attri ss -> Element tag attri (delete' k ss) 
    _ -> h
  where 
    delete' :: SimpleHTML String -> [SimpleHTML String] -> [SimpleHTML String]
    delete' k = foldr (\s acc -> if s == k then acc else delete k s : acc) []

member :: SimpleHTML String -> SimpleHTML String -> Bool 
member E _ = True
member k h@(Element tag attri ss) = k == h || any (member k) ss
member k h = k == h 

elements :: SimpleHTML String -> [String]
elements = Foldable.toList

-- Post-Condiiton Property
prop_FindPostPresent :: SimpleHTML String -> SimpleHTML String -> Property
prop_FindPostPresent k h = 
  Maybe.isJust (insert k h) ==> member k (Maybe.fromJust (insert k h))

prop_FindPostPresentStmt :: Statement -> Statement -> Property 
prop_FindPostPresentStmt s1 s2 = prop_FindPostPresent (convStmt s1) (convStmt s2)

prop_FindPostPresentCmpt :: Component -> Component -> Property 
prop_FindPostPresentCmpt c1 c2 = prop_FindPostPresent (convCmpt c1) (convCmpt c2)

prop_FindPostAbsent :: SimpleHTML String -> SimpleHTML String -> Bool
prop_FindPostAbsent k h = not (member k (delete k h))

prop_FindPostAbsentStmt :: Statement -> Statement -> Bool 
prop_FindPostAbsentStmt s1 s2 = prop_FindPostAbsent (convStmt s1) (convStmt s2)

prop_FindPostAbsentCmpt :: Component -> Component -> Bool 
prop_FindPostAbsentCmpt c1 c2 = prop_FindPostAbsent (convCmpt c1) (convCmpt c2)

-- Metamorphic Properties
prop_InsertEmpty :: SimpleHTML String -> Bool
prop_InsertEmpty k = 
    elements (Maybe.fromJust (insert k empty)) == elements k

prop_InsertEmptyStmt :: Statement -> Bool
prop_InsertEmptyStmt s = prop_InsertEmpty (convStmt s)

prop_InsertEmptyCmpt :: Component -> Bool
prop_InsertEmptyCmpt c = prop_InsertEmpty (convCmpt c)

prop_DeleteEmpty :: SimpleHTML String -> Bool
prop_DeleteEmpty k = null $ elements (delete k empty)

prop_DeleteEmptyStmt :: Statement -> Bool
prop_DeleteEmptyStmt s = prop_DeleteEmpty (convStmt s)

prop_DeleteEmptyCmpt :: Component -> Bool
prop_DeleteEmptyCmpt c = prop_DeleteEmpty (convCmpt c)

-- Model-based Property 
prop_InsertModel :: SimpleHTML String -> SimpleHTML String -> Property
prop_InsertModel k h = 
  Maybe.isJust (insert k h) ==> 
    elements (Maybe.fromJust (insert k h)) == elements h ++ elements k

prop_InsertStmt :: Statement -> Statement -> Property
prop_InsertStmt s1 s2 = prop_InsertModel (convStmt s1) (convStmt s2)

prop_InsertCmpt:: Component -> Component -> Property 
prop_InsertCmpt c1 c2 = QC.within 1000000 $ prop_InsertModel (convCmpt c1) (convCmpt c2)

-- >>> html2string $ convCmpt ut
-- "<ul><li><p>This is love </p></li><li><p>Yep, it is </p></li></ul>"

