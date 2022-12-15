module TestConverter where 
  
import Data.Maybe as Maybe ( isJust, fromJust )

import Test.HUnit (Test(..), (~?=), (~:), runTestTT)
import Test.QuickCheck  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
  )
import Test.QuickCheck as QC
    ( (==>), within, quickCheck, Property )

import MarkdownSyntax
    ( Block(Block),
      Statement(..),
      Header(H5, H4, H1, H3),
      DefItem(DI),
      Component(..),
      Markdown(..) )

import HtmlConverter
    ( SimpleHTML(PCDATA, Element),
      toHtml,
      render,
      convCmpt,
      convStmt,
      empty,
      insert,
      delete,
      member,
      elements )

-- Unit tests for convStmt
-- >>> runTestTT tStmtTest 
-- Counts {cases = 13, tried = 13, errors = 0, failures = 0}

tStmtTest :: Test
tStmtTest = "Statement test" ~: TestList [ 
  tTestBold,
  tTestItalic,
  tTestBoldItalic,
  tTestStrikethrough,
  tTestHighlight,
  tTestSub,
  tTestSup,
  tTestBacktick,
  tTestEmoji,
  tTestLink,
  tTestImage,
  tTestLineBreak,
  tTestLiteral
  ]

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
expectedTable = Element "table" [("style","border: 1px solid black;")] [
  Element "tr" [("style","border: 1px solid black;")] [
    Element "th" [("style","border: 1px solid black;")] [PCDATA "Company"],
    Element "th" [("style","border: 1px solid black;")] [PCDATA "Contact"],
    Element "th" [("style","border: 1px solid black;")] [PCDATA "Country"]
    ],

  Element "tr" [("style","border: 1px solid black;")] [
    Element "td" [("style","border: 1px solid black;")] [PCDATA "Alfreds Futterkiste"],
    Element "td" [("style","border: 1px solid black;")] [PCDATA "Maria Anders"],
    Element "td" [("style","border: 1px solid black;")] [PCDATA "Germany"]
    ],

  Element "tr" [("style","border: 1px solid black;")] [
    Element "td" [("style","border: 1px solid black;")] [PCDATA "Centro comercial Moctezuma"],
    Element "td" [("style","border: 1px solid black;")] [PCDATA "Francisco Chang"],
    Element "td" [("style","border: 1px solid black;")] [PCDATA "Mexico"]
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




-- >>> runTestTT tCmptTest
-- Counts {cases = 11, tried = 11, errors = 0, failures = 0}

tCmptTest :: Test 
tCmptTest = "Component test" ~: TestList [
  tTestHeading,
  tTestHeadingID,
  tTestParagraph,
  tTestBlockquote,
  tTestOrderedList,
  tTestUnorderedList,
  tTestTable,
  tTestDefinitionList,
  tTestCodeBlock,
  tTestHorinzontalRule,
  tTestPlain
  ]

-- >>> runTestTT tMarkdownTest
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

tMarkdownTest :: Test 
tMarkdownTest = "Markdown test" ~: TestList [
  toHtml test1 ~?= expected1,
  toHtml test2 ~?= expected2,
  toHtml test3 ~?= expected3
  ]

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

test3 :: Markdown 
test3 = Markdown [
  Heading H5 (Block [Literal "H5 Heading"]) Nothing,
  CodeBlock (Block [Literal "getDate()"])
  ]
expected3 :: SimpleHTML String
expected3 = render [Element "h5" [] [PCDATA  "H5 Heading"], Element "code" [] [PCDATA "getDate()"]]

-- Post-Condiiton Property
prop_FindPostPresent :: SimpleHTML String -> SimpleHTML String -> Property
prop_FindPostPresent k h = 
  Maybe.isJust (insert k h) ==> member k (Maybe.fromJust (insert k h))

prop_FindPostPresentStmt :: Statement -> Statement -> Property 
prop_FindPostPresentStmt s1 s2 = prop_FindPostPresent (convStmt s1) (convStmt s2)

prop_FindPostPresentCmpt :: Component -> Component -> Property 
prop_FindPostPresentCmpt c1 c2 = prop_FindPostPresent (convCmpt c1) (convCmpt c2)

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

runTests :: IO ()
runTests = do 
  putStrLn "Test converting statements"
  _ <- runTestTT tStmtTest 
  putStrLn "Test converting components"
  _ <- runTestTT tCmptTest 
  putStrLn "Test converting markdown input"
  _ <- runTestTT tMarkdownTest

  putStrLn "quickCheck prop_FindPostPresentStmt"
  quickCheck prop_FindPostPresentStmt

  putStrLn "quickCheck prop_FindPostPresentCmpt"
  quickCheck prop_FindPostPresentCmpt

  putStrLn "quickCheck prop_InsertEmptyStmt"
  quickCheck prop_InsertEmptyStmt

  putStrLn "quickCheck prop_InsertEmptyCmpt"
  quickCheck prop_InsertEmptyCmpt

  putStrLn "quickCheck prop_DelettEmptyStmt"
  quickCheck prop_DeleteEmptyStmt

  putStrLn "quickCheck prop_DeleteEmptyCmpt"
  quickCheck prop_DeleteEmptyCmpt

  putStrLn "quickCheck prop_InsertStmt"
  quickCheck prop_InsertStmt 

  putStrLn "quickCheck prop_InsertCmpt"
  quickCheck prop_InsertCmpt
