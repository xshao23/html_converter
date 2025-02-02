module MarkdownSyntax where 

import qualified Data.Char as Char
import Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck as QC

newtype Markdown = Markdown [Component]
  deriving (Eq, Show)

-- See https://www.markdownguide.org/basic-syntax
data Component
  = Heading Header Block (Maybe String)
  | Paragraph Block -- <p>
  | Blockquote [Block] -- <blockquote>
  | OrderedList [Item] -- <ol>
  | UnorderedList [Item] -- <ul>
  | Table [Row]
  | DefinitionList [DefItem]
  | CodeBlock Block -- <code>
  | HorizontalRule -- <hr/>
  | Newline -- <br/>
  | Plain Block -- no component-level open/close tags associated
  deriving (Eq, Show)

type Item = [Component]

type Row = [Col] 
type Col = Component

data DefItem = DI Component [Component]
  deriving (Eq, Show)

newtype Block = Block [Statement]
  deriving (Eq, Show)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

data Statement
  = Bold Block -- <strong>
  | Italic Block -- <em>
  | Strikethrough Block -- <s>
  | Highlight Block
  | Sub Block 
  | Sup Block
  | Backtick String -- <code>
  | Emoji String -- &#...;
  | Link Block String (Maybe String) -- optional title  
  | Image String String (Maybe String) -- alt src optional title
  | LineBreak -- <br>
  | Literal String
  deriving (Eq, Show)
    
data Header
    = H1
    | H2 
    | H3 
    | H4 
    | H5 
    | H6 
    deriving (Eq)

instance Show Header where 
  show H1 = "h1"
  show H2 = "h2"
  show H3 = "h3"
  show H4 = "h4"
  show H5 = "h5"
  show H6 = "h6"

-- Below is the generators that will be later used for property check

genHeader :: Gen Header
genHeader = getHeader <$> QC.choose(1, 6)

getHeader :: Int -> Header 
getHeader 1 = H1 
getHeader 2 = H2 
getHeader 3 = H3 
getHeader 4 = H4 
getHeader 5 = H5 
getHeader 6 = H6
getHeader _ = H6

genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    -- escape special characters appearing in the string,
    escape :: String -> String
    escape = foldr Char.showLitChar ""
    -- generate strings containing printable characters or spaces, but not including '\"'
    stringLitChars :: [Char]
    stringLitChars = filter (\c -> c /= '\"' && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~']
    

genCmpt :: Int -> Gen Component
genCmpt n | n <= 1 = QC.oneof [
  return Newline, 
  return HorizontalRule
  ]
genCmpt n =
  QC.frequency
    [ (1, return Newline),
      (1, return HorizontalRule),
      -- generate loops half as frequently as if statements
      (n, Heading <$> genHeader <*> genBlock n' <*> genMaybe),
      (n, Paragraph <$> genBlock n'),
      (n, Blockquote <$> genBlocks n'),
      (n, UnorderedList <$> genItems n'),
      (n, OrderedList <$> genItems n'),
      (n, Table <$> genRows n'),
      (n, DefinitionList <$> genDefItems n'),
      (n, CodeBlock <$> genBlock n'),
      (n, Plain <$> genBlock n')
    ]
  where
    n' = n `div` 2

genCmpts :: Int -> Gen [Component]
genCmpts 0 = pure []
genCmpts n =
      QC.frequency
        [ (1, return []),
          (n, (:) <$> genCmpt n' <*> genCmpts n')
        ]
      where
        n' = n `div` 2

genBlock :: Int -> Gen Block 
genBlock n = Block <$> genStmts n

genBlocks :: Int -> Gen [Block]
genBlocks n = QC.frequency [
  (1, return []),
  (n, (:) <$> genBlock n' <*> genBlocks n')
  ]
  where n' = n `div` 2

genStmt :: Int -> Gen Statement
genStmt n | n <= 1 = 
  QC.oneof [
    Literal <$> genStringLit, 
    Backtick <$> genStringLit, 
    Image <$> genStringLit <*> genStringLit <*> genMaybe,
    return LineBreak
    ]
genStmt n = 
  QC.frequency [
    (1, Literal <$> genStringLit),
    (1, Backtick <$> genStringLit),
    (1, Emoji <$> genStringLit),
    (1, Image <$> genStringLit <*> genStringLit <*> genMaybe),
    (1, return LineBreak),
    (n, Bold <$> genBlock n'),
    (n, Italic <$> genBlock n'),
    (n, Strikethrough <$> genBlock n'),
    (n, Highlight <$> genBlock n'),
    (n, Sub <$> genBlock n'),
    (n, Sup <$> genBlock n'),
    (n, Link <$> genBlock n' <*> genStringLit <*> genMaybe)
    ]
  where
    n' = n `div` 2

genStmts :: Int -> Gen [Statement]
genStmts 0 = pure []
genStmts n =
      QC.frequency
        [ (1, return []),
          (n, (:) <$> genStmt n' <*> genStmts n')
        ]
      where
        n' = n `div` 2

-- | Some helper generators below
genBool :: Gen Bool
genBool = QC.oneof [return False, return True]

genMaybe :: Gen (Maybe String)
genMaybe = QC.oneof [return Nothing, Just <$> genStringLit]

genItem :: Int -> Gen Item 
genItem 0 = pure [] 
genItem n = QC.frequency [
  (1, return []),
  (n, (:) <$> genCmpt n <*> genCmpts (n `div` 2))
  ]

genItems :: Int -> Gen [Item]
genItems 0 = pure [] 
genItems n = QC.frequency [
  (1, return []),
  (n, (:) <$> genItem n <*> genItems (n `div` 2))
  ] 

genRows :: Int -> Gen [Row] 
genRows 0 = pure [] 
genRows n = QC.frequency [
  (1, return []),
  (n, (:) <$> genItem n <*> genRows (n `div` 2))
  ] 

genDefItem :: Int -> Gen DefItem 
genDefItem 0 = DI <$> genCmpt 0 <*> pure []
genDefItem n = QC.frequency [
  (1, DI <$> genCmpt 0 <*> pure []),
  (n, DI <$> genCmpt n <*> genCmpts (n `div` 2))
  ]

genDefItems :: Int -> Gen [DefItem]
genDefItems 0 = pure [] 
genDefItems n = QC.frequency [
  (1, return []),
  (n, (:) <$> genDefItem n <*> genDefItems (n `div` 2))
  ]

instance Arbitrary Component where
  arbitrary = QC.sized genCmpt

instance Arbitrary Block where
  arbitrary = QC.sized genBlock 

instance Arbitrary Statement where 
  arbitrary = QC.sized genStmt

sampleStatement :: IO ()
sampleStatement = QC.sample' (arbitrary :: Gen Statement) >>= mapM_ print 

sampleBlock :: IO ()
sampleBlock = QC.sample' (arbitrary :: Gen Block) >>= mapM_ print 

sampleComponent :: IO ()
sampleComponent = QC.sample' (arbitrary :: Gen Component) >>= mapM_ print 

