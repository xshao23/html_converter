module MarkdownSyntax where 

import Control.Monad (liftM2)
import Data.List 
import Data.Foldable qualified as Foldable

import qualified Data.Char as Char
import Data.Char
import qualified Data.Set as Set (fromList, member)
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Text.PrettyPrint (Doc, (<+>))
import qualified Test.QuickCheck as QC

newtype Markdown = Markdown [Component]
  deriving (Eq, Show)

-- See https://www.markdownguide.org/basic-syntax
data Component
  = Heading Header Block
  | Paragraph Block
  | Blockquote [Component]
  | OrderedList [Item]
  | UnorderedList [Item]
  | TaskList [TaskItem] 
  | CodeBlock String
  | HorizontalRule 
  | Newline
  | Plain String -- no open/close tags associated
  deriving (Eq, Show)

newtype Block = Block [Statement]
  deriving (Eq, Show)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

type Item = [Component]

newtype TaskItem = Bool [Component]
  deriving (Eq, Show)

data Statement
    = Bold Block 
    | Italic Block 
    | Strikethrough Block
    | Backtick String 
    | Emoji String
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
    deriving (Eq, Show)

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
    

genComponent :: Int -> Gen Component
genComponent n | n <= 1 = QC.oneof [Plain <$> genStringLit, return HorizontalRule]
genComponent n =
  QC.frequency
    [ (1, Plain <$> genStringLit),
      (1, return HorizontalRule),
      (n, Heading <$> genHeader <*> genBlock n'),
      (n, Paragraph <$> genBlock n'),
      -- generate loops half as frequently as if statements
      (n, Blockquote <$> genCmpts n'),
      (n, UnorderedList <$> genItems n'),
      (n, OrderedList <$> genItems n')
    ]
  where
    n' = n `div` 2

genStatement :: Int -> Gen Statement
genStatement n | n <= 1 = 
  QC.oneof [
    Literal <$> genStringLit, 
    Backtick <$> genStringLit, 
    Image <$> genStringLit <*> genStringLit <*> genMaybe,
    return LineBreak
    ]

genStatement n = 
  QC.frequency [
    (1, Literal <$> genStringLit),
    (1, Backtick <$> genStringLit),
    (1, Image <$> genStringLit <*> genStringLit <*> genMaybe),
    (1, return LineBreak),
    (n, Bold <$> genBlock n'),
    (n, Italic <$> genBlock n'),
    (n, Link <$> genBlock n' <*> genStringLit <*> genMaybe)
    ]
  where
    n' = n `div` 2

genMaybe :: Gen (Maybe String)
genMaybe = QC.oneof [return Nothing, Just <$> genStringLit]

genItem :: Int -> Gen Item 
genItem 0 = pure [] 
genItem n = (:) <$> genComponent n <*> genCmpts (n `div` 2)

genItems :: Int -> Gen [Item]
genItems 0 = pure [] 
genItems n = (:) <$> genItem n <*> genItems (n `div` 2)


genCmpts :: Int -> Gen [Component]
genCmpts 0 = pure []
genCmpts n =
      QC.frequency
        [ (1, return []),
          (n, (:) <$> genComponent n' <*> genCmpts n')
        ]
      where
        n' = n `div` 2

genStmts :: Int -> Gen [Statement]
genStmts 0 = pure []
genStmts n =
      QC.frequency
        [ (1, return []),
          (n, (:) <$> genStatement n' <*> genStmts n')
        ]
      where
        n' = n `div` 2

genBlock :: Int -> Gen Block 
genBlock n = Block <$> genStmts n

instance Arbitrary Component where
  arbitrary = QC.sized genComponent


instance Arbitrary Statement where 
  arbitrary = QC.sized genStatement

instance Arbitrary Block where
  arbitrary = QC.sized genBlock 
  
sampleBlock :: IO ()
sampleBlock = QC.sample' (arbitrary :: Gen Block) >>= mapM_ print 

sampleStatement :: IO ()
sampleStatement = QC.sample' (arbitrary :: Gen Statement) >>= mapM_ print 

sampleComponent :: IO ()
sampleComponent = QC.sample' (arbitrary :: Gen Component) >>= mapM_ print 

