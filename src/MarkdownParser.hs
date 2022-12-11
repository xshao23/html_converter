{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}

module MarkdownParser where

import Control.Applicative
import Data.Char (isNumber)
import MarkdownSyntax
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck as QC ()

-- The function below will be called by IOHandler (borrowed from parseLuFile)
-- Note that Markdown file consists of many components (in parallel)
-- See MarkdownSyntax line 12
parseMarkdownFile :: String -> IO (Either P.ParseError Markdown)
parseMarkdownFile = P.parseFromFile (const <$> markdownP <*> P.eof)

-- 下面四个是我看着LuParser加上的，如果后来你发现不需要就删掉吧。
markdownP :: Parser Markdown
markdownP = Markdown <$> many componentP

parseMarkdownCmpt :: String -> Either P.ParseError Component
parseMarkdownCmpt = P.parse componentP

parseMarkdownStmt :: String -> Either P.ParseError Statement
parseMarkdownStmt = P.parse statementP

-- | Skip whitespace
wsP :: Parser a -> Parser a
-- wsP p = many P.space *> p
wsP p = p <* many P.space

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

-- | Parse only a particular string
stringP :: String -> Parser ()
stringP s = wsP $ P.string s *> pure ()

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a"
        ~?= Right
          [(), ()]
    ]

-- | Combination string and whitespace parser
constP :: String -> a -> Parser a
constP s newVal = stringP s *> pure newVal

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

componentP :: Parser Component
componentP = undefined

itemP :: Parser Item
itemP = undefined

taskItemP :: Parser TaskItem
taskItemP = undefined

headerP :: Parser Header
headerP = undefined

headingP :: Parser Component
headingP = undefined

paragraphP :: Parser Component
paragraphP = undefined

blockquoteP :: Parser Component
blockquoteP = undefined

orderedListP :: Parser Component
orderedListP = undefined

unorderedListP :: Parser Component
unorderedListP = undefined

codeBlockP :: Parser Component
codeBlockP = undefined

horizontalRuleP :: Parser Component
horizontalRuleP = undefined

newlineP :: Parser Component
newlineP = undefined

plainP :: Parser Component
plainP = undefined

blockP :: Parser Block
blockP = Block <$> many statementP

-- Statement Parsers --
statementP :: Parser Statement
statementP = strikethroughP <|> boldP <|> italicP <|> imageP <|> linkP <|> backtickP <|> linebreakP <|> emojiP <|> literalP

boldP :: Parser Statement
boldP = boldStarP <|> boldUnderscoreP

boldStarP :: Parser Statement
boldStarP = Bold <$> (many P.space *> P.between (stringP "**") blockP (stringP "**"))

boldUnderscoreP :: Parser Statement
boldUnderscoreP = Bold <$> (many P.space *> P.between (stringP "__") blockP (stringP "__"))

italicP :: Parser Statement
italicP = italicStarP <|> italicUnderscoreP

italicStarP :: Parser Statement
italicStarP = Italic <$> (many P.space *> P.between (P.char '*') blockP (P.char '*'))

italicUnderscoreP :: Parser Statement
italicUnderscoreP = Italic <$> (many P.space *> P.between (P.char '_') blockP (P.char '_'))

backtickP :: Parser Statement
backtickP = Backtick <$> (many P.space *> P.between (P.char '`') (many $ P.satisfy (/= '`')) (P.char '`'))

-- >>> P.doParse backtickP "`code`"

linkP :: Parser Statement
linkP = link3P <|> link2P

link2P :: Parser Statement
link2P =
  Link2
    <$> (many P.space *> P.between (P.char '[') blockP (P.char ']'))
    <*> P.between (P.char '(') (many $ P.satisfy (/= ')')) (P.char ')')

link3P :: Parser Statement
link3P =
  Link3
    <$> (many P.space *> P.between (P.char '[') blockP (P.char ']'))
    <*> imageTupP

imageP :: Parser Statement
imageP = image3P <|> image2P

-- >>> P.doParse literalP "a"
-- Just (Literal "a","")

-- >>> P.doParse imageP "![Tux, the Linux mascot](/assets/images/tux.png)\"a\""
-- Just (Image2 "Tux, the Linux mascot" "/assets/images/tux.png","\"a\"")

image2P :: Parser Statement
image2P =
  Image2
    <$> (many P.space *> P.char '!' *> P.between (P.char '[') (many $ P.satisfy (/= ']')) (P.char ']'))
    <*> P.between (P.char '(') (many $ P.satisfy (/= ')')) (P.char ')')

image3P :: Parser Statement
image3P =
  Image3
    <$> (many P.space *> P.char '!' *> P.between (P.char '[') (many $ P.satisfy (/= ']')) (P.char ']'))
    <*> imageTupP

-- <* many P.space
imageTupP :: Parser (String, String)
imageTupP =
  (,)
    <$> (P.char '(' *> many (P.satisfy (/= '"')))
    <*> (P.between (P.char '\"') (many $ P.satisfy (/= '\"')) (P.char '\"') <* P.char ')')

-- >>> P.doParse (many P.space *> P.char '!' ) "![Tux, the Linux mascot]"
-- Just ('!',"[Tux, the Linux mascot]")

-- >>> P.doParse imageP' "![Tux](/a/b/c)"
-- Just (Image2 "Tux" "/a/b/c","")

-- >>> P.doParse imageTupP "(/a/b/c \"title\")"
-- Just (("/a/b/c ","title"),"")

linebreakP :: Parser Statement
linebreakP = constP "<br>" LineBreak

-- | TODO: Double check the correctness after other methods finished
literalP :: Parser Statement
literalP = Literal <$> many (P.satisfy (/= '"')) <* many P.space

emojiP :: Parser Statement
emojiP = Emoji <$> P.between (P.char ':') (many $ P.satisfy (/= ':')) (P.char ':') <* many P.space

strikethroughP :: Parser Statement
strikethroughP = Strikethrough <$> (P.between (stringP "~~") blockP (stringP "~~") <* many P.space)

stiketr :: Statement
-- ProgressCancelledException
stiketr = Strikethrough (Block [Literal "  abc  "])

-- >>> P.doParse strikethroughP "~~ abc ~~"

strikethroughP' :: Parser Statement
strikethroughP' = Strikethrough <$> (P.between (stringP "~~") blockP' (stringP "~~") <* many P.space)

blockP' :: Parser Block
blockP' = Block <$> many literalP

-- >>> P.doParse strikethroughP'  "~~  abc  ~~"
