{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

module MarkdownParser where 

import Control.Applicative hiding ((<|>), many, optional)
import Data.Char (isNumber, isSpace, isAlpha, isAlphaNum)
import Data.List.Split hiding (oneOf)
import Data.Functor
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck as QC
import Data.Set (Set)
import Control.Monad (guard, when, join)
import qualified Data.Set as Set (fromList, member)

import qualified System.IO as IO
import qualified System.IO.Error as IO
import Debug.Trace
import Text.Parsec.Char
import Text.ParserCombinators.Parsec hiding (runParser, between, parseFromFile)
import Text.Parsec.Pos
import MarkdownSyntax
import Text.Parsec.Error (newErrorUnknown)
import Text.Parsec (runParserT)

-- The function below will be called by IOHandler (borrowed from parseLuFile)
-- Note that Markdown file consists of many components (in parallel)
-- See MarkdownSyntax line 12
parseMarkdownFile :: String -> IO (Either ParseError Markdown)
parseMarkdownFile = parseFromFile (const <$> markdownP <*> eof)

type ParseError1 = String 

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do
  IO.catchIOError
    (do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ doParse parser str)
    (\e ->
        pure $ Left $ newErrorUnknown (initialPos "file IO error")
    )

markdownP :: Parser Markdown 
markdownP = Markdown <$> many componentP

blockP :: Parser Block 
blockP = Block <$> many statementP

strip :: String -> String
strip [] = []
strip [' '] = []
strip (x:xs) = x:strip xs

wsp :: Parser String
wsp = many (char ' ')

someTill :: Parser a -> Parser end -> Parser [a]
someTill p end = liftA2 (:) p (manyTill p end)

surround :: Parser start -> Parser end -> Parser Block
surround start end = start *> (Block <$> someTill statementP (try end))

between ::  String -> String -> Parser Block
between start end = surround (string start) (string end)

boldP ::   Parser Statement
boldP  = Bold <$> (between "**" "**" <|> try (between "__" "__"))

italicP :: Parser Statement
italicP = Italic <$> (single '*' <|> single '_')

strikethroughP ::Parser Statement 
strikethroughP = Strikethrough <$> between "~~" "~~"

highlightP :: Parser Statement 
highlightP = Highlight <$> between "==" "==" 
{-
char' :: Char -> Parser Char
char' c = satisfy (c ==)

betweenS :: Parser open -> Parser a -> Parser close -> Parser a
betweenS open p close = open *> p <* close

stringValP :: Parser String
stringValP = some anyChar

stringP :: String -> Parser ()
stringP s = string s *> many space *> pure ()

blks :: Parser a -> Parser a
blks x = betweenS (string "```") x (string "```")

cP :: Parser Component 
cP = CodeBlock <$> blks stringValP
-}
-- >>> doParse cP "``` abc ``` ```"
-- Left (line 1, column 16):
-- unexpected end of input
-- expecting "```"


subP :: Parser Statement 
subP = Sub <$> single '~'

supP :: Parser Statement 
supP = Sup <$> single '^'

emojiP :: Parser Statement
emojiP = char ':' *> (
  Emoji . toUnicode . dropWspOnly <$> some (
    notFollowedBy (endOfLine *> wsp *> endOfLine) *> satisfy (not . emojiisReserved))
  ) <* char ':'

toUnicode :: String -> String
toUnicode "lion" = "129409"
toUnicode "joy" = "128516"
toUnicode "devil" = "128520"
toUnicode "smily" = "128522"
toUnicode "cool" = "128526"
toUnicode _ = ""

-- >>> doParse emojiP ":joy:"
-- Right (Emoji "U+1F603")

single :: Char -> Parser Block
single ch = surround 
  (satisfy (==ch)) 
  (try (lookAhead (notFollowedBy $ between [ch, ch] [ch, ch]) *> satisfy(==ch)))

statementP :: Parser Statement
statementP = notFollowedBy (endOfLine *> wsp *> endOfLine) *> choice [
  try backtickP, 
  try imageP, 
  --try autoLinkP, 
  try linkP,
  try boldP, 
  try italicP, 
  try strikethroughP, 
  try highlightP,
  try subP,
  try supP,
  try lineBreakP,
  try emojiP,
  try literalP <|> Literal . (:[]) <$> (try (endOfLine <* wsp) <|> oneOf reserved)
  ]

lineBreakP ::  Parser Statement
lineBreakP  = do
  try $ string "\\" <|> (count 2 (char ' ') <* many (char ' '))
  try $ lookAhead $ char '\n' <* many (char ' ') <* satisfy (/= '\n')
  return LineBreak

backtickP :: Parser Statement
backtickP = Backtick <$> try (string "`" *> manyTill anyChar (string "`"))

codeblockP :: Parser Component
codeblockP = do
      s <- try (string "```\n" *> manyTill anyChar (string "```"))
      return $ CodeBlock (Block (map Literal (splitOn "\n" s) ))

ss = splitOn "\n" "abc \n  def()"

sTest = "```\nabc\ndef()  ```"

-- >>> doParse codeblockP sTest
-- Right (CodeBlock (Block [Literal "abc",Literal "def()  "]))

-- >>> ss
-- ["abc ","  def()"]

-- >>> doParse codeblockP "```\nabc  \n   def()```"
-- Right (CodeBlock (Block [Literal "abc  ",Literal "   def()"]))

titleP :: Parser (String, Maybe String)
titleP = char '(' *> many (char ' ') *> p <* many (char ' ') <* char ')'
  where              
    p = (,) <$> destLinkP <*> titleP
    destLinkP :: Parser String
    destLinkP = try (char '<' *> manyTill (satisfy (/= '\n')) (char '>')) 
                <|> many (satisfy (\c -> c /= ' ' && c /= ')' && c /= '\n'))
    titleP :: Parser (Maybe String)
    titleP = try (Just <$> (some (char ' ') *> try wrappedTitleP)) <|> Nothing <$ (many (char ' ') *> string "")
    wrappedTitleP :: Parser String
    wrappedTitleP = choice [
      char '"' *> manyTill anyChar  (char '"'),
      char '\'' *> manyTill anyChar  (char '\''),
      char '(' *> manyTill anyChar  (char ')')
      ]

linkP :: Parser Statement
linkP = (\text (link, title) -> Link text link title)
    <$> (char '[' *> (Block <$> manyTill statementP' (char ']'))) --(char '[' *> (combineStmts . Block <$> manyTill statementP' (char ']')))
    <*> titleP
  where
    statementP' :: Parser Statement
    statementP' = try $ do
                  --notFollowedBy autoLinkP
                  notFollowedBy linkP 
                  statementP

imageP :: Parser Statement
imageP = (\alt (link, title) -> Image alt link title)
         <$> (string "![" *>  manyTill anyChar (char ']'))
         <*> titleP

reserved :: String
reserved = "*_`)[] <!#\n\\~\\^\\="

isReserved :: Char -> Bool
isReserved c = c `elem` reserved

emojiisReserved :: Char -> Bool 
emojiisReserved c = c `elem` (reserved ++ ":")

combineStmts :: Block -> Block
combineStmts (Block []) = Block []
combineStmts (Block [x]) = Block [x]
combineStmts (Block (s1 : s2 : ss)) = case (s1, s2) of
  (Literal a, Literal b@"\n") -> combineStmts $ Block (Literal (strip a ++ b) : ss)
  (Literal a, Literal b) -> combineStmts $ combineStr Literal a b ss
  (Bold b1, Bold b2) -> combineStmts $ combineBlk Bold b1 b2 ss
  (Italic b1, Italic b2) -> combineStmts $ combineBlk Italic b1 b2 ss
  (Strikethrough b1, Strikethrough b2) -> combineStmts $ combineBlk Strikethrough b1 b2 ss
  (Highlight b1, Highlight b2) -> combineStmts $ combineBlk Highlight b1 b2 ss
  (Sub b1, Sub b2) -> combineStmts $ combineBlk Sub b1 b2 ss
  (Sup b1, Sup b2) -> combineStmts $ combineBlk Sup b1 b2 ss
  (Backtick a, Backtick b) -> combineStmts $ combineStr Backtick a b ss
  (Emoji a, Emoji b) -> combineStmts $ combineStr Emoji a b ss
  _ -> Block [s1] <> combineStmts (Block (s2 : ss))

combineBlk :: (Block -> Statement) -> Block -> Block -> [Statement] -> Block 
combineBlk f x y xs = Block (f (combineStmts (x <> y)) : xs)

combineStr :: (String -> Statement) -> String -> String -> [Statement] -> Block 
combineStr f x y xs = Block (f (x ++ y) : xs)

literalP :: Parser Statement
literalP = Literal . dropWspOnly <$> some (
  notFollowedBy (endOfLine *> wsp *> endOfLine) *> satisfy (not . isReserved)
  )

dropWspOnly s = reverse (dropS (reverse s)) where
  dropS ls@(x:xs) = if x == ' ' then dropS xs else ls
  dropS [] = []

{-
autoLinkP :: Parser Statement
autoLinkP = (\s -> Link (Block [Literal s]) s Nothing) <$> p
  where
    p :: Parser String
    p = char '<' *> ((++) <$> prefixP <*> (notFollowedBy backtickP >> many urlChar <* char '>'))
    prefixP :: Parser String
    prefixP = (\a b c -> a:b++[c])
                <$> satisfy isAlpha
                <*> some (satisfy (\c -> isAlphaNum c || c == '.' || c == '+' || c == '-'))
                <*> char ':'
    urlChar :: Parser Char
    urlChar = satisfy (`Set.member` whiteList)
    whiteList :: Set Char
    whiteList =  Set.fromList ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
                            ++ "abcdefghijklmnopqrstuvwxyz"
                            ++ "0123456789-._~:/?#[]@!$&'()*+,;=")
-}
doParse :: Parser a -> String -> Either ParseError a
doParse p = parse p ""

headingP :: Parser Component
headingP = do
  sps <- wsp
  hLevel <- some (char '#') 
  guard (length hLevel < 7 && not (null hLevel))
  hText <- some (char ' ') *> blockP
  return (Heading (getHeader (length hLevel)) hText)

blockquoteP :: Parser Component 
blockquoteP = do 
  ss <- some blockquoteP'
  return $ Blockquote ss

blockquoteP' :: Parser Block
blockquoteP' = do
  s <- try (string "> " *> manyTill anyChar (string "\n"))
  case doParse blockP s of 
    Left _ -> return (Block [])
    Right b -> return b
  
-- >>> doParse blockP "This is love"
-- Right (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "love"])

-- >>> doParse blockquoteP' "> This is love\n"
-- Right (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "love"])

-- >>> doParse blockquoteP "> ***This is love***\n> ~~Yep, it is~~ \n"
-- Right (Blockquote [Block [Bold (Block [Italic (Block [Literal "This",Literal " ",Literal "is",Literal " ",Literal "love"])])],Block [Strikethrough (Block [Literal "Yep,",Literal " ",Literal "it",Literal " ",Literal "is"]),Literal " "]])


-- >>> doParse blockquoteP "> **A**\n> B\n"
-- Right (Blockquote [Block [Bold (Block [Literal "A"])],Block [Literal "B"]])

-- >>> doParse blockquoteP "> How do I love thee? Let me count the ways.  \n> I love thee to the depth and breadth and height  \n"
-- Right (Blockquote (Block [Literal "How do I love thee? Let me count the ways.  ",Literal "I love thee to the depth and breadth and height  "]))

-- >>> doParse blockquoteP' "> How \n> "
-- Left (line 1, column 6):
-- unexpected " "
-- expecting new-line or "\n"

{-
codeblockP :: Parser Component
codeblockP = do
      s <- try (string "```\n" *> p (string "```"))
      return $ CodeBlock (Block (map Literal (splitOn "\n" s) ))
  where
    p :: Parser a -> Parser String
    p = manyTill (notFollowedBy (string "\n\n\n") >> anyChar)
-}

paragraphP :: Parser Component
paragraphP = Paragraph . Block <$>
  (try (
    someTill statementP (
      try (endOfLine *> endOfLine) <|> lookAhead (endOfLine <* wsp <* lookAhead stopP)
    )
  ) <|> some statementP)

stopP :: Parser Component
stopP = try hrP <|> try headingP <|> try listP

hrP :: Parser Component
hrP = wsp *> (hrP' '*' <|> hrP' '-' <|> hrP' '_')
  where 
    hrP' :: Char -> Parser Component 
    hrP' c = do
      t <- count 3 (char c <* wsp)
      i <- many (char (head t) <* wsp) *> optionMaybe (satisfy (/='\n'))
      {-
      case i of
        Just _ -> fail "Unrecognized horizontal rules"
        Nothing -> return ()
        -}
      return HorizontalRule

plainP :: Parser Component 
plainP = Plain <$> statementP

indentedCmptP :: Int -> Parser Component
indentedCmptP i = do
  -- _ <- lookAhead (wsp *> noneOf "-*+")
  sps <- try (count i (char ' '))
  s <- try componentP
  _ <- many (try (wsp *> endOfLine))
  return s

{-
- A
- B
  - C 
  - D
-}

-- 0 
-- 2  
--  "- A\n  - 1\n"
-- [Component] :: [A, UnorderedList "1" ] 

listP :: Parser Component
listP = unorderedListP <|> orderedListP

unorderedListP :: Parser Component
unorderedListP = do 
  item <- some (try uiP) 
  -- item <- unorderedItemsP
  return (UnorderedList item)

uiP :: Parser Item
uiP = do
  sps <- lookAhead (wsp <* oneOf "-*+")
  sps2 <- try (wsp *> oneOf "-*+" *> some (char ' '))
  c <- componentP
  _ <- many (try (wsp *> endOfLine))
  -- _ <- trace ("DEBUG: indentation depth" ++ show (length sps + length sps2  +1)) (length sps + length sps2  +1)
  cs <- many (indentedCmptP (length sps + length sps2 + 1))
  return (c : cs)

-- >>> doParse unorderedItemsP "- A\n- B\n  - C\n  - D\n"
-- Right [[Paragraph (Block [Literal "A"])],[Paragraph (Block [Literal "B"]),UnorderedList [[Paragraph (Block [Literal "C"]),Paragraph (Block [Literal "D",Literal "\n"])]]]]

unorderedListP' :: Parser Component
unorderedListP' = do 
  li <- many (unorderedItemP 0)
  traceM $ "li is " ++ show li 
  if null li 
    then return (Plain (Literal ""))
    else return (UnorderedList li)

unorderedItemsP :: Parser [Item]
unorderedItemsP  = many (unorderedItemP 0)

unorderedItemP :: Int -> Parser Item
unorderedItemP i = do 
  sp <- manyTill (char ' ') (try (oneOf "+-*"))
  traceM("sp is " ++ show sp )
  --guard (length sp == i)
  _ <- try wsp -- consume white space 
  c <- componentP <* many (try (wsp *> endOfLine))
  traceM ("component c is " ++ show c)
  -- cs <- try (many (unorderedCmptP (i + 2))) -- cs :: [Component] 
  -- determine if we call unorderedCmptP i `
  sp' <- lookAhead (manyTill (char ' ') (oneOf "+-*") <|> many endOfLine )  -- Need to handle eol here 
  traceM ("sp' before guard is " ++ show sp)
  if length sp' == (i + 2) 
    then do 
      cs <- many (unorderedCmptP (i + 2))
      return $ c : [UnorderedList [cs]]
    else 
      return [c]

unorderedCmptP :: Int -> Parser Component 
unorderedCmptP i = do 
  _ <- manyTill (char ' ') (try (oneOf "+-*"))
  _ <- try wsp -- consume white space 
  componentP <* many (try (wsp *> endOfLine))

-- >>> doParse (unorderedItemP 0) "- A\n- B\n"
-- Prelude.undefined

-- >>> doParse (unorderedItemP 0) "- A\n  - B\n  - C\n  - D\n"
-- Prelude.undefined


-- >>> doParse (unorderedItemP 0) "- A\n"
-- Right [Paragraph (Block [Literal "A",Literal "\n"]),UnorderedList [[]]]


-- >>> doParse (unorderedItemP 0) "- A\n- B"
-- Left (line 2, column 1):
-- unexpected "-"
-- expecting " "

oiP :: Parser Item
oiP = do
  sps <- lookAhead (wsp <* many digit <* char '.')
  num <- lookAhead (many digit <* char '.')
  sps2 <- try (wsp *> (many digit <* char '.') *> some (char ' '))
  c <- componentP
  _ <- many (try (wsp *> endOfLine))
  cs <- many (orderedIndentedBlockP (length sps + length sps2 + length num + 1)) 
  return (c : cs)

orderedIndentedBlockP :: Int -> Parser Component
orderedIndentedBlockP i = do
  _ <- lookAhead (wsp *> notFollowedBy (many digit <* char '.'))
  indentedCmptP i

orderedListP :: Parser Component
orderedListP = do
  start <- lookAhead (many digit <* char '.')
  items <- some (try oiP)
  return (OrderedList items)

componentP :: Parser Component
componentP = (
  try hrP 
  <|> try codeblockP
  <|> try blockquoteP
  <|> try headingP 
  <|> try listP 
  <|> try paragraphP
  <|> try plainP
  ) <* optional (many endOfLine)

-- | pass in previous level depth

itemP :: Int -> Parser Item
itemP i = do 
  _ <- lookAhead eof
  _ <- try (count i wsp)
  _ <- try (oneOf "-*+" *> some (char ' ')) -- keep 
  c <- try componentP
  _ <- many (try (wsp *> endOfLine))
  cs <- try (count 2 (char ' ') *> itemP (i + 2)) <|> itemP i
  return (c : cs)  -- sps2 <- try (count 2 (char ' ')) 
  
-- >>> doParse (itemP 0) "- A\n- B\n" 
-- Left (line 1, column 1):
-- unexpected '-'
-- expecting end of input


cmptP :: Int -> Parser Component 
cmptP i = undefined

-- >>> doParse (strP 0) "- A\n- B\n"
-- Left (line 1, column 1):
-- unexpected '-'
-- expecting end of input


-- >>> doParse (itemP 0) "- A\n- B"
-- Left (line 1, column 2):
-- unexpected " "

{-
- A 
- B 
  - C 
  - D 
-}

