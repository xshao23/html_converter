{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

module MarkdownParser where 

import Control.Applicative hiding ((<|>), many, optional)
import Data.Char (isNumber, isSpace, isAlpha, isAlphaNum)
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

-- | Skip whitespace 
-- wsP :: Parser a -> Parser a
-- -- wsP p = many P.space *> p
-- wsP p = p <* many P.space

-- test_wsP :: Test
-- test_wsP =
--   TestList
--     [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
--       P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
--     ]

-- | Parse only a particular string 
-- stringP :: String -> Parser ()
-- stringP s = wsP $ P.string s *> pure ()

-- test_stringP :: Test
-- test_stringP =
--   TestList
--     [ P.parse (stringP "a") "a" ~?= Right (),
--       P.parse (stringP "a") "b" ~?= Left "No parses",
--       P.parse (many (stringP "a")) "a  a"
--         ~?= Right
--           [(), ()]
--     ]
-- | Combination string and whitespace parser
-- constP :: String -> a -> Parser a
-- constP s newVal = stringP s *> pure newVal

-- test_constP :: Test
-- test_constP =
--   TestList
--     [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
--       P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
--     ]

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

italicsP :: Parser Statement
italicsP = Italic <$> (single '*' <|> single '_')

strikethroughP ::Parser Statement 
strikethroughP = Strikethrough <$> between "~~" "~~"

highlightP :: Parser Statement 
highlightP = Highlight <$> between "==" "==" 

subP :: Parser Statement 
subP = Sub <$> single '~'

supP :: Parser Statement 
supP = Sup <$> single '^'

-- emojiP :: Bool -> Parser Statement
-- emojiP isPar = Emoji <$> between isPardropWspOnly <$> some (
--   notFollowedBy (endOfLine *> wsp *> endOfLine) *> satisfy (not . isReserved)
--   )

emojiP :: Parser Statement
emojiP = char ':' *> (Emoji . dropWspOnly <$> some (
    notFollowedBy (endOfLine *> wsp *> endOfLine)
    *> satisfy (not . emojiisReserved))) <* char ':'

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
  try italicsP, 
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
backtickP = Backtick 
        <$> (try (string "``" *> p (string "``"))
        <|> (char '`' *> p (char '`')))
  where
    p :: Parser a -> Parser String
    p = manyTill (notFollowedBy (string "\n\n") >> f <$> anyChar)
    f x = if x=='\n' then ' ' else x

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


uiP :: Parser Item
uiP = do
  sps <- lookAhead (wsp <* oneOf "-*+")
  sps2 <- try (wsp *> oneOf "-*+" *> some (char ' '))
  c <- componentP
  _ <- many (try (wsp *> endOfLine))
  -- _ <- trace ("DEBUG: indentation depth" ++ show (length sps + length sps2  +1)) (length sps + length sps2  +1)
  cs <- many (indentedCmptP (length sps + length sps2 + 1))
  return (c : cs)

trace2 :: Show a => [Char] -> a -> a
trace2 name x = trace (name ++ ": " ++ show x) x

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

unorderedItemP :: Int -> Parser Item
unorderedItemP i = do 
  _ <- try (count i (char ' '))
  item <- oneOf "-*+" *> wsp *> many (satisfy (not . isReserved)) <* endOfLine 
  case runParserT (count (i + 2) (char ' ')) of
    Left _ -> return [Plain (Literal item)]
    Right _ -> do 
      uis <- unorderedItemsP (i + 2)
      return [Plain (Literal item), UnorderedList uis]

unorderedItemsP :: Int -> Parser [Item]
unorderedItemsP i = do 
  item <- oneOf "-*+" *> wsp *> many (satisfy (not . isReserved)) <* endOfLine 
  items <- many (unorderedItemP i)
  return $ [Plain (Literal item)] : items

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

tt = "- A\n- B\n  - C\n  - D\n"

-- >>> doParse parseList tt
-- Right (UnorderedList [[Literal "A"],[Literal "B"]])

-- >>> doParse unorderedListP tt
-- Right (UnorderedList [[Literal "A"],[Literal "B",Literal "-"]])

-- >>> doParse listP tt
-- Right (UnorderedList [[Literal "A"],[Literal "B",Literal "-"]])

{-
t = UnorderedList [
    [Paragraph (Block [Literal "hello world!",LineBreak])],
    [
      Paragraph (Block [Literal "unordered",LineBreak]),
      UnorderedList [
        [
          Paragraph (Block [Literal "first item",LineBreak]),
          UnorderedList [[Paragraph (Block [Literal "second item\n"])]]
          ]
        ]
      ]
    ]

tCorrect = UnorderedList [
    [Paragraph (Block [Literal "hello world!",LineBreak])],
    [
      Paragraph (Block [Literal "unordered",LineBreak]),
      UnorderedList [
        [Paragraph (Block [Literal "first item",LineBreak])],
        [Paragraph (Block [Literal "second item\n"])]
      ]
    ]
  ]
  -}

listP :: Parser Component
listP = unorderedListP <|> orderedListP

unorderedListP :: Parser Component
unorderedListP = do
  items <- some (try uiP) -- [Block]
  return (UnorderedList items)

orderedListP :: Parser Component
orderedListP = do
  start <- lookAhead (many digit <* char '.')
  items <- some (try oiP)
  return (OrderedList items)

componentP :: Parser Component
componentP = (
  try hrP 
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

strP :: Int -> Parser String 
strP i = do 
  lookAhead eof
  _ <- try (count i wsp) 
  s <- try (oneOf "-*+" *> wsp *> many (satisfy (not . isReserved)) <* endOfLine)
  ss <- strP i 
  return (s ++ " & " ++ ss)

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

