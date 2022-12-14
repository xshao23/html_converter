{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
{-# LANGUAGE BlockArguments #-}

module MarkdownParser where 

import Control.Applicative hiding ((<|>), many, optional)
import Data.Char (isNumber, isSpace, isAlpha, isAlphaNum)
import Data.Functor
import Data.List.Split hiding (oneOf)
import Data.List
import Data.String
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
parseMarkdownFile :: String -> IO (Either ParseError Markdown)
parseMarkdownFile = parseFromFile (const <$> markdownP <*> eof)

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

statementP :: Parser Statement
statementP = notFollowedBy (endOfLine *> wsp *> endOfLine) *> choice [
  --try autoLinkP, 
  try boldP, 
  try italicP, 
  try strikethroughP, 
  try highlightP,
  try subP,
  try supP,
  try backtickP, 
  try emojiP,
  try linkP,
  try imageP, 
  try lineBreakP,
  try literalP <|> Literal . (:[]) <$> (try (endOfLine <* wsp) <|> oneOf reserved)
  ]

doParse :: Parser a -> String -> Either ParseError a
doParse p = parse p ""

strip :: String -> String
strip [] = []
strip [' '] = []
strip (x:xs) = x:strip xs

wsp :: Parser String
wsp = many (char ' ')

reserved :: String
reserved = "*_`)[] <!#\n\\~\\^\\="

isReserved :: Char -> Bool
isReserved c = c `elem` reserved

emojiisReserved :: Char -> Bool 
emojiisReserved c = c `elem` (reserved ++ ":")

someTill :: Parser a -> Parser end -> Parser [a]
someTill p end = liftA2 (:) p (manyTill p end)

surround :: Parser start -> Parser end -> Parser Block
surround start end = start *> (Block <$> someTill statementP (try end))

between ::  String -> String -> Parser Block
between start end = surround (string start) (string end)

single :: Char -> Parser Block
single ch = surround 
  (satisfy (==ch)) 
  (try (lookAhead (notFollowedBy $ between [ch, ch] [ch, ch]) *> satisfy(==ch)))

boldP ::   Parser Statement
boldP  = Bold <$> (between "**" "**" <|> try (between "__" "__"))

italicP :: Parser Statement
italicP = Italic <$> (single '*' <|> single '_')

strikethroughP ::Parser Statement 
strikethroughP = Strikethrough <$> between "~~" "~~"

highlightP :: Parser Statement 
highlightP = Highlight <$> between "==" "==" 

subP :: Parser Statement 
subP = Sub <$> single '~'

supP :: Parser Statement 
supP = Sup <$> single '^'

backtickP :: Parser Statement
backtickP = Backtick <$> try (string "`" *> manyTill anyChar (string "`"))

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

lineBreakP ::  Parser Statement
lineBreakP  = do
  try $ string "\\" <|> (count 2 (char ' ') <* many (char ' '))
  try $ lookAhead $ char '\n' <* many (char ' ') <* satisfy (/= '\n')
  return LineBreak

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

headingP :: Parser Component
headingP = do
  sps <- wsp
  hLevel <- some (char '#') 
  guard (length hLevel < 7 && not (null hLevel))
  hText <- some (char ' ') *> blockP
  return (Heading (getHeader (length hLevel)) hText)

paragraphP :: Parser Component
paragraphP = Paragraph <$> paragraphP'

paragraphP' :: Parser Block
paragraphP' = do
  s <- try (
    someTill statementP (
      try (some endOfLine *> endOfLine) <|> lookAhead (endOfLine <* wsp <* lookAhead stopP)
      )
    ) <|> some statementP
  return (Block s)

plainP :: Parser Component 
plainP = Plain <$> plainP'

plainP' :: Parser Block
plainP' = do
  b <- try (someTill statementP (lookAhead stopP)) <|> some statementP
  return (Block b)

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

listP :: Parser Component 
listP = ulP <|> olP

itemP :: Parser Item 
itemP = many componentP

ulP :: Parser Component 
ulP = unorderedListP 0 [] []

unorderedListP :: Int -> Item -> [Item] -> Parser Component
unorderedListP d lastItem itemList = UnorderedList <$> unorderedListP' d lastItem itemList

unorderedListP' :: Int -> Item -> [Item] -> Parser [Item]
unorderedListP' d lastItem itemList = do 
  ws <- wsp <* lookAhead (oneOf "-*+")
  if length ws < d -- base case 
    then return (itemList ++ [lastItem])
    else do
      s <- try (oneOf "-*+" *> some (char ' ') *> manyTill anyChar (string "\n"))
      case doParse itemP s of 
        Left _ -> return [] 
        Right currItem -> if length ws == d 
          then -- same level as the previous line
            unorderedListP' d currItem (if null lastItem then itemList else itemList ++ [lastItem])
          else do -- length ws = d + 2, next level 
            nested <- unorderedListP (length ws) currItem []
            unorderedListP' (length ws) (lastItem ++ [nested]) itemList


olP :: Parser Component 
olP = orderedListP 0 [] [] 

orderedListP :: Int -> Item -> [Item] -> Parser Component
orderedListP d lastItem itemList = OrderedList <$> orderedListP' d lastItem itemList

orderedListP' :: Int -> Item -> [Item] -> Parser [Item]
orderedListP' d lastItem itemList = do 
  ws <- wsp <* lookAhead (digit <|> oneOf "-*+")
  if length ws < d -- base case 
    then return (itemList ++ [lastItem])
    else do
      s <- try (some digit *> char '.' *> some (char ' ') *> manyTill anyChar (string "\n"))
      case doParse itemP s of 
        Left _ -> return [] 
        Right currItem -> if length ws == d 
          then -- same level as the previous line
            orderedListP' d currItem (if null lastItem then itemList else itemList ++ [lastItem])
          else do -- length ws = d + 2, next level 
            nested <- orderedListP (length ws) currItem []
            orderedListP' (length ws) (lastItem ++ [nested]) itemList

tableP :: Parser Component 
tableP = Table <$> rowsP

rowsP :: Parser [Row]
rowsP = do 
  rows <- some rowP
  return $ filter (not . null) rows

rowP :: Parser Row 
rowP = do 
  row <- wsp *> char '|' *> wsp *> manyTill anyChar (char '\n')
  if "-" `isPrefixOf` row then return []
  else let col = init (splitOn "|" row)
    in return (colP col)

colP :: [String] -> Row 
colP ss = map cellP (filter (not . null) ss)

cellP :: String -> Component 
cellP s = do 
  case doParse componentP s of 
    Left _ -> Plain (Block [Literal ""])
    Right c -> c

defListP :: Parser Component 
defListP = do 
  di <- defItemP 
  try (string "\n")
  return $ DefinitionList [di]
  
defItemP :: Parser DefItem 
defItemP = do
  term <- manyTill anyChar (lookAhead (string "\n: "))
  _ <- char '\n' 
  case doParse componentP term of 
    Left _ -> return $ DI (Plain (Block [Literal ""])) []
    Right c -> do 
      cs <- many defP 
      return $ DI c cs

defP :: Parser Component
defP = do 
  s <- try (string ": " *> manyTill anyChar (string "\n"))
  case doParse componentP s of 
    Left _ -> return (Plain (Block [Literal ""]))
    Right c -> return c

codeblockP :: Parser Component
codeblockP = do
      s <- try (string "```\n" *> manyTill anyChar (string "```"))
      return $ CodeBlock (Block (map Literal (splitOn "\n" s) ))

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

componentP :: Parser Component
componentP = (
  try hrP 
  <|> try codeblockP
  <|> try blockquoteP
  <|> try headingP 
  <|> try listP 
  <|> try tableP
  <|> try defListP
  <|> try paragraphP
  <|> try plainP
  ) <* optional (many endOfLine)

stopP :: Parser Component
stopP = try hrP <|> try headingP <|> try listP
