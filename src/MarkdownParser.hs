{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
{-# LANGUAGE BlockArguments #-}

module MarkdownParser where 

import Control.Applicative
    ( Applicative(liftA2), Alternative(some) )
import Control.Monad (guard, when, join)
import Data.Char (isNumber, isSpace, isAlpha, isAlphaNum)
import Data.List.Split ( splitOn )
import Data.List ( elemIndex, isPrefixOf )

import qualified Data.Set as Set (fromList, member)

import qualified System.IO as IO
import qualified System.IO.Error as IO

import Text.Parsec.Char
    ( anyChar, oneOf, satisfy, endOfLine, string, char, digit )
import Text.ParserCombinators.Parsec
    ( optional,
      parse,
      anyChar,
      count,
      oneOf,
      choice,
      lookAhead,
      satisfy,
      notFollowedBy,
      (<|>),
      string,
      try,
      manyTill,
      char,
      many,
      eof,
      Parser,
      ParseError,
      digit )
import Text.Parsec.Pos ( initialPos )
import MarkdownSyntax
    ( Item,
      getHeader,
      Header,
      Statement(..),
      Block(..),
      Component(HorizontalRule, Heading, Paragraph, Blockquote,
                UnorderedList, OrderedList, Table, DefinitionList, Plain,
                CodeBlock),
      Markdown(..),
      DefItem(..),
      Row )
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

lstrip :: String -> String
lstrip [] = []
lstrip (x : xs) = if x == ' ' then lstrip xs else x : xs

rstrip :: String -> String 
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = rstrip . lstrip

wsp :: Parser String
wsp = many (char ' ')

reserved :: String
reserved = "*_`)[] <!\n\\~\\^\\="

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
  Emoji . toUnicode . strip <$> some (
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
titleP = do 
  s <- char '(' *> wsp *> manyTill anyChar (try (wsp *> char ')'))
  if '\"' `notElem` s  
    then return (s, Nothing)
    else let 
      l = filter (not . null) (splitOn "\"" s) in 
        case l of 
          [href, title] -> return (strip href, Just title)
          _ -> return (s, Nothing)

-- >>> doParse linkP "[top of this doc](#sonnets_heading) directly\n"
-- Right (Link (Block [Literal "top",Literal " ",Literal "of",Literal " ",Literal "this",Literal " ",Literal "doc"]) "#sonnets_heading" Nothing)

linkP :: Parser Statement 
linkP = do 
  ss <- char '[' *> manyTill statementP (char ']')
  (href, title) <- titleP 
  return (Link (Block ss) href title)

imageP :: Parser Statement 
imageP = do 
  alt <- string "![" *> manyTill anyChar (char ']') 
  (href, title) <- titleP 
  return (Image alt href title)

lineBreakP ::  Parser Statement
lineBreakP  = do
  try $ string "\\" <|> (count 2 (char ' ') <* many (char ' '))
  try $ lookAhead $ char '\n' <* many (char ' ') <* satisfy (/= '\n')
  return LineBreak

literalP :: Parser Statement
literalP = Literal . strip <$> some (
  notFollowedBy (endOfLine *> wsp *> endOfLine) *> satisfy (not . isReserved)
  )

headingP :: Parser Component
headingP = do
  sps <- wsp
  l <- some (char '#') 
  guard (length l < 7 && not (null l))
  s <- wsp *> manyTill anyChar (try endOfLine)
  let (t, id) = getTextId s in 
    Heading <$> headingLevelP l <*> headingTextP t <*> return id

getTextId :: String -> (String, Maybe String)
getTextId s = case (elemIndex '{' s, elemIndex '#' s, elemIndex '}' s) of 
  (Just i, Just k, Just j) -> (take i s, Just $ take (j - k - 1) (drop (k + 1) s))
  (_, _, _) -> (s, Nothing)

headingLevelP :: String -> Parser Header 
headingLevelP l = return (getHeader (length l))

headingTextP :: String -> Parser Block 
headingTextP s = case doParse blockP (strip s) of 
  Left _ -> return $ Block [] 
  Right b -> return b

-- >>> doParse headingP "## H1 Heading    \n"
-- Right (Heading h2 (Block [Literal "H1",Literal " ",Literal "Heading"]) Nothing)

-- >>> elemIndex '{' "# H1 Heading {#id-3} \n"
-- Just 13

-- >>> elemIndex '}' "# H1 Heading {#id-3} \n"
-- Just 19

-- >>> take 13 "# H1 Heading {#id-3} \n"
-- "# H1 Heading "

-- >>> drop 13 "# H1 Heading {#id-3} \n"
-- "{#id-3} \n"

-- >>> take 5 "#id-3} \n"
-- "#id-3"

-- >>> doParse headingP "# H1 Heading {#id-3} \n"
-- Right (Plain (Block [Literal "H1 Heading {#id-3} "]))

-- >>> doParse headingP "# **H1 Heading** {#id-3}"
-- Right (Heading h1 (Block [Bold (Block [Literal "H1",Literal " ",Literal "Heading"]),Literal " ",Literal "{#id-3}"]) Nothing)

-- >>> splitOn "{" "# **H1 Heading** {#id-3}"
-- ["# **H1 Heading** {#id-3}"]

hidP :: Int -> Block -> Component 
hidP hl (Block []) = Heading (getHeader hl) (Block []) Nothing
hidP hl (Block ss) = undefined

withId :: Statement -> Bool 
withId (Literal s) = '{' `elem` s && '}' `elem` s
withId _ = False

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


codeBlockP :: Parser Component 
codeBlockP = codeBlockP' "```\n" "```" <|> codeBlockP' "~~~\n" "~~~"

codeBlockP' :: String -> String -> Parser Component
codeBlockP' start end = do
  s <- try (string start *> manyTill anyChar (string end))
  return $ CodeBlock (Block (map Literal (splitOn "\n" s) ))

hrP :: Parser Component 
hrP = hrP' '*' <|> hrP' '-' <|> hrP' '_'

hrP' :: Char -> Parser Component
hrP' c = do 
  _ <- wsp *> count 3 (char c) -- has to have at least three occurrences
  r <- many (char c) <* wsp *> manyTill anyChar (try (wsp *> char '\n'))
  if null r then return HorizontalRule else fail "Invalid Horizontal Rule"

componentP :: Parser Component
componentP = (
  try hrP 
  <|> try codeBlockP
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
