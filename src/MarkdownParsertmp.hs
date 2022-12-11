{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}

module MarkdownParser where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char
import Data.Char (isNumber)
import Data.Foldable (asum)
import MarkdownSyntax
import System.IO qualified as IO
import System.IO.Error qualified as IO
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck as QC ()
import Prelude hiding (filter)

-- definition of the parser type
newtype Parser a = P {doParse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \s -> do
    (c, cs) <- doParse p s
    return (f c, cs)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \s -> Just (x, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = P $ \s -> do
    (f, s') <- doParse p1 s
    (x, s'') <- doParse p2 s'
    return (f x, s'')

instance Alternative Parser where
  empty :: Parser a
  empty = P $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

-- | Combine two Maybe values together, producing the first
-- successful result
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing y = y

-- | Return the next character from the input
get :: Parser Char
get = P $ \case
  (c : cs) -> Just (c, cs)
  [] -> Nothing

-- | This parser *only* succeeds at the end of the input.
eof :: Parser ()
eof = P $ \case
  [] -> Just ((), [])
  _ : _ -> Nothing

-- | Filter the parsing results by a predicate
filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> do
  (c, cs) <- doParse p s
  guard (f c)
  return (c, cs)

---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------

type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors, but we
-- give it a type similar to other Parsing libraries.
parse :: Parser a -> String -> Either ParseError a
parse parser str = case doParse parser str of
  Nothing -> Left "No parses"
  Just (a, _) -> Right a

-- | parseFromFile p filePath runs a string parser p on the input
-- read from filePath using readFile. Returns either a
-- ParseError (Left) or a value of type a (Right).
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do
  IO.catchIOError
    ( do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ parse parser str
    )
    ( \e ->
        pure $ Left $ "Error:" ++ show e
    )

-- | Return the next character if it satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = filter p get

-- | Parsers for specific sorts of characters
alpha, digit, upper, lower, space, noSpace :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace
noSpace = satisfy (not . isSpace)

-- | Parses and returns the specified character
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parses and returns the specified string.
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = foldr (\c p -> (:) <$> char c <*> p) (pure "")

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Int
int = read <$> ((++) <$> string "-" <*> some digit <|> some digit)

-- | Parses one or more occurrences of @p@ separated by binary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the `Parsers` lecture for explanation of this operator.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` pop = foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> pop <*> p)

-- | @chainl p pop x@ parses zero or more occurrences of @p@, separated by @pop@.
-- If there are no occurrences of @p@, then @x@ is returned.
chainl :: Parser b -> Parser (b -> b -> b) -> b -> Parser b
chainl p pop x = chainl1 p pop <|> pure x

-- | Combine all parsers in the list (sequentially)
choice :: [Parser a] -> Parser a
choice = asum -- equivalent to: foldr (<|>) empty

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is pureed.
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

---------------------------------------------

-- The function below will be called by IOHandler (borrowed from parseLuFile)
-- Note that Markdown file consists of many components (in parallel)
-- See MarkdownSyntax line 12
parseMarkdownFile :: String -> IO (Either ParseError Markdown)
parseMarkdownFile = parseFromFile (const <$> markdownP <*> eof)

-- 下面四个是我看着LuParser加上的，如果后来你发现不需要就删掉吧。
markdownP :: Parser Markdown
markdownP = Markdown <$> many componentP

parseMarkdownCmpt :: String -> Either ParseError Component
parseMarkdownCmpt = parse componentP

parseMarkdownStmt :: String -> Either ParseError Statement
parseMarkdownStmt = parse statementP

-- | Skip whitespace
wsP :: Parser a -> Parser a
-- wsP p = many space *> p
wsP p = p <* many space

-- | Parse only a particular string
stringP :: String -> Parser ()
stringP s = wsP $ string s *> pure ()

-- | Combination string and whitespace parser
constP :: String -> a -> Parser a
constP s newVal = stringP s *> pure newVal

test_constP :: Test
test_constP =
  TestList
    [ parse (constP "&" 'a') "&  " ~?= Right 'a',
      parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- | TODO: Fix componentP
componentP :: Parser Component
componentP = heading2P <|> orderedListP <|> unorderedListP <|> taskListP <|> plainP

-- >>> doParse unorderedListP "* this is item"
-- ProgressCancelledException

itemP :: Parser Item
itemP = many componentP

taskItemP :: Parser TaskItem
taskItemP = TI <$> constP "- [x]" False <*> many componentP <|> TI <$> constP "- [ ]" False <*> many componentP

taskListP :: Parser Component
taskListP = TaskList <$> many taskItemP

headerP :: Parser Header
headerP = constP "######" H6 <|> constP "#####" H5 <|> constP "####" H4 <|> constP "###" H3 <|> constP "##" H2 <|> constP "#" H1

headingP :: Parser Component
headingP = undefined

heading2P :: Parser Component
heading2P = Heading2 <$> headerP <*> blockP

paragraphP :: Parser Component
paragraphP = undefined

blockquoteP :: Parser Component
blockquoteP = Blockquote <$> (char '>' *> many componentP)

orderedListP :: Parser Component
orderedListP = OrderedList <$> (digit *> stringP ". " *> many itemP)

unorderedListP :: Parser Component
unorderedListP = unorderedListPlusP <|> unorderedListStarP <|> unorderedListDashP

unorderedListPlusP :: Parser Component
unorderedListPlusP = UnorderedList <$> (stringP "+ " *> many itemP)

unorderedListStarP :: Parser Component
unorderedListStarP = UnorderedList <$> (stringP "* " *> many itemP)

unorderedListDashP :: Parser Component
unorderedListDashP = UnorderedList <$> (stringP "- " *> many itemP)

-- codeBlockP :: Parser Component
-- codeBlockP = undefined

horizontalRuleP :: Parser Component
horizontalRuleP = constP "***" HorizontalRule <|> constP "---" HorizontalRule

-- | parse for backslash
newlineP :: Parser Component
newlineP = undefined

plainP :: Parser Component
plainP = Plain <$> statementP

blockP :: Parser Block
blockP = Block <$> many statementP

-- Statement Parsers --
statementP :: Parser Statement
statementP = strikethroughP <|> boldP <|> italicP <|> imageP <|> linkP <|> backtickP <|> linebreakP <|> emojiP <|> literalP

boldP :: Parser Statement
boldP = boldStarP <|> boldUnderscoreP

boldStarP :: Parser Statement
boldStarP = Bold <$> (many space *> between (stringP "**") blockP (stringP "**"))

boldUnderscoreP :: Parser Statement
boldUnderscoreP = Bold <$> (many space *> between (stringP "__") blockP (stringP "__"))

italicP :: Parser Statement
italicP = italicStarP <|> italicUnderscoreP

italicStarP :: Parser Statement
italicStarP = Italic <$> (many space *> between (char '*') blockP (char '*'))

italicUnderscoreP :: Parser Statement
italicUnderscoreP = Italic <$> (many space *> between (char '_') blockP (char '_'))

backtickP :: Parser Statement
backtickP = Backtick <$> (many space *> between (char '`') (many $ satisfy (/= '`')) (char '`'))

-- >>> P.doParse backtickP "`code`"

linkP :: Parser Statement
linkP = link3P <|> link2P

link2P :: Parser Statement
link2P =
  Link2
    <$> (many space *> between (char '[') blockP (char ']'))
    <*> between (char '(') (many $ satisfy (/= ')')) (char ')')

link3P :: Parser Statement
link3P =
  Link3
    <$> (many space *> between (char '[') blockP (char ']'))
    <*> imageTupP

imageP :: Parser Statement
imageP = image3P <|> image2P

-- >>> P.doParse literalP "a"
-- Just (Literal "a","")

-- >>> doParse imageP "![Tux, the Linux mascot](/assets/images/tux.png)\"a\""
-- Just (Image2 "Tux, the Linux mascot" "/assets/images/tux.png","\"a\"")

image2P :: Parser Statement
image2P =
  Image2
    <$> (many space *> char '!' *> between (char '[') (many $ satisfy (/= ']')) (char ']'))
    <*> between (char '(') (many $ satisfy (/= ')')) (char ')')

image3P :: Parser Statement
image3P =
  Image3
    <$> (many space *> char '!' *> between (char '[') (many $ satisfy (/= ']')) (char ']'))
    <*> imageTupP

-- <* many space
imageTupP :: Parser (String, String)
imageTupP =
  (,)
    <$> (char '(' *> many (satisfy (/= '"')))
    <*> (between (char '\"') (many $ satisfy (/= '\"')) (char '\"') <* char ')')

-- >>> P.doParse (many space *> char '!' ) "![Tux, the Linux mascot]"
-- Just ('!',"[Tux, the Linux mascot]")

-- >>> doParse imageP' "![Tux](/a/b/c)"
-- Variable not in scope: imageP' :: Parser a

-- >>> P.doParse imageTupP "(/a/b/c \"title\")"
-- Just (("/a/b/c ","title"),"")

linebreakP :: Parser Statement
linebreakP = constP "<br>" LineBreak

-- | TODO: Double check the correctness after other methods finished
literalP :: Parser Statement
literalP = Literal <$> wsP ((:) <$> satisfy (not . isSpaceOrReserved) <*> many (alpha <|> digit <|> char ' ')) -- add '~' to reserved

emojiP :: Parser Statement
emojiP = Emoji <$> between (char ':') (many $ satisfy (/= ':')) (char ':') <* many space

strikethroughP :: Parser Statement
strikethroughP = Strikethrough <$> (between (stringP "~~") blockP (stringP "~~") <* many space)

stiketr :: Statement
stiketr = Strikethrough (Block [Literal "  abc  "])

-- >>> doParse literalP "~abc~"
-- Nothing

-- >>> doParse blockP' "~~abc~~"
-- Just (Block [],"~~abc~~")

strikethroughP' :: Parser Statement
strikethroughP' = Strikethrough <$> wsP (between (stringP "~~") blockP (stringP "~~"))

blockP' :: Parser Block
blockP' = Block <$> many literalP

-- nameP :: Parser Name
-- nameP =
--   P.filter
--     (not . (`elem` reserved))
--     ( (:)
--         <$> (P.char '_' <|> P.alpha)
--         <*> many (P.alpha <|> P.digit <|> P.char '_')
--     )
--     <* many P.space

-- >>> doParse (stringP "~~" *> literalP )  "~~abc~~"
-- Just (Literal "abc~~","")

-- >>> doParse strikethroughP  "~~abc~~"
-- Just (Strikethrough (Block [Literal "abc"]),"")

reservedChar :: [Char]
reservedChar = ['~', '_', '*']

isReserved :: Char -> Bool
isReserved c = c `notElem` reservedChar

isSpaceOrReserved :: Char -> Bool
isSpaceOrReserved c = not (isSpace c || isReserved c)

reserved :: [String]
reserved =
  [ "_",
    "*",
    "~~"
  ]
