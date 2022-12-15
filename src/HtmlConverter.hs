{-# LANGUAGE DeriveFoldable #-}
module HtmlConverter where 


import Data.Semigroup ( Endo(Endo, appEndo) )


import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Test.QuickCheck as QC

import MarkdownSyntax
    ( Item,
      Header(H5, H4, H1, H3),
      Statement(..),
      Block(..),
      Component(..),
      Markdown(..),
      DefItem(..),
      Row )
import qualified GHC.Generics as Maybe

data SimpleHTML a
  = E
  | PCDATA a
  | Element a [(a, a)] [SimpleHTML a] -- Element ElementName Attri [..]
  deriving (Eq, Show, Foldable)

toHtmlStr :: Markdown -> String 
toHtmlStr = html2string . toHtml

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

-- Part 1 : Convert the given Markdown file to an HTML string
toHtml :: Markdown -> SimpleHTML String 
toHtml (Markdown cs) = render (map convCmpt cs)

render :: [SimpleHTML String] -> SimpleHTML String 
render ss = Element "html" [] [Element "body" [] ss]

tableBorder :: Maybe String
tableBorder = Just "border: 1px solid black;"

-- Part 2 : Convert each component of the Markdown to a SimpleHTML
convCmpt :: Component -> SimpleHTML String
convCmpt (Heading h b hid) = Element (show h) (addAttris [("id", hid)]) (convBlock b)
convCmpt (Paragraph b) = Element "p" [] (convBlock b) 
convCmpt (Blockquote bs) = Element "blockquote" [] (convBlocks bs)
convCmpt (OrderedList ol) = Element "ol" [] (map convItem ol)
convCmpt (UnorderedList ul) = Element "ul" [] (map convItem ul)
convCmpt (Table tr) = Element "table" (
  addAttris [("style", tableBorder)]
  ) (convRows True tr)
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
convRows isTitle (r : rs) = Element "tr" (
  addAttris [("style", tableBorder)]
  ) (
  map (if isTitle then convRowTitle else convCol) r
  ) : convRows False rs

convRowTitle :: Component -> SimpleHTML String 
convRowTitle title = Element "th" (
  addAttris [("style", tableBorder)]
  ) [convCmpt title]

convCol :: Component -> SimpleHTML String 
convCol col = Element "td" (
  addAttris [("style", tableBorder)]
  ) [convCmpt col]

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
