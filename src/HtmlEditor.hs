module HtmlEditor where 

import Data.Foldable qualified as Foldable
import Data.List qualified as List

import MarkdownSyntax

type CompStr = String 

data HTML
  = E -- empty html file
  | Html [CompStr] -- <h1>This is CIS 552</h1>
  deriving (Eq, Show)

empty :: HTML 
empty = E 

-- insert (Heading H1 (Block [Literal "CIS 552"])) E -> Heading H1 (Block [Literal "CIS 552"])
insert :: CompStr -> HTML -> HTML
insert = undefined

convert :: Component -> CompStr 
convert = undefined

-- | Delete all matched component found 
delete :: CompStr -> HTML -> HTML
delete = undefined

member :: CompStr -> HTML -> Bool 
member = undefined

elements :: HTML -> [CompStr]
elements (Html cs) = cs
elements _ = []

-- Post-Condiiton Property
prop_FindPostPresent :: CompStr -> HTML -> Bool 
prop_FindPostPresent k h = member k (insert k h)

prop_FindPostAbsent :: CompStr -> HTML -> Bool 
prop_FindPostAbsent k h = not (member k (delete k h))

-- Metamorphic Property 
prop_InsertEmpty :: CompStr -> Bool 
prop_InsertEmpty k = elements (insert k empty) == [k]

prop_InsertInsert :: CompStr -> CompStr -> HTML -> Bool 
prop_InsertInsert x y h = insert x (insert y h) == insert y (insert x h)

prop_InsertDelete :: CompStr -> CompStr -> HTML -> Bool 
prop_InsertDelete k k0 h = 
  insert k (delete k0 h) == 
    if k == k0 then insert k h else delete k0 (insert k h)

prop_MemberInsert :: CompStr -> CompStr -> HTML -> Bool 
prop_MemberInsert k k0 h = 
  member k0 (insert k h) == (k == k0 || member k0 h)

prop_DeleteEmpty :: CompStr -> Bool 
prop_DeleteEmpty k = delete k empty == empty 

prop_DeleteInsert :: CompStr -> CompStr -> HTML -> Bool 
prop_DeleteInsert k k0 h = 
  delete k (insert k0 h) == if k == k0 then 
    if member k0 h then delete k h else h
    else insert k0 (delete k h)

prop_DeleteDelete :: CompStr -> CompStr -> HTML -> Bool 
prop_DeleteDelete x y h = 
  delete x (delete y h) == delete y (delete x h)

-- Model-based Property 
prop_InsertModel :: CompStr -> HTML -> Bool 
prop_InsertModel k h = 
  elements (insert k h) == List.insert k (List.delete k $ elements h)

prop_DeleteModel :: CompStr -> HTML -> Bool 
prop_DeleteModel k h = 
  elements (delete k h) == List.delete k (elements h)
