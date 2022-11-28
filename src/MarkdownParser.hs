module MarkdownParser where 

import Control.Applicative
import qualified Data.Char as Char

import MarkdownSyntax

{-

TODO: 
    1) Data types for HTML (DONE)
    2) HTML Converter (implemenation left undefined) + model-based property tests
    3) MarkdownParser (implementation left undefined) + tests
    4) HUnit tests (self-construct a Markdown syntax and te)
        a) doParse filePath ~= Block [Component [Statement]]
-}