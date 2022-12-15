# project-cis5520

Name: Xuchong Shao (xuchong), Yiwen Zhu (yiwzhu)


This is an "Empty project" for Haskell. It is configured in the same way as
the lecture demo and homework assignments for CIS 5520, but contains no
code. Feel free to use this project for experimentation!

If you want to change the name of this project, look for all occurrences of
`project-cis5520` in the `project-cis5520.cabal` file and in the `hie.yaml` 
file. (And change the name of the cabal file to match your new name!)

## Module organization

Haskell packages typically divide their source code into three separate places:

  - The source code consists of three main modules, MarkdownSyntax, MarkdownParser, and HtmlConverter. MarkdownSyntax contains the internal Markdown data structures used by the HtmlConverter and produced by the MarkdownParser. MarkdownParser parses a Markdown file into our internal Markdown data structures. HtmlConverter then converts the internal data structures to a nested HTML structures. Last, IOHandler handles all the input and output utilities. 
    
  
  - The entry point of the project your executable is in [Main.hs](app/Main.hs). The main function converts a Markdown file into a HTML file. 
  
  - All of the test cases is in [the test directory](test/Spec.hs) that includes tests for the parser and the converter. We test our application against unit tests and property tests. 

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries
We use Parsec library to build our parser. 
<!-- 
This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-19.19 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library. -->

