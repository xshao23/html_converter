# project-cis5520

| Name | Penn Key |
| ----- | ------- |
| Xuchong Shao | xuchong |
| Yiwen Zhu | yiwzhu |
​
Our project is to take a Markdown input file, convert it and output to an HTML file. We support all the basic syntaxes that are commonly seen in Markdown files, as well as all of the extended syntaxes apart from footnote and automatic URL linking.
​
## Module organization

Haskell packages typically divide their source code into three separate places:

  - The source code consists of four main modules, `MarkdownSyntax.hs`, `MarkdownParser.hs`, `HtmlConverter.hs` and `IOHandler.hs`. 
    - `MarkdownSyntax.hs`: Contains the internal Markdown data structures used by the HtmlConverter and produced by the MarkdownParser. 
    - `MarkdownParser.hs`: Implements the parsers that break down an entire Markdown file to smaller pieces (based on the structure defined in `MarkdownSyntax`) and translates them to the internal Markdown structure.
    - `HtmlConverter.hs`: Takes the result of Markdown structure (as produced by `MarkdownParser`) and translates them to the internal HTML structure (*SimpleHTML*).
    - `IOHandler.hs`: Accepts user-defined input, consisiting of source file and the output path, and produces the final output (an HTML file).
  
  - The entry point of the project your executable is in [Main.hs](app/Main.hs). The main function simples calls whatever is passed to `Lib.hs`, which serves as a bridge connecting to the source files.
  
  - All of the test cases is in [the test directory](test/Spec.hs) that includes tests for the parser and the converter. We test our application against unit tests and property tests. 
    - `TestConverter.hs`: Contains all unit tests and several property tests for the HTML structure.
    - `TestParser.hs`: Contains all unit tests for parsing different parts of the markdown file.

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`. Specifically, you should pass two arguments [source_file] [output_path], e.g., 

```
  stack run markdownInput.md htmlOutput.html
```

You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries
We used Parsec library to implement `MarkdownParser`. However, we already added it to our `.cabal` and there is nothing the user needs to do to run the program.
 
<!-- 
This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-19.19 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library. -->

