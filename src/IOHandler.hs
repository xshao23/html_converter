module IOHandler where

import System.Environment ( getArgs, getProgName ) 

import HtmlConverter (convertMarkdownContent, error)
import MarkdownParser (parseMarkdownFile)


-- Read in the markdown file, and output the HTML
-- TODO: Should be able to take command line args as file

markdownToHtml :: String -> String -> IO ()
markdownToHtml src dest = do 
  res <- parseMarkdownFile src 
  case res of 
    (Left _) -> writeFile dest $ show HtmlConverter.error
    (Right content) -> writeFile dest (convertMarkdownContent content)

main = do 
  args <- getArgs 
  case args of 
    [src, dest] -> do
      putStrLn ("converting " ++ src ++ " to " ++ dest ++ "...")
      --writeFile dest "<html><body><strong>Hello,</strong><em> world</em></body></html>" -- for testing
      markdownToHtml src dest 
    _ -> putStrLn "acceptable arguments: [source_file] [dest_file]"