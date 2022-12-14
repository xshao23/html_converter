module IOHandler where

import System.Environment ( getArgs, getProgName ) 

import HtmlConverter (toHtmlStr, error)
import MarkdownParser (parseMarkdownFile)

-- Read in the markdown file, and output the HTML
convertMarkdownToHtml :: String -> String -> IO ()
convertMarkdownToHtml src dest = do 
  res <- parseMarkdownFile src 
  case res of 
    (Left _) -> writeFile dest $ show HtmlConverter.error
    (Right content) -> writeFile dest (toHtmlStr content)

-- Accept input and output file path
convert :: IO ()
convert = do 
  args <- getArgs 
  case args of 
    [src, dest] -> do
      putStrLn ("converting " ++ src ++ " to " ++ dest ++ "...")
      --writeFile dest "<html><body><strong>Hello,</strong><em> world</em></body></html>" -- for testing
      convertMarkdownToHtml src dest 
      putStrLn "Done!"
    _ -> putStrLn "acceptable arguments: [source_file] [dest_file]"