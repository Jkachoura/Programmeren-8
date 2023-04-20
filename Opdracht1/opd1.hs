import System.Environment
import Data.List

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      putStrLn $ "Input file: " ++ inputFile
      putStrLn $ "Output file: " ++ outputFile
      inputContents <- readFile inputFile
      let sortedContents = sort inputContents
      writeFile outputFile sortedContents
      putStrLn "File sorted and written successfully!"
    _ -> putStrLn "Usage: sortfile <input_file> <output_file>"
