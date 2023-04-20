import System.Environment
import Data.Char (isDigit)

-- Functie om run-length decompressie uit te voeren op een string
rldecompress :: String -> String
rldecompress "" = ""
rldecompress (c:cs) = replicate count c ++ rldecompress rest
  where
    count = read (takeWhile isDigit cs) :: Int
    rest = dropWhile isDigit cs

-- Functie om de compressiefactor te berekenen
compressFactor :: String -> String -> Double
compressFactor original compressed = fromIntegral (length compressed) / fromIntegral (length original) * 100

-- Hoofdfunctie
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      -- Lees de inhoud van het input bestand
      input <- readFile inputFile

      -- Voer de run-length decompressie uit
      let decompressed = rldecompress input

      -- Schrijf de gedecomprimeerde gegevens naar het output bestand
      writeFile outputFile decompressed

      -- Bereken en print de decompressiefactor
      let factor = compressFactor decompressed input
      putStrLn $ "Length of compressed file " ++ inputFile ++ ": " ++ show (length input) ++ " characters"
      putStrLn $ "Length of " ++ outputFile ++ ": " ++ show (length decompressed) ++ " characters"
      putStrLn $ "Factor: " ++ show factor ++ "%"
      putStrLn "Done."
    _ -> putStrLn "Usage: rlcompress <input_file> <output_file>"
