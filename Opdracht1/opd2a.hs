import System.Environment

-- Functie om run-length compressie uit te voeren op een string
rlcompress :: String -> String
rlcompress "" = ""
rlcompress str@(c:cs) = (c : show count) ++ rlcompress rest
  where
    count = length $ takeWhile (== c) str
    rest = dropWhile (== c) str

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

      -- Voer de run-length compressie uit
      let compressed = rlcompress input

      -- Schrijf de gecomprimeerde gegevens naar het output bestand
      writeFile outputFile compressed

      -- Bereken en print de compressiefactor
      let factor = compressFactor input compressed
      putStrLn $ "Length of " ++ inputFile ++ ": " ++ show (length input) ++ " characters"
      putStrLn $ "Length of compressed file " ++ outputFile ++ ": " ++ show (length compressed) ++ " characters"
      putStrLn $ "Factor: " ++ show factor ++ "%"
      putStrLn "Done."
    _ -> putStrLn "Usage: rlcompress <input_file> <output_file>"
