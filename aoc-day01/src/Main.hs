module Main where

parse :: String -> Int
parse ('-':x) = negate (read x)
parse ('+':x) = read x

-- part1 :: [String] -> Int
-- part1 [] = 0
-- part1 (x:xs) = parse x + part1 xs

firstDup :: Eq a => [a] -> Maybe a
firstDup = go []
  where go seen [] = Nothing
        go seen (x:xs) | x `elem` seen = Just x
                       | otherwise = go (x:seen) xs

part1 :: [String] -> Int
part1 = sum . map parse

part2 :: [String] -> Maybe Int
part2 = firstDup . scanl (+) 0 . cycle . map parse

main :: IO ()
main = do
  file <- getContents
  let contents = lines file
  print (part1 contents)
  print (part2 contents)
