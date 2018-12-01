module Main where

parse :: String -> Int
parse ('-':x) = negate (read x)
parse ('+':x) = read x

-- part1 :: [String] -> Int
-- part1 [] = 0
-- part1 (x:xs) = parse x + part1 xs

part1 :: [String] -> Int
part1 = sum . map parse

ints :: [Int]
ints = 1:2:3:[]

main :: IO ()
main = do
  file <- getContents
  let contents = lines file
  print (part1 contents)
