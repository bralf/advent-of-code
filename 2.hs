import Data.Char
type Quad = (Int,Int,Char,String)

answer1 = do
  f <- readFile "2-input.txt" 
  let quads = map (process. words) $ lines f :: [Quad]
  return $ length $ filter valid1 quads

answer2 = do
  f <- readFile "2-input.txt" 
  let quads = map (process. words) $ lines f :: [Quad]
  return $ length $ filter valid2 quads
   
process :: [String] -> Quad 
process [s1,s2,s3] = (a,b,head s2,s3)
  where (a,b) = (read $ takeWhile isDigit s1,read $ tail $ dropWhile isDigit s1)

valid1 :: Quad -> Bool
valid1 (a,b,c,s) = let l = length $ filter (==c) s in a <= l && l <= b

valid2 :: Quad -> Bool
valid2 (a,b,c,s) | (s !! (a-1) == c) = not (s !! (b-1) == c)
                 | otherwise = (s !! (b-1) == c)
