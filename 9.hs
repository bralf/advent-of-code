import Data.List

answer = do
  f <- readFile "9-input.txt"
  let ns = map read (lines f)
  putStrLn "Answer 1:"
  print $ ns !! ((head (dropWhile (valid ns) [1..]))+25)
  putStrLn "Answer 2:"
  print $ (minimum (sumToAns ns))+maximum (sumToAns ns)
    

valid :: [Int] -> Int ->  Bool
valid ns i = (ns !! (i+25)) `elem` [(a+b) | let ns' = take 25 (drop i ns),a<-ns',b<-ns']

sumToAns :: [Int] -> [Int]
sumToAns ns = head [ns' | let l = length ns,a<-[0..(l-1)],b<-[0..(l-1)],let ns' = take b (drop a ns),sum ns' == 57195069]



