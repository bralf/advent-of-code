answer = do
  f <- readFile "3-input.txt"
  let tss = map trees $ lines f
  putStrLn "Answer 1:"
  print $ length $ impact tss 3 1
  putStrLn "Answer 2:"
  print $ product $ map length [impact tss 1 1,impact tss 3 1,impact tss 5 1,impact tss 7 1,impact tss 1 2]

trees :: String -> [Int]
trees s = [n `mod` 31 | (c,n) <- zip s [1..],c == '#']

impact :: [[Int]] -> Int -> Int -> [Int]
impact tss r d = [l | (l,ts) <- zip [0..] tss, l `mod` d == 0, ((((r*l) `div` d)+1) `mod` 31) `elem` ts]




  
