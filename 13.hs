answer = do
  f <- readFile "13-input.txt"
  let (l1:l2:_) = lines f
  putStrLn "Answer 1:"
  print $ (\(m,n) -> m*n) $ divAfter (read l1) (readNums ' ' l2)
  putStrLn "Answer 2:"
  print $ fst $ foldl remComb (1,0) (remList (readNums '0' l2))

--Reading

comToSpaceXtoA :: Char -> Char -> Char
comToSpaceXtoA a c | (c == ',') = ' '
                   | (c == 'x') = a
                   | otherwise  = c
             
readNums :: Char -> String -> [Int]
readNums a = (map read) . words . map (comToSpaceXtoA a)          

--Problem 1

coprime :: [Int] -> Bool
coprime ns = null [(n,m)| n<-ns,m<-ns,n/=m,gcd n m /= 1]

divAfter :: Int -> [Int] -> (Int,Int)
divAfter n bs = head [(ni-n,b) | ni <- [n..],b<-bs,mod ni b == 0]

--Problem 2

remList :: [Int] -> [(Int,Int)]
remList bs = [(b,((-r `mod` b)+b) `mod` b) | (b,r)<-zip bs [0..],b/=0]

remComb :: (Int,Int) -> (Int,Int) -> (Int,Int)
remComb (b1,r1) (b2,r2) = head [(b1*b2,r12) | a <- [0..b2-1],let r12 = r1+a*b1,r12 `mod` b2 == r2]


