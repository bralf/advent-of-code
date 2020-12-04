answer1 = do
  f <- readFile "input.txt" 
  let ns = map read $ lines f :: [Int]
  return $ head [m*n | m<-ns,n<-ns,m+n==2020]
answer2 = do
  f <- readFile "input.txt" 
  let ns = map read $ lines f :: [Int]
  return $ head [m*n*p | m<-ns,n<-ns,p<-ns,m+n+p==2020]
                                   
