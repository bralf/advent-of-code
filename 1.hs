answer = do
  f <- readFile "1-input.txt" 
  let ns = map read $ lines f :: [Int]
  putStrLn "Answer 1: "
  print $ head [m*n | m<-ns,n<-ns,m+n==2020]
  putStrLn "Answer 2: "
  print $ head [m*n*p | m<-ns,n<-ns,p<-ns,m+n+p==2020]

