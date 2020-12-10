import qualified Data.List as L

answer = do
  f <- readFile "10-input.txt"
  let ns = (0 : (L.sort (map read (lines f))))
  print ns
  putStrLn "Answer 1:"
  print $ (diffns ns 1)*(1+(diffns ns 3))
  putStrLn "Answer 2:"
  print $ routes ns

-- Problem 1  

diffns :: [Int] -> Int -> Int
diffns ns k = length $ filter (jumpSize ns k 1) [1..((length ns)-1)]

jumpSize :: [Int] -> Int -> Int -> Int -> Bool 
jumpSize ns diff jump i = (((ns !! i)-(ns !! (i-jump))) == diff)

--- Problem 2

routes :: [Int] -> Int                  
routes = product.((map (chainRoutes.length)). chains)

chainRoutes :: Int -> Int
chainRoutes 1 = 1
chainRoutes 2 = 1
chainRoutes 3 = 2
chainRoutes 4 = 4
chainRoutes 5 = 7

chains :: [Int] -> [[Int]]
chains = groupBy (\x y -> y==x+1) 


groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p = map (uncurry (:)) . groupByNonEmpty p

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [(a,[a])]
groupByNonEmpty p =
   foldr
      (\x0 yt ->
         let (xr,yr) =
               case yt of
                  (x1,xs):ys ->
                     if p x0 x1
                       then (x1:xs,ys)
                       else ([],yt)
                  [] -> ([],yt)
         in  (x0,xr):yr)
      []


                              

                      
                              
