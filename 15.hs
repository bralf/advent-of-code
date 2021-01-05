import Data.List

type Mem = [(Int,Int)]

answer = head ((iterate turn start) !! (30000000-6))

start :: [Int]
start = [1,9,0,14,20,19]

test :: [Int]
test = [6,3,0]

start2 = [(0,3),(1,1),(9,2),(14,4),(19,6),(20,5)]

test2 = [(0,3),(3,2),(6,1)]

next :: [Int] -> Int
next [] = 0
next (n:ns) = f (elemIndex n ns)
  where f Nothing  = 0
        f (Just i) = i+1

next2 :: Mem -> Mem
next2 [] = []
next2 (

turn :: [Int] -> [Int]
turn ns = next ns : ns




