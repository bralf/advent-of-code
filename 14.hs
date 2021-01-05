import Data.List
import Data.Char
import Control.Monad.State.Lazy

main = do
  f <- readFile "14-input.txt"
  putStrLn "Answer 1:"
  print $ sum $ map snd $ evalState (execute (lines f)) ([],[])
  putStrLn "Answer 2:"
  let st = evalState (execute2 (lines f)) ([],[])
  --print (length st)
  --print (clean st)
  print $ sum $ map snd st
  

type Bit = Char
type Bin = [Bit]
type Mask = Bin
type MemLoc = Int
type MemVal = Int

type MemState = [(MemLoc,MemVal)]
type ProgState = (Mask,MemState)

execute :: [String] -> State ProgState MemState
execute [] = do
  (_,st) <- get
  return st
execute (l:ls) = do
  (m,st) <- get
  if (l!!1=='a') then put ((readMask l),st) >> execute ls else let (ml,mv) = readMem l in put (m,update st (ml,(binToInt (mask m (intToBin mv))))) >> execute ls

execute2 :: [String] -> State ProgState MemState
execute2 [] = do
  (_,st) <- get
  return st
execute2 (l:ls) = do
  (m,st) <- get
  if (l!!1=='a') then put ((readMask l),st) >> execute2 ls else let (ml,mv) = readMem l in put (m,foldl update st [(binToInt ml',mv) | ml'<- bToBs (mask2 m (intToBin ml))]) >> execute2 ls   

update :: MemState -> (MemLoc,MemVal) -> MemState
update [] (l,v)= [(l,v)]
update ((l',v'):st) (l,v) | l<l'      = (l, v):(l',v'):st
                          | l==l'     = (l',v):st
                          | otherwise = (l',v') : update st (l,v)

--Problem 1

maskBit :: Bit -> Bit -> Bit
maskBit mi bi | mi == 'X' = bi
              | otherwise = mi

mask :: Mask -> Bin -> Bin
mask m bs = reverse [ maskBit mi bi | (mi,bi) <- zip (reverse m) (reverse bs ++ repeat '0')]

--Problem 2

maskBit2 :: Bit -> Bit -> Bit
maskBit2 mi bi | mi == '0' = bi
               | otherwise = mi

mask2 :: Mask -> Bin -> Bin
mask2 m bs = reverse [maskBit2 mi bi | (mi,bi) <- zip (reverse m) (reverse bs ++ repeat '0')]

bToBs :: Bin -> [Bin]
bToBs [] = [[]]
bToBs (mi:mis) | mi == 'X' = (map ('0':) (bToBs mis)) ++ (map ('1':) (bToBs mis))
              | otherwise = map (mi:) (bToBs mis)

---Read

intToBin :: Int -> Bin
intToBin n | n == 0    = [] 
           | even n    = intToBin (n `div` 2) ++ ['0']
           | otherwise = intToBin ((n-1) `div` 2) ++ ['1']


---   foldr (\x y -> (read [x]) + (2*y)) 0 1010
--   = (\x y -> (read [x]) + (2*y)) '0' 0 = 1

binToInt :: Bin -> Int
binToInt bs = sum [2^a*b | (a,b) <- zip [0..] (map (read.return) (reverse (dropWhile (=='0') bs)))]

readMask :: String -> Mask
readMask = last . words

readMem :: String -> (Int,Int)
readMem st = (read (takeWhile isDigit (drop 4 a)),read b)
  where (a:_:b:_) = words st


