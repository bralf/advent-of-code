import Data.List
import Data.Char
import qualified Data.Set as S

answer = do
  f <- readFile "6-input.txt"
  let grps = (map (filter (/= "")) . groupBy (\x y -> y /= "") . lines) f
  print "Answer 1:"
  print $ sum $ map (S.size.S.unions .(map S.fromList)) grps
  print "Answer 2:"
  print $ sum $ map (length. foldr1 intersect) grps







