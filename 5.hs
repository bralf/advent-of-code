import Data.List

answer = do
  f <- readFile "5-input.txt"
  print "Answer 1:"
  print $ maximum (map seatNo (lines f))
  print "Answer 2:"
  print $ maximum $ ([1..maximum (map seatNo (lines f))] \\ (map seatNo (lines f)))
  
seatNo :: String -> Int
seatNo (a:b:c:d:e:f:g:h:i:j:_) = 512*(rb a)+256*(rb b)+128*(rb c)+64*(rb d)+32*(rb e)+16*(rb f)+8*(rb g)+4*(cb h)+2*(cb i)+(cb j)

rb :: Char -> Int
rb c | c == 'B' = 1
     | c == 'F' = 0
     | otherwise = error "invalid"

cb :: Char -> Int
cb c | c == 'R' = 1
     | c == 'L' = 0
     | otherwise = error "invalid"



