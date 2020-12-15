import Data.List
import Data.Char
import Control.Monad.State.Lazy

type Pos = (Int,Int)
type Dir = Int
type ShipState1 = (Pos,Dir)
type ShipState2 = (Pos,Pos)
type Inst = (Char,Int)

answer = do
  f <- readFile "12-input.txt"
  let insts = map (\(c:st)->(c,read st)) $ lines f
  putStrLn "Answer 1:"
  print $ (md.fst) $ foldl move1 ((0,0),90) insts
  putStrLn "Answer 2:"
  print $ (md.fst) $ foldl move2 ((0,0),(10,1)) insts

md :: Pos -> Int
md (x,y) = (abs x) + (abs y)

move1 :: ShipState1 -> Inst ->  ShipState1
move1 ((x,y),d) ('N',i) = ((x,y+i),d)
move1 ((x,y),d) ('S',i) = ((x,y-i),d)
move1 ((x,y),d) ('E',i) = ((x+i,y),d)
move1 ((x,y),d) ('W',i) = ((x-i,y),d)
move1 ((x,y),d) ('L',i) = ((x,y),(((d-i)+360) `mod` 360))
move1 ((x,y),d) ('R',i) = ((x,y),(d+i) `mod` 360)
move1 ((x,y),d) ('F',i) | d == 0   = ((x,y+i),0)
                        | d == 90  = ((x+i,y),90)
                        | d == 180 = ((x,y-i),180)
                        | d == 270 = ((x-i,y),270)

move2 :: ShipState2 -> Inst ->  ShipState2
move2 ((sx,sy),(wx,wy)) ('N',i) = ((sx,sy),(wx,wy+i))
move2 ((sx,sy),(wx,wy)) ('S',i) = ((sx,sy),(wx,wy-i))
move2 ((sx,sy),(wx,wy)) ('E',i) = ((sx,sy),(wx+i,wy))
move2 ((sx,sy),(wx,wy)) ('W',i) = ((sx,sy),(wx-i,wy))
move2 ((sx,sy),(wx,wy)) ('L',i) | i == 90  = ((sx,sy),(-wy,wx))
                                | i == 180 = ((sx,sy),(-wx,-wy))
                                | i == 270 = ((sx,sy),(wy,-wx))
move2 ((sx,sy),(wx,wy)) ('R',i) = move2 ((sx,sy),(wx,wy)) ('L',360-i)
move2 ((sx,sy),(wx,wy)) ('F',i) = ((sx+i*wx,sy+i*wy),(wx,wy))











