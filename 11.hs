import Data.List
import Data.Char

data Cell = Fl | Oc | Un
  deriving (Show,Eq)
type Row = [Cell]
type Grid = [Row]
type GridAlt = [(Int,Int,Cell)]


answer = do
  f <- readFile "11-input.txt"
  let g = map (map readCell) (lines f)
  let xl = length (head g)
  let yl = length g
  putStrLn "Answer 1:"
  print $ noOcc (fpgAdj g)
  putStrLn "Answer 2:"
  print $ noOcc (fpgVis g)

lookUpG :: Grid -> (Int,Int) -> Cell
lookUpG g (x,y) = (g !! (y-1)) !! (x-1)

adjacentxys :: (Int,Int) -> [(Int,Int)]
adjacentxys (x,y) = [(x',y') | x'<-[x-1,x,x+1],y'<-[y-1,y,y+1],x'>0,x'<=96,y'>0,y'<=91] \\ [(x,y)]

adjacent :: Grid -> (Int,Int) -> GridAlt
adjacent g (x,y) = [(x',y',lookUpG g (x',y')) | (x',y') <- adjacentxys (x,y)]

noOccAdj :: Grid -> (Int,Int) -> Int
noOccAdj g (x,y) = length [c' | (x',y',c') <- adjacent g (x,y),c'==Oc]

cellChangeAdj :: Grid -> Int -> Int -> Cell -> Cell
cellChangeAdj g x y c | c == Fl = c
                   | c == Un = if noOccAdj g (x,y) == 0 then Oc else Un
                   | c == Oc = if noOccAdj g (x,y) >= 4 then Un else Oc
                     
stepAdj :: Grid -> Grid
stepAdj g = [r' | (r,y) <- zip g [1..], let r' = [c' | (c,x) <- zip r [1..],let c' = cellChangeAdj g x y c]]


fpgAdj :: Grid-> Grid
fpgAdj g | g == stepAdj g = g
         | otherwise      = fpgAdj (stepAdj g)

uList :: Grid -> (Int,Int) -> [Cell]
uList g (x,y) = [c | i<-[1..(y-1)],let c = lookUpG g (x,y-i),c/=Fl]

urList :: Grid -> (Int,Int) -> [Cell]
urList g (x,y) = [c | let k = min (y-1) (96-x), i<-[1..k],let c = lookUpG g (x+i,y-i),c/=Fl]

ulList :: Grid -> (Int,Int) -> [Cell]
ulList g (x,y) = [c | let k = min (y-1) (x-1), i<-[1..k],let c = lookUpG g (x-i,y-i),c/=Fl]

lList :: Grid -> (Int,Int) -> [Cell]
lList  g (x,y) = [c | i<-[1..(x-1)],let c = lookUpG g (x-i,y),c/=Fl]

rList :: Grid -> (Int,Int) -> [Cell]
rList g (x,y) = [c | i<-[1..(96-x)],let c = lookUpG g (x+i,y),c/=Fl]

drList :: Grid -> (Int,Int) -> [Cell]
drList g (x,y) = [c | let k = min (91-y) (96-x), i<-[1..k],let c = lookUpG g (x+i,y+i),c/=Fl]

dlList :: Grid -> (Int,Int) -> [Cell]
dlList g (x,y) = [c | let k = min (91-y) (x-1), i<-[1..k],let c = lookUpG g (x-i,y+i),c/=Fl]

dList :: Grid -> (Int,Int) -> [Cell]
dList g (x,y) = [c | i<-[1..(91-y)],let c = lookUpG g (x,y+i),c/=Fl]

visibleDir :: [Cell] -> Int
visibleDir [] = 0
visibleDir (c:cs) | c == Oc = 1
                  | c == Un = 0

noOccVis :: Grid -> (Int,Int) -> Int
noOccVis g (x,y) = sum $ map visibleDir (([uList,urList,ulList,lList,rList,drList,dlList,dList]<*> [g]) <*> [(x,y)])

cellChangeVis :: Grid -> Int -> Int -> Cell -> Cell
cellChangeVis g x y c | c == Fl = c
                   | c == Un = if noOccVis g (x,y) == 0 then Oc else Un
                   | c == Oc = if noOccVis g (x,y) >= 5 then Un else Oc
                     
stepVis :: Grid -> Grid
stepVis g = [r' | (r,y) <- zip g [1..], let r' = [c' | (c,x) <- zip r [1..],let c' = cellChangeVis g x y c]]


fpgVis :: Grid-> Grid
fpgVis g | g == stepVis g = g
         | otherwise      = fpgVis (stepVis g)         

noOcc :: Grid -> Int
noOcc g = length [c | r <- g,c<-r,c == Oc]

-------Read

readCell :: Char -> Cell
readCell 'L' = Un
readCell '.' = Fl

