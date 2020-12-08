import Data.List
import Data.Char
import Control.Monad.State.Lazy

type Acc = Int
type Log = [Line]
type CodeState = (Acc,Log,Line,Int)
type Line = Int
data Cmd = Nop | Acc | Jmp
  deriving (Show,Eq)
type Inst = (Cmd,Int)

answer = do
  f <- readFile "8-input.txt"
  let insts = map readLine (lines f)
  putStrLn "Answer 1:"
  print $ evalState (execute insts) (0,[],1,0)  
  putStrLn "Answer 2:"
  return $ evalState (execute insts) (0,[],1,1) 

execute :: [Inst] -> State CodeState Line
execute insts = do
  (acc,log,l,n) <- get
  let insts' = changeInsts n insts
  case () of _
                | (l>length insts') || (l `elem` log && (n==0)) -> return acc
                | l `elem` log -> put (0,[],1,n+1) >> execute insts
                | otherwise -> put (doInst (acc,log,l,n) (insts' !! (l-1))) >> execute insts
                 


doInst :: CodeState -> Inst -> CodeState
doInst (acc,log,l,n) (Nop,_) = (acc,l:log,l+1,n)
doInst (acc,log,l,n) (Acc,m) = (acc+m,l:log,l+1,n)
doInst (acc,log,l,n) (Jmp,m) = (acc,l:log,l+m,n)

changeInsts :: Int -> [Inst] -> [Inst]
changeInsts 0 insts = insts
changeInsts i insts | cmd == Nop = as ++ ((Jmp,m):bs)
                    | cmd == Jmp = as ++ ((Nop,m):bs)
                    | cmd == Acc = insts
  where (as,((cmd, m):bs)) = splitAt (i-1) insts

---Parsing file

readInst :: String -> Cmd
readInst "nop" = Nop
readInst "acc" = Acc
readInst "jmp" = Jmp

readLine :: String -> Inst
readLine st = (readInst (take 3 st),s*(read (drop 5 st)))
  where s = if (st !! 4) == '+' then 1 else -1
