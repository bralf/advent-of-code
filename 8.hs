import Data.List
import Data.Char
import Control.Monad.State.Lazy

type Acc = Int
type Log = [Line]
type CodeState = (Acc,Log,Line)
type Line = Int
data Cmd = Nop | Acc | Jmp
  deriving (Show,Eq)
type Inst = (Cmd,Int)

answer = do
  f <- readFile "8-input.txt"
  let ls = lines f
  let insts = map readLine ls
  putStrLn "Answer 1:"
  print $ evalState (execute1 insts) start
  putStrLn "Answer 2:"
  return $ evalState (execute2 insts 1) start

execute1 :: [Inst] -> State CodeState Line
execute1 insts = do
  (acc,log,l) <- get
  if l `elem` log
    then return acc
    else do put (doInst (acc,log,l) (insts !! (l-1)))
            execute1 insts

execute2 :: [Inst] -> Int -> State CodeState Line
execute2 insts i = do
  let insts' = changeInsts i insts
  (acc,log,l) <- get
  if l `elem` log
    then do
      put start
      execute2 insts (i+1)
  else if (l>length insts')
    then return acc
  else do
     put (doInst (acc,log,l) (insts' !! (l-1)))
     execute2 insts i

doInst :: CodeState -> Inst -> CodeState
doInst (acc,log,l) (Nop,n) = (acc,l:log,l+1)
doInst (acc,log,l) (Acc,n) = (acc+n,l:log,l+1)
doInst (acc,log,l) (Jmp,n) = (acc,l:log,l+n)

changeInsts :: Int -> [Inst] -> [Inst]
changeInsts i insts | cmd == Nop = as ++ ((Jmp,n):bs)
                    | cmd == Jmp = as ++ ((Nop,n):bs)
                    | cmd == Acc = insts
  where (as,((cmd, n):bs)) = splitAt (i-1) insts

start :: CodeState
start = (0,[],1)

---Parsing file

readInst :: String -> Cmd
readInst "nop" = Nop
readInst "acc" = Acc
readInst "jmp" = Jmp

readLine :: String -> Inst
readLine st = (readInst (take 3 st),s*(read (drop 5 st)))
  where s = if (st !! 4) == '+' then 1 else -1
