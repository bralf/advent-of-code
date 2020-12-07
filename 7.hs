import Data.List
import Data.Char

data Colour = C String String
  deriving (Show,Eq)
type Rule = (Colour,[(Int,Colour)])
type RGraph = [Rule]

answer = do
  f <- readFile "7-input.txt"
  let ls = map words (lines f)
  let rs = map rule ls
  let sg = (C "shiny" "gold")
  putStrLn "Answer 1:"
  print $ length.nub $ containers rs sg
  putStrLn "Answer 2:"
  return $ noContains rs sg

----Parsing the file

readContents :: String -> (Int,Colour)
readContents st = (n,C a b)
  where ws = words st
        n  = read (head ws)
        a  = ws !! 1
        b  = ws !! 2    

contents :: String -> [(Int,Colour)]
contents l = map (readContents.dropWhile (not.isDigit)) (groupBy (\_ y -> y /= ',') l)

rule :: [String] -> Rule
rule l | head (rest) == "no" = (C a b,[])
       | otherwise = (C a b,cntns)
  where (a:b:_) = take 2 l
        rest    = drop 4 l
        cntns   = contents (unwords rest)

--------Problem 1

containers1 :: RGraph -> Colour -> [Colour]
containers1 rs c = [rc | (rc,rncs) <- rs, c `elem` map snd rncs]

containers :: RGraph -> Colour -> [Colour]
containers rs c = containers1 rs c ++ concat (map (containers rs) (containers1 rs c)) 

--------Problem 2

contentsCN :: RGraph -> Colour -> [(Int,Colour)]
contentsCN rs c = [(ni,ci) | let rcncs = snd rc, (ni,ci) <- rcncs]
  where rc = head $ filter (\r -> fst r == c) rs

noContains :: RGraph -> Colour -> Int
noContains rs c = sum [ni+ni*nci | (ni,ci) <- contentsCN rs c,let nci = noContains rs ci]













                       







