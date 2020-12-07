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

readContents :: [String] -> (Int,Colour)
readContents (w:ws) = (read w,C (ws !! 0) (ws !! 1))

contents :: String -> [(Int,Colour)]
contents l = map (readContents.words.dropWhile (not.isDigit)) (groupBy (\_ y -> y /= ',') l)

rule :: [String] -> Rule
rule (a:b:_:_:rest) | head (rest) == "no" = (C a b,[])
                    | otherwise = (C a b,contents (unwords rest))
        
--------Problem 1

containers1 :: RGraph -> Colour -> [Colour]
containers1 rs c = [rc | (rc,rncs) <- rs, c `elem` map snd rncs]

containers :: RGraph -> Colour -> [Colour]
containers rs c = containers1 rs c ++ concat (map (containers rs) (containers1 rs c)) 

--------Problem 2

contentsCN :: RGraph -> Colour -> [(Int,Colour)]
contentsCN rs c = [(ni,ci) | let rc = head $ filter (\r -> fst r == c) rs,let rcncs = snd rc, (ni,ci) <- rcncs]

noContains :: RGraph -> Colour -> Int
noContains rs c = sum [ni+ni*nci | (ni,ci) <- contentsCN rs c,let nci = noContains rs ci]













                       







