import Data.List
import Data.Char

type Cond = String -> Bool

answer = do
  f <- readFile "4-input.txt"
  let ids = map (concat . (map words)) $ paragraphs (lines f)
  putStrLn "Answer 1: "
  print $ length $ filter (\x -> present x) ids
  putStrLn "Answer 2: "
  print $ length $ filter (\x -> present x && valid x) ids

paragraphs = map (filter (/= "")) . groupBy (\x y -> y /= "")

present :: [String] -> Bool
present sts = (["byr","iyr","eyr","hgt","hcl","ecl","pid"] \\ map (takeWhile (/=':')) sts) == []

valid :: [String] -> Bool
valid sts = and [xvalid sts "byr" (btwn 1920 2002),xvalid sts "iyr" (btwn 2010 2020),xvalid sts "eyr" (btwn 2020 2030),xvalid sts "hgt" hgtCond,xvalid sts "hcl" hclCond,xvalid sts "ecl" (`elem` ["amb","blu","brn","gry","grn","hzl","oth"]),xvalid sts "pid" (\st-> (length st == 9 && all isDigit st))]

xvalid :: [String] -> String -> Cond -> Bool
xvalid sts cat cond = (not . null)[st | st <- sts, let(pr,(s:su)) = splitAt 3 st,pr == cat,cond su]

btwn :: Int -> Int -> Cond
btwn a b = (\x -> (x>=a && x<=b)).read

hgtCond :: Cond
hgtCond st | (b=="cm") = a>=150 && a<=193
           | (b=="in") = a>=59  && a<=76
           | otherwise = False
  where (a',b) = break (not.isDigit) st
        a = read a'

hclCond :: Cond
hclCond (c:cs) = c == '#' && length cs == 6 && all (`elem` ['0'..'9']++['a'..'f']) cs



  
