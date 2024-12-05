import Data.List.Split
import Data.List
import Debug.Trace
import Data.Maybe

-- mainF :: [[Char]] -> IO ()
-- mainF rs pss s1 = do
--     line <- getLine
--     if s1
--         then
--             if not (length line < 2)  
--                 then mainF (parseRule line:rs) pss s1
--                 else mainF rs pss False
--         else 
--             if not (length line < 2)
--                 then mainF rs (parseUpdate line:pss) s1
--                 else print (doWork (reverse rs) (reverse pss))

-- parseUpdate s = map (\x -> read x :: Integer) (splitOn "," s)

-- parseRule r = (read (take 2 r) :: Integer, read (drop 3 r) :: Integer)

-- doWork rs pss = sum $ map (\ps -> ps!!(length ps `div` 2)) $ filter (\ps -> checkUpdate rs ps) pss

-- checkUpdate rs [] = True
-- checkUpdate rs (p:ps) = if anyIn (getEarlierRules rs p) ps then False else checkUpdate rs ps

-- anyIn xs ys = any (`elem` ys) xs

-- getEarlierRules rs p = map fst $ filter (\(a, b) -> b==p) rs

mainF rs pss s1 = do
    line <- getLine
    if s1
        then
            if not (length line < 2)  
                then mainF (parseRule line:rs) pss s1
                else mainF rs pss False
        else 
            if not (length line < 2)
                then mainF rs (parseUpdate line:pss) s1
                else print (doWork (reverse rs) (reverse pss))

parseUpdate s = map (\x -> read x :: Integer) (splitOn "," s)

parseRule r = (read (take 2 r) :: Integer, read (drop 3 r) :: Integer)

doWork rs pss = sum $ map (\ps -> ps!!(length ps `div` 2)) $ map (getValidPerm rs) $ filter (\ps -> not $ checkUpdate rs ps) pss  -- 

getValidPerm rs ps = if checkUpdate rs ps then ps else getValidPerm rs (swapFirstInvalid rs ps)
-- getValidPerm rs ps = concat $ filter (\qs -> checkUpdate rs qs) $ permutations ps

swapFirstInvalid rs ps = foldr (\x acc -> if anyIn (getEarlierRules rs x) acc && (not $ null acc) then swapHeadAndIndex (x:acc) (findInvalidIndex (getEarlierRules rs x) acc) else x:acc) [] ps

swapHeadAndIndex (x:xs) i = xs!!i : (take i xs ++ [x] ++ drop (i+1) xs)

-- find the index of where some element of ys first appears in xs
findInvalidIndex ys xs = fromJust $ (filter isJust $ map (`elemIndex` xs) ys)!!0  -- guaranteed to finish cos of use case

checkUpdate rs [] = True
checkUpdate rs (p:ps) = if anyIn (getEarlierRules rs p) ps then False else checkUpdate rs ps

anyIn xs ys = any (`elem` ys) xs

getEarlierRules rs p = map fst $ filter (\(a, b) -> b==p) rs
