import Data.List
-- mainF :: [[Char]] -> IO()
-- mainF css = do
--     line <- getLine
--     let ns = words line
--     if length line < 4 then print (getXMAS css) else mainF (line:css)

-- getXMAS :: [[Char]] -> Integer
-- getXMAS css = (sum (map getXMASRow css) + sum (map getXMASRow (transpose css)) + sum (map getXMASRow (getDiagonals css)) + sum (map getXMASRow (getDiagonals (map reverse css))))

-- getDiagonals :: [[a]] -> [[a]]
-- getDiagonals css = map (\n -> zipWith (!!) (drop n css) [0..(length (css!!0) - 1)]) [0..(length css) - 1] ++ map (\n -> zipWith (!!) (lose n css) [n..(length (css!!0)) - 1]) [1..(length (css!!0)) - 1]

-- lose n xs = reverse (drop n (reverse xs))

-- getXMASRow cs = findXMAS cs 0 + findXMAS (reverse cs) 0

-- findXMAS [] p = 0
-- findXMAS (c:cs) p 
--     | c == 'X'           = findXMAS cs 1
--     | c == 'M' && p == 1 = findXMAS cs 2
--     | c == 'A' && p == 2 = findXMAS cs 3
--     | c == 'S' && p == 3 = 1 + findXMAS cs 0
--     | otherwise          = findXMAS cs 0

mainF :: [[Char]] -> IO()
mainF css = do
    line <- getLine
    let ns = words line
    if length line < 2 then print (inBoth $ getXMAS css) else mainF (line:css)

inBoth (p1, p2) = length $ filter (\x -> x `elem` (concat p1)) (concat p2)

getXMAS css = (map getXMASRow (getDiagonals (addIndices css)), map getXMASRow (getDiagonals (map reverse (addIndices css))))

addIndices css = zipWith (\cs i -> zipWith (\c j -> (c, (i, j))) cs [0..length cs]) css [0..length css]

getDiagonals :: [[a]] -> [[a]]
getDiagonals css = reverse (map (\n -> zipWith (!!) (drop n css) [0..(length (css!!0) - 1)]) [0..(length css) - 1]) ++ map (\n -> zipWith (!!) (lose n css) [n..(length (css!!0)) - 1]) [1..(length (css!!0)) - 1]

lose n xs = reverse (drop n (reverse xs))

getXMASRow cs = findXMAS cs 0 (0, 0) ++ findXMAS (reverse cs) 0 (0, 0)

findXMAS [] _ _ = []
findXMAS ((c, pos):cs) p prevPos
    | c == 'M'           = findXMAS cs 1 pos
    | c == 'A' && p == 1 = findXMAS cs 2 pos
    | c == 'S' && p == 2 = prevPos : findXMAS cs 0 pos
    | otherwise          = findXMAS cs 0 pos