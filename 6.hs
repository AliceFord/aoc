import Data.List

mainF :: [[Char]] -> IO()
mainF css = do
    line <- getLine
    if length line < 4 then print (countMovedTo $ doMoveUntilComplete (reverse css)) else mainF (line:css)

countMovedTo = length . concat . map (filter (\c -> c == 'X' || c == '^'))

doMoveUntilComplete :: [[Char]] -> [[Char]]
doMoveUntilComplete css = if complete then css2 else doMoveUntilComplete css2
    where (complete, css2) = doOneMove css

doOneMove css
    | not $ null arrowPos = if (snd (head arrowPos)) == 0 then (True, css) else (False, doMoveFromLocation css (arrowPos!!0))
    | otherwise           = doOneMove (rotl css)
    where arrowPos = getArrowPos css

rotl :: [[Char]] -> [[Char]]
rotl = map (map rotlc) . reverse . transpose

rotlc c 
   | c == '^' = '<'
   | c == '<' = 'v'
   | c == 'v' = '>'
   | c == '>' = '^'
   | otherwise = c

doMoveFromLocation css (x, y) = if (css!!(y-1))!!x == '#' 
    then map (map (\c -> if c == '^' then '>' else c)) css
    else take (y-1) css ++ [take x (css!!(y-1)) ++ ['^'] ++ drop (x+1) (css!!(y-1))] ++ [take x (css!!y) ++ ['X'] ++ drop (x+1) (css!!y)] ++ drop (y+1) css

getArrowPos css = [(x,y) | (cs, y) <- zip css [0..], (c, x) <- zip cs [0..], c == '^']
