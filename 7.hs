import Data.List.Split
import Data.List
import Debug.Trace

-- mainF (xs, yss) = do
--     line <- getLine
--     if length line < 4 then print (sum $ map fst $ doWork (reverse xs) (reverse yss)) else mainF (splitCons line xs yss)

-- splitCons text xs yss = ((read (splitText!!0) :: Integer): xs, (map (\x -> read x :: Integer) . splitOn " ") (splitText!!1) : yss)
--     where splitText = splitOn ": " text

-- doWork xs yss = filter individualWorks (zip xs yss)

-- individualWorks (t, ys) = not (null (filter ((==t).apply ys) (pos (length ys - 1))))

-- apply xs ops = foldl (\acc (op, x) -> acc `op` x) (head xs) (zip ops (tail xs))

-- pos 1 = [[(+)], [(*)]]
-- pos n = map ((+):) (pos (n-1)) ++ map ((*):) (pos (n-1))

mainF (xs, yss) = do
    line <- getLine
    if length line < 4 then print (sum $ map fst $ doWork (reverse xs) (reverse yss)) else mainF (splitCons line xs yss)

splitCons text xs yss = ((read (splitText!!0) :: Integer): xs, (map (\x -> read x :: Integer) . splitOn " ") (splitText!!1) : yss)
    where splitText = splitOn ": " text

doWork xs yss = filter individualWorks (zip xs yss)

individualWorks (t, ys) = not (null (filter ((==t).apply ys) (pos (length ys - 1))))

apply xs ops = foldl (\acc (op, x) -> acc `op` x) (head xs) (zip ops (tail xs))

pos 1 = [[(+)], [(*)], [whackConcat]]
pos n = map ((+):) (pos (n-1)) ++ map ((*):) (pos (n-1)) ++ map (whackConcat:) (pos (n-1))

whackConcat x y = read (show x ++ show y) :: Integer
