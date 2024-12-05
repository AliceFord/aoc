import Data.List

-- p1 :: ([Integer], [Integer]) -> IO()
-- p1 (xs, ys) = do
--     line <- getLine
--     let ls = words line
--     if length line < 2 then print (getDist xs ys) else p1 ((read (ls!!0) :: Integer):xs, (read (ls!!1) :: Integer):ys)

-- getDist :: [Integer] -> [Integer] -> Integer
-- getDist xs ys = sum (zipWith (\x y -> abs (x - y)) (sort xs) (sort ys))


p1 :: ([Integer], [Integer]) -> IO()
p1 (xs, ys) = do
    line <- getLine
    let ls = words line
    if length line < 2 then print (getDist xs ys) else p1 ((read (ls!!0) :: Integer):xs, (read (ls!!1) :: Integer):ys)

getDist :: [Integer] -> [Integer] -> Integer
getDist xs ys = sum (map (`mulTimes` ys) xs)

mulTimes :: Integer -> [Integer] -> Integer
x `mulTimes` ys = x * foldr (\y -> if x == y then (+1) else id) 0 ys