import Data.List

-- mainF :: [[Integer]] -> IO()
-- mainF xss = do
--     line <- getLine
--     let ns = words line
--     if length ns < 2 then print (getSafes xss) else mainF ((map (\x -> read x :: Integer) ns):xss)

-- getSafes :: [[Integer]] -> Integer
-- getSafes xss = sum (map (\x -> if x then 1 else 0) (map isSafe xss))

-- isSafe :: [Integer] -> Bool
-- isSafe xs = ((maximum.distsBetween) xs <= 3) && ((minimum.distsBetween) xs >= 1) && (strictMonotone (>) xs || strictMonotone (<) xs)

-- strictMonotone :: (Integer -> Integer -> Bool) -> [Integer] -> Bool
-- strictMonotone f xs = all id (zipWith f (drop 1 xs) xs)

-- distsBetween :: [Integer] -> [Integer]
-- distsBetween xs = zipWith (\x y -> abs (x-y)) (drop 1 xs) xs

mainF :: [[Integer]] -> IO()
mainF xss = do
    line <- getLine
    let ns = words line
    if length ns < 2 then print (getSafes xss) else mainF ((map (\x -> read x :: Integer) ns):xss)

getSafes :: [[Integer]] -> Integer
getSafes xss = sum (map (\x -> if x then 1 else 0) (map (\xs -> any isSafe (perm xs)) xss))

perm :: [a] -> [[a]]
perm xs = [take (n-1) xs ++ drop n xs | n <- [1..(length xs)]]

isSafe :: [Integer] -> Bool
isSafe xs = ((maximum.distsBetween) xs <= 3) && ((minimum.distsBetween) xs >= 1) && (strictMonotone (>) xs || strictMonotone (<) xs)

strictMonotone :: (Integer -> Integer -> Bool) -> [Integer] -> Bool
strictMonotone f xs = all id (zipWith f (drop 1 xs) xs)

distsBetween :: [Integer] -> [Integer]
distsBetween xs = zipWith (\x y -> abs (x-y)) (drop 1 xs) xs