import Text.Regex.TDFA

-- mainF :: IO()
-- mainF = do
--     line <- getLine
--     print (sumUp line)

-- sumUp :: String -> Integer
-- sumUp cs = sum(map getProd (getAllTextMatches (cs =~ "mul\\(([0-9]+),([0-9]+)\\)") :: [String]))

-- getProd :: String -> Integer
-- getProd cs = product (map read (getAllTextMatches (cs =~ "[0-9]+") :: [String]))

mainF :: IO()
mainF = do
    line <- getLine
    print (sumUp line)

sumUp :: String -> Integer
sumUp cs = sum(map getProd (filterDont True (getAllTextMatches (cs =~ "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)") :: [String])))

filterDont :: Bool -> [String] -> [String]
filterDont _ [] = []
filterDont b (cs:css)
    | cs == "do()"    = filterDont True css
    | cs == "don't()" = filterDont False css
    | not b           = filterDont b css
    | otherwise       = cs:filterDont b css

getProd :: String -> Integer
getProd cs = product (map read (getAllTextMatches (cs =~ "[0-9]+") :: [String]))
