intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

revIntList :: [Integer] -> [Integer]
revIntList [] = []
revIntList (x) = revIntList(tail(x)) ++ [head(x)]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = x  : y * 2 : doubleEveryOther(zs)

getDigits :: Integer -> [Integer]
getDigits 0 = []
getDigits x = getDigits(quot x 10) ++ [x `mod` 10]

mod10 :: Integer -> Integer
mod10 x = x `mod` 10

decomposeIntsGreaterThan10 :: [Integer] -> [Integer]
decomposeIntsGreaterThan10 [] = []
decomposeIntsGreaterThan10 (x:xs) = getDigits(x) ++ decomposeIntsGreaterThan10(xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits(xs)

validate :: Integer -> Bool
validate x = (mod10 . sumDigits . decomposeIntsGreaterThan10 . doubleEveryOther . revIntList . getDigits) x == 0
