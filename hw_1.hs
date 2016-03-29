lastDigit x = x `mod` 10

dropLastDigit x = x `div` 10

toRevDigits 0 = []
toRevDigits x = lastDigit x : toRevDigits (dropLastDigit x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = helper x True
  where helper [] _ = []
        helper (x:xs) True = x : helper xs False
        helper (x:xs) False = 2 * x : helper xs True

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits all = sum (map helper all)
  where helper n
          | n <= 9 = n
          | n > 9 = (n `mod` 10) + (n `div` 10)

luhn :: Integer -> Bool
luhn x
  | y `mod` 10 == 0 = True
  | otherwise = False
    where y = sumDigits $ doubleEveryOther $ toRevDigits x
