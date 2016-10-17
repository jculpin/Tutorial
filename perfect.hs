-- A perfect number is a number for which the sum of its proper divisors is exactly equal to the 
-- number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
-- which means that 28 is a perfect number.

properfactors :: Int -> [Int]
properfactors x = filter (\y->(x `mod` y == 0)) [2..(x-1)]

sumproperfactors :: Int -> Int
sumproperfactors x = foldl (+) 1 (properfactors x)

perfect :: Int -> Bool
perfect x = sumproperfactors x == x


listPerfect = filter (\x -> perfect x == True) [2..]
