-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

properfactors :: Int -> [Int]
properfactors x = filter (\y->(x `mod` y == 0)) [2..(x-1)]

sumproperfactors :: Int -> Int
sumproperfactors x = foldl (+) 1 (properfactors x)

amicable :: (Int, Int) -> Bool
amicable (x,y) = sumproperfactors x == y && sumproperfactors y == x


listAmicable = filter (\(x,y) -> amicable(x ,y) == True) [(x,y) | x <- [1..], y <- [1..]]
