{- Lab 1
   Authors:
   Lab group:
 -}
---------------------------------------------
import MeasureTime




power :: Int -> Int -> Int
power n k | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)



-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k 
   | k < 0 = error "stepsPower: negative argument"
stepsPower n k = k + 1

-- B -------------------------
-- power1
power1 :: Int -> Int -> Int
power1 n k  | k < 0 = error "power1: negative argument"
power1 n 0 = 1
power1 n k = product(replicate k n)



-- C -------------------------
-- power2
power2 :: Int -> Int -> Int
power2 n k  | k < 0 = error "power2: negative argument" 
power2 n 0 = 1
power2 n k  | even k = power2 (n * n) (div k 2)
            | odd k = n * power2 n (k-1)



-- D -------------------------
-- test cases
listaN = [(-1000)..100000]
listaK = [0..100000]
{- 
listaN contains all the test cases for bas n, we choose a list from -1000 to 100000, a large variety of number that would cover all of the needed tests
listaK is basically the same with the exeption of negative numbers because k has to be a positive integer, else function power would return ERROR.

 -}

-- comparePower1
comparePower1 :: Int -> Int -> Bool
comparePower1 n k  = power n k == power1 n k && power n k == power2 n k


-- Test function
--listaCompare :: (Num comparePower1) => comparePower1 -> Bool -> [Bool] -> Bool
--listaCompare = and [comparePower1 n k [(n,k) | n <- [0..10], k <-  [0..10] ]]

listCompare :: Bool
listCompare = and [ comparePower1 (listaN !! i) (listaK !! j) | i <- [0..10], j <- [0..10] ]
