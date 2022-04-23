{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}

module Example3 (fib, notTwice, roundTripRSA) where

import Plugin.CurryPlugin.Prelude hiding (not)

----------- Fib -----------

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-------- Not Twice --------

not' :: Bool -> Bool
not' False = True
not' True = False

notTwice :: Bool -> Bool
notTwice x = not' (not' x)

----------- RSA -----------

roundTripRSA :: String -> String
roundTripRSA text =
  let (n, e, d) = makeKeys 14237890 324579807
   in decrypt n d (encrypt n e text)

encrypt, decrypt :: Integer -> Integer -> String -> String
encrypt n e = unlines . map (show . power e n . code) . collect (size n)
decrypt n d = concat . map (decode . power d n . read) . lines

code :: String -> Integer
code = foldl accum 0
  where
    accum x y = (128 * x) + fromIntegral (intFromEnum y)

decode :: Integer -> String
decode n = reverse (expand n)
  where
    expand 0 = []
    expand x = intToEnum (fromIntegral (x `mod` 128)) : expand (x `div` 128)

collect :: Int -> [a] -> [[a]]
collect 0 _ = []
collect _ [] = []
collect n xs = take n xs : collect n (drop n xs)

size :: Integer -> Int
size n = (length (show n) * 47) `div` 100 -- log_128 10 = 0.4745

------- Constructing keys -------------------------

makeKeys :: Integer -> Integer -> (Integer, Integer, Integer)
makeKeys p' q' = (n, invert phi d, d)
  where
    p = nextPrime p'
    q = nextPrime q'
    n = p * q
    phi = (p - 1) * (q - 1)
    d = nextPrime (p + q + 1)

nextPrime :: Integer -> Integer
nextPrime a = head (filter prime [odd, odd + 2 ..])
  where
    odd
      | even a = a + 1
      | True = a
    prime p = and [power (p - 1) p x == 1 | x <- [3, 5, 7]]

invert :: Integer -> Integer -> Integer
invert n a = if e < 0 then e + n else e
  where
    e = iter n 0 a 1

iter :: Integer -> Integer -> Integer -> Integer -> Integer
iter _ v 0 _ = v
iter g v h w = iter h w (g - fact * h) (v - fact * w)
  where
    fact = g `div` h

------- Fast exponentiation, mod m -----------------

power :: Integer -> Integer -> Integer -> Integer
power 0 _ _ = 1
power n m x
  | even n = sqr (power (n `div` 2) m x) `mod` m
  | True = (x * power (n - 1) m x) `mod` m

sqr :: Integer -> Integer
sqr x = x * x

-- Assoc
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

lines :: String -> [String]
lines "" = []
lines s =
  cons
    ( case break (== '\n') s of
        (l, s') ->
          ( l,
            case s' of
              [] -> []
              _ : s'' -> lines s''
          )
    )
  where
    cons ~(h, t) = h : t

unlines :: [String] -> String
unlines [] = []
unlines (l : ls) = l ++ '\n' : unlines ls

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

even :: (Integral a) => a -> Bool
even n = n `rem` 2 == 0

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ xs@[] = (xs, xs)
break p xs@(x : xs')
  | p x = ([], xs)
  | otherwise = let (ys, zs) = break p xs' in (x : ys, zs)

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs

readCharToInt :: Char -> Integer
readCharToInt '0' = 0
readCharToInt '1' = 1
readCharToInt '2' = 2
readCharToInt '3' = 3
readCharToInt '4' = 4
readCharToInt '5' = 5
readCharToInt '6' = 6
readCharToInt '7' = 7
readCharToInt '8' = 8
readCharToInt '9' = 9
readCharToInt _ = -1

tenPower :: Int -> Int
tenPower 0 = 1
tenPower n = tenPower (n - 1) * 10

read :: [Char] -> Integer
read (x : []) = readCharToInt x
read (x : xs) = toInteger (tenPower $ length xs) * readCharToInt x + read xs

chars :: String
chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"§$%&/()=?+*~'#-_.:,; <>|{[]}"

intToEnum :: Int -> Char
intToEnum = (!!) chars

intFromEnum :: Char -> Int
intFromEnum enum = elemIndex enum chars

elemIndex :: (Eq a) => a -> [a] -> Int
elemIndex _ [] = -1
elemIndex value (a : as)
  | value == a = 0
  | otherwise =
      let index = elemIndex value as
       in if index == -1 then index else index + 1
