module Int where

import Prelude hiding (and, or, not)
import Prop

bit :: String -> Int -> WFF
bit a i = var (a ++ "_" ++ show i)

c :: String -> String -> Int -> WFF
c a b i = var ("c_" ++ a ++ b ++ "_" ++ show i)

s :: String -> String -> Int -> WFF
s a b i = var ("s_" ++ a ++ b ++ "_" ++ show i)


carry :: String -> String -> Int -> WFF
carry a b i =
    let j = i - 1
    in or [and [bit a j, bit b j], and [bit a j, c a b j], and [bit b j, c a b j]]

sumBit :: String -> String -> Int -> WFF
sumBit a b i = xor [bit a i, bit b i, c a b i]

sumsEqualities :: Int -> String -> String -> [WFF]
sumsEqualities n a b = [Coimplies (s a b i) (sumBit a b i) | i <- [0..n-1]]

carryEqualities :: Int -> String -> String -> [WFF]
carryEqualities n a b = [Coimplies (c a b (i+1)) (carry a b (i+1)) | i <- [0..n-1]]


addConstraints :: Int -> String -> String -> WFF
addConstraints n a b = and (sumsEqualities n a b ++ carryEqualities n a b)

diff :: Int -> String -> String -> WFF
diff n a b = or [xor [s a b i, s b a i] | i <- [0..n-1]]

constraints :: Int -> WFF
constraints n = and [
    addConstraints n "x" "y",
    addConstraints n "y" "x",
    diff n "x" "y",
    Not (c "x" "y" 0),
    Not (c "y" "x" 0)
  ]
