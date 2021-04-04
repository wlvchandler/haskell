import Data.List
import Data.Bits
import System.IO
import System.CPUTime
import Control.DeepSeq

fib :: Int -> Integer
fib n = snd . foldl_ fib_ (1, 0) . dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib_ (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g
        foldl_ = foldl' -- '

main = do 
  start <- getCPUTime
  let r = fib 1000000000 
  end <- deepseq r getCPUTime
  putStrLn (show (div (end - start) (10^12)))
