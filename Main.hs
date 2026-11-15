module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Parallel.Strategies (runEval, rpar, rseq)
import GHC.Conc (setNumCapabilities, getNumCapabilities)
import GHC.Clock (getMonotonicTimeNSec)
import Data.List (sort)

-- Перевірка, чи є число степенем двійки
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
    | n <= 0 = False
    | n == 1 = True
    | even n = isPowerOfTwo (n `div` 2)
    | otherwise = False

-- Вибір елементів із парними позиціями
evens :: [Int] -> [Int]
evens (x:_:xs) = x : evens xs
evens [x] = [x]
evens [] = []

-- Вибір елементів із непарними позиціями
odds :: [Int] -> [Int]
odds (_:x:xs) = x : odds xs
odds _ = []

-- Почергове об'єднання двох списків
interleave :: [Int] -> [Int] -> [Int]
interleave (x:xs) (y:ys) =
    x : y : interleave xs ys
interleave [] [] = []
interleave _ _ = error "Invalid merge"

-- Порівняння неперекривних пар
-- Починаючи з другого елемента
compareSwap :: [Int] -> [Int]
compareSwap (x:xs) = x : comparePairs xs
compareSwap [] = []

comparePairs :: [Int] -> [Int]
comparePairs (x:y:xs) =
    min x y : max x y : comparePairs xs
comparePairs xs = xs

-- Odd-Even Merge Бетчера
-- Вхідні списки мають бути відсортованими,
-- однакової довжини, що є степенем двійки
oddEvenMerge :: [Int] -> [Int] -> [Int]
oddEvenMerge [] [] = []

oddEvenMerge [x] [y] =
    if x <= y then [x, y] else [y, x]

oddEvenMerge xs ys =
    let evenPart =
            oddEvenMerge (evens xs) (evens ys)

        oddPart =
            oddEvenMerge (odds xs) (odds ys)

        merged = interleave evenPart oddPart

    in compareSwap merged

-- Паралельне сортування Бетчера
batcherSort :: [Int] -> [Int]
batcherSort [] = []
batcherSort [x] = [x]

batcherSort xs =
    let half = length xs `div` 2
        (left, right) = splitAt half xs

        (sortedLeft, sortedRight) = runEval $ do
            leftResult <- rpar (force (batcherSort left))
            rightResult <- rpar (force (batcherSort right))

            leftSorted <- rseq leftResult
            rightSorted <- rseq rightResult

            return (leftSorted, rightSorted)

    in oddEvenMerge sortedLeft sortedRight

-- Перевірка відсортованості масиву
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) =
    x <= y && isSorted (y:xs)

-- Перевірка правильності результату
-- Порівняння з вбудованим сортуванням Haskell
isCorrect :: [Int] -> [Int] -> Bool
isCorrect original result =
    result == sort original

-- Головна функція
main :: IO ()
main = do
    putStrLn "=== Batcher Odd-Even Merge Sort ==="

    putStrLn "Enter array elements separated by spaces:"
    input <- getLine

    let numbers = map read (words input) :: [Int]

    if not (isPowerOfTwo (length numbers))
        then putStrLn "Array length must be a power of two."
        else do
            putStrLn "Enter number of threads (k):"
            kInput <- getLine

            case reads kInput of
                [(k, "")] ->
                    if k <= 0
                        then putStrLn "Number of threads must be positive."
                        else do
                            setNumCapabilities k

                            actualCapabilities <- getNumCapabilities

                            start <- getMonotonicTimeNSec

                            result <- evaluate (force (batcherSort numbers))

                            end <- getMonotonicTimeNSec

                            let elapsed =
                                    fromIntegral (end - start) / 1000000
                                        :: Double

                            putStrLn "\nSorted array:"
                            print result

                            putStrLn ("Execution time: " ++ show elapsed ++ " ms")

                            putStrLn ("Sorted correctly: " ++ show (isSorted result))

                            putStrLn ("Correct result: " ++ show (isCorrect numbers result))

                            putStrLn ("Requested threads: " ++ show k)

                            putStrLn ("Actual capabilities: " ++ show actualCapabilities)

                _ -> putStrLn "Invalid number of threads."