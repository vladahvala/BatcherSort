module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Parallel.Strategies (runEval, rpar, rseq)
import GHC.Conc (setNumCapabilities)
import GHC.Clock (getMonotonicTimeNSec)

-- Перевірка, чи є довжина масиву степенем двійки
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
    | n <= 0 = False
    | n == 1 = True
    | even n = isPowerOfTwo (n `div` 2)
    | otherwise = False

-- Отримання елементів на парних позиціях
evenElements :: [Int] -> [Int]
evenElements (x:_:xs) = x : evenElements xs
evenElements [x] = [x]
evenElements [] = []

-- Отримання елементів на непарних позиціях
oddElements :: [Int] -> [Int]
oddElements (_:x:xs) = x : oddElements xs
oddElements _ = []

-- Об'єднання двох списків почергово
interleave :: [Int] -> [Int] -> [Int]
interleave (x:xs) (y:ys) =
    x : y : interleave xs ys
interleave [] [] = []
interleave _ _ = error "Lists must have equal lengths"

-- Порівняння та обмін сусідніх елементів
compareAdjacent :: [Int] -> [Int]
compareAdjacent (x:y:xs) =
    min x y : max x y : compareAdjacent xs
compareAdjacent xs = xs

-- Об'єднання двох відсортованих половин за Бетчером
oddEvenMerge :: [Int] -> [Int] -> [Int]
oddEvenMerge [x] [y]
    | x <= y = [x, y]
    | otherwise = [y, x]

oddEvenMerge xs ys =
    let evenMerged =
            oddEvenMerge (evenElements xs) (evenElements ys)

        oddMerged =
            oddEvenMerge (oddElements xs) (oddElements ys)

        merged = interleave evenMerged oddMerged

    in case merged of
        [] -> []
        (x:rest) -> x : compareAdjacent rest

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

-- Перевірка правильності сортування
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) =
    x <= y && isSorted (y:xs)

-- Головна функція
main :: IO ()
main = do
    putStrLn "=== Batcher Parallel Sort ==="

    putStrLn "Enter array elements separated by spaces:"
    input <- getLine

    let numbers = map read (words input) :: [Int]

    if not (isPowerOfTwo (length numbers))
        then putStrLn "Error: array length must be a power of two."
        else do
            putStrLn "Enter number of threads (k):"
            k <- readLn

            if k <= 0
                then putStrLn "Error: k must be positive."
                else do
                    setNumCapabilities k

                    start <- getMonotonicTimeNSec

                    result <- evaluate (force (batcherSort numbers))

                    end <- getMonotonicTimeNSec

                    let elapsed =
                            fromIntegral (end - start) / 1000000
                                :: Double

                    putStrLn "\nSorted array:"
                    print result

                    putStrLn ("Execution time: " ++ show elapsed ++ " ms")

                    putStrLn ("Correct result: " ++ show (isSorted result))
                    putStrLn ("Number of threads: " ++ show k)