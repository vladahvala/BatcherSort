module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Parallel.Strategies (runEval, rpar, rseq)
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

------------------------------------------------------------
-- Допоміжні функції
------------------------------------------------------------

-- Найменший степінь двійки, не менший за n
nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n = until (>= n) (* 2) 1

-- Елементи з парними позиціями (0, 2, 4, ...)
evens :: [Int] -> [Int]
evens (x:_:xs) = x : evens xs
evens [x]      = [x]
evens []       = []

-- Елементи з непарними позиціями (1, 3, 5, ...)
odds :: [Int] -> [Int]
odds (_:x:xs) = x : odds xs
odds _        = []

-- Почергове об'єднання двох списків однакової довжини
interleave :: [Int] -> [Int] -> [Int]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave [] []         = []
interleave _ _           = error "interleave: lists of different length"

-- Компаратори Бетчера: перший елемент лишається на місці,
-- далі порівнюються й за потреби міняються пари (1,2), (3,4), ...
compareSwap :: [Int] -> [Int]
compareSwap (x:xs) = x : comparePairs xs
compareSwap []     = []

comparePairs :: [Int] -> [Int]
comparePairs (x:y:xs) = min x y : max x y : comparePairs xs
comparePairs xs       = xs

------------------------------------------------------------
-- Паралельність
------------------------------------------------------------

-- Обчислює два незалежні результати.
-- Якщо parallel = True, перший обчислюється в окремому потоці (spark),
-- другий — у поточному; потім чекаємо обидва.
parPair :: NFData a => Bool -> a -> a -> (a, a)
parPair False a b = (a, b)
parPair True a b = runEval $ do
    a' <- rpar (force a)
    b' <- rseq (force b)
    _  <- rseq a'
    return (a', b')

------------------------------------------------------------
-- Обмінне сортування зі злиттям Бетчера
------------------------------------------------------------

-- Odd-even merge Бетчера з бюджетом k потоків.
-- Вхідні списки відсортовані й мають однакову довжину (степінь двійки).
-- Злиття парних і непарних позицій — незалежні задачі,
-- тому при k > 1 вони виконуються паралельно, а k ділиться між ними.
oddEvenMerge :: Int -> [Int] -> [Int] -> [Int]
oddEvenMerge _ [] []   = []
oddEvenMerge _ [x] [y] = if x <= y then [x, y] else [y, x]
oddEvenMerge k xs ys =
    let kl = k `div` 2
        kr = k - kl
        (evenPart, oddPart) =
            parPair (k > 1)
                (oddEvenMerge kl (evens xs) (evens ys))
                (oddEvenMerge kr (odds xs) (odds ys))
    in compareSwap (interleave evenPart oddPart)

-- Сортування Бетчера з k потоками.
-- Масив ділиться навпіл, потоки діляться між половинами (k/2 і k - k/2),
-- доки на гілку не залишиться один потік — далі сортування послідовне.
-- У результаті масив розбивається рівно на k частин, кожна сортується
-- у своєму потоці, після чого частини зливаються (теж паралельно).
batcherSort :: Int -> [Int] -> [Int]
batcherSort _ []  = []
batcherSort _ [x] = [x]
batcherSort k xs =
    let (left, right) = splitAt (length xs `div` 2) xs
        kl = k `div` 2
        kr = k - kl
        (sortedLeft, sortedRight) =
            parPair (k > 1) (batcherSort kl left) (batcherSort kr right)
    in oddEvenMerge k sortedLeft sortedRight

-- Сортування масиву довільної довжини:
-- доповнюємо до степеня двійки значеннями maxBound,
-- сортуємо й відкидаємо доповнення (воно опиняється в кінці).
sortArray :: Int -> [Int] -> [Int]
sortArray k xs =
    let n      = length xs
        padded = xs ++ replicate (nextPowerOfTwo n - n) maxBound
    in take n (batcherSort k padded)

------------------------------------------------------------
-- Перевірки
------------------------------------------------------------

isSorted :: [Int] -> Bool
isSorted (x:y:xs) = x <= y && isSorted (y:xs)
isSorted _        = True

-- Порівняння з вбудованим сортуванням Haskell
isCorrect :: [Int] -> [Int] -> Bool
isCorrect original result = result == sort original

------------------------------------------------------------
-- Ввід / вивід
------------------------------------------------------------

prompt :: String -> IO String
prompt msg = do
    putStrLn msg
    hFlush stdout
    getLine

-- Читає ціле додатне число, повторює запит при помилці
readPositive :: String -> IO Int
readPositive msg = do
    s <- prompt msg
    case readMaybe s of
        Just v | v > 0 -> return v
        _ -> do
            putStrLn "Error: enter a positive integer."
            readPositive msg

-- Читає масив цілих чисел, повторює запит при помилці
readArray :: IO [Int]
readArray = do
    s <- prompt "Enter array elements separated by spaces:"
    case mapM readMaybe (words s) of
        Just xs@(_:_) -> return xs
        _ -> do
            putStrLn "Error: enter at least one integer, separated by spaces."
            readArray

-- Псевдовипадковий масив (лінійний конгруентний генератор),
-- щоб не додавати залежність від пакета random
randomList :: Int -> Int -> [Int]
randomList seed n = take n (map (`mod` 100000) (drop 1 (iterate step seed)))
  where
    step s = (s * 1103515245 + 12345) `mod` 2147483648

-- Короткий вивід великих масивів
preview :: [Int] -> String
preview xs
    | length xs <= 32 = show xs
    | otherwise = show (take 10 xs) ++ " ... (" ++ show (length xs) ++ " elements)"

-- Час обчислення (у мс) повністю обчисленого списку
timed :: [Int] -> IO ([Int], Double)
timed xs = do
    start  <- getMonotonicTimeNSec
    result <- evaluate (force xs)
    end    <- getMonotonicTimeNSec
    return (result, fromIntegral (end - start) / 1e6)

------------------------------------------------------------
-- Головна функція
------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Batcher Odd-Even Merge Sort ==="
    cores <- getNumProcessors
    putStrLn ("CPU cores available: " ++ show cores)

    mode <- prompt "Choose input: 1 - enter array manually, 2 - generate random array"
    numbers <- case words mode of
        ["2"] -> do
            n    <- readPositive "Enter array size:"
            seed <- getMonotonicTimeNSec
            return (randomList (fromIntegral (seed `mod` 2147483648)) n)
        _ -> readArray

    k <- readPositive "Enter number of threads (k):"
    if k > cores
        then putStrLn ("Warning: k is greater than the number of CPU cores ("
                       ++ show cores ++ "), no extra speedup is expected.")
        else return ()

    -- Обчислюємо вхідний масив заздалегідь, щоб не враховувати це в часі
    _ <- evaluate (force numbers)

    -- Послідовний запуск (1 потік) — для порівняння
    setNumCapabilities 1
    (_, seqTime) <- timed (sortArray 1 numbers)

    -- Паралельний запуск з k потоками
    setNumCapabilities k
    actual <- getNumCapabilities
    (result, parTime) <- timed (sortArray k numbers)

    putStrLn ""
    putStrLn ("Input array:  " ++ preview numbers)
    putStrLn ("Sorted array: " ++ preview result)
    putStrLn ""
    putStrLn ("Threads requested (k): " ++ show k)
    putStrLn ("Capabilities in use:   " ++ show actual)
    putStrLn ("Time, 1 thread:  " ++ show seqTime ++ " ms")
    putStrLn ("Time, k threads: " ++ show parTime ++ " ms")
    if parTime > 0
        then putStrLn ("Speedup: " ++ show (seqTime / parTime))
        else return ()
    putStrLn ""
    putStrLn ("Sorted correctly: " ++ show (isSorted result))
    putStrLn ("Matches Data.List.sort: " ++ show (isCorrect numbers result))
