calc :: Int -> Int
calc x = (x `div` 3) - 2

rec_calc :: Int -> Int
rec_calc x 
    | x <= 0 = 0
    | otherwise = extra + rec_calc extra
    where
        extra = max 0 (calc x)

solve1 :: [Int] -> Int
solve1 = sum . map calc

solve2 :: [Int] -> Int
solve2 = sum . map rec_calc

newline str = str ++ "\n"

solveBoth :: [Int] -> (Int, Int)
solveBoth xs = (solve1 xs, solve2 xs)

main = interact (newline . show . solveBoth . map read . lines)
