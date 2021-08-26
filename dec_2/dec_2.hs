import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)

doAdrOp :: (Int -> Int -> Int) -> [Int] -> Int -> [Int]
doAdrOp f mem pc = intcode new_mem new_pc
  where
    (lhs, rhs, out) = getThree mem pc
    pre = take out mem
    post = drop (out + 1) mem
    new_val = f (mem !! lhs) (mem !! rhs)
    new_pc = pc + 4
    new_mem = pre ++ [new_val] ++ post

getThree :: [Int] -> Int -> (Int, Int, Int)
getThree mem pc = (lhs, rhs, out)
  where
    lhs = mem !! (pc + 1)
    rhs = mem !! (pc + 2)
    out = mem !! (pc + 3)

intcode :: [Int] -> Int -> [Int]
intcode mem pc
  | mem !! pc == 99 = mem
  | mem !! pc == 1 = doAdrOp (+) mem pc
  | mem !! pc == 2 = doAdrOp (*) mem pc
  | otherwise = error "Invalid opcode!"

intcodeWithInputs :: Int -> Int -> [Int] -> Int
intcodeWithInputs a b [] = error "No program"
intcodeWithInputs a b (x : pre) = head post
  where
    treated = x : [a, b] ++ drop 2 pre
    post = intcode treated 0

solve1 :: [Int] -> Int
solve1 = intcodeWithInputs 12 2

solve2 :: [Int] -> Int
solve2 mem = 100 * noun + verb
  where
    magicValue = 19690720
    f (a, b) = magicValue == intcodeWithInputs a b mem
    allPairs = [(a, b) | a <- [0 .. 99], b <- [0 .. 99]]
    (noun, verb) = head . filter f $ allPairs

newline :: [Char] -> [Char]
newline str = str ++ "\n"

solveBoth :: [Int] -> (Int, Int)
solveBoth xs = (solve1 xs, solve2 xs)

main :: IO ()
main = interact (newline . show . solveBoth . map read . splitOn ",")
