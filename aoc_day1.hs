import System.IO
import Data.List
import Text.Read

main = do
    input <- readFile "day1_input" 
    let list_input = lines input
    let input_nums = map make_readable list_input
    print(sum input_nums)
    return(sum input_nums)

make_readable :: String -> Int

make_readable num_string = 
    let num | head(num_string) == '+' = read(tail num_string) | head(num_string) == '-' = 0 - read(tail num_string)
    in num

