import System.IO 
import Data.List
import Text.Read

-- To make sure you didn't miss any, you scan the likely candidate boxes again, 
-- counting the number that have an ID containing exactly two of any letter and then 
-- separately counting those with exactly three of any letter. You can multiply those
-- two counts together to get a rudimentary checksum and compare it to what your device 
-- predicts.

-- read the thing, put it in lines
-- for every element (so use map):
-- check if a letter appears (exactly) twice, if yes put in list
-- take sizes of both lists
-- multiply

main = do
    input <- readFile "day2_input.txt"
    let list_input = lines input
    let two_times = map check_two list_input
    let three_times = map check_three list_input
    let two_times_filtered = (filter (/= False) two_times)
    let three_times_filtered = (filter (/= False) three_times)
    let val = (length two_times_filtered) * (length three_times_filtered)
    print two_times
    print two_times_filtered
    print val
    return(val) 

check_two :: [Char] -> Bool

check_two "" = False

check_two code 
    | (length code - length(filter (/= letter) (tail code)) == 2) = True | otherwise = check_two (filter (/= letter) (tail code))
    where letter = head code

check_three :: [Char] -> Bool
check_three "" = False
check_three code 
    | (length code - length(filter(/= letter) (tail code)) == 3) = True | otherwise = check_three(filter(/= letter) (tail code))
    where letter = head code
