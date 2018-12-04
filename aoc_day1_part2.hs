

main = do
    input <- readFile "day1_input" 
    let list_input = lines input
    let readed = map make_readable list_input
    let result = Main.repeat readed
    print result

    

make_readable :: String -> Int

make_readable num_string = 
    let num | head(num_string) == '+' = read(tail num_string) | head(num_string) == '-' = 0 - read(tail num_string)
    in num

-- repeat :: [Int] -> (Int -> [Int] -> [Int]) -> Int

repeat num_list = repeat' start num_list [] where
    start = 0
    repeat' num [] new_list = repeat' num num_list new_list
    repeat' num input_list new_list 
        | (num +  (head input_list)) `elem` new_list = num + (head input_list)
        | otherwise = repeat' (num+ (head input_list)) (tail input_list) (new_list++ [(num+ (head input_list))])