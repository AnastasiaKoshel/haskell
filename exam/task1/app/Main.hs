generateList :: Int -> [[Int]]
generateList 1 = [[1], [2], [3]]
generateList n = (map (1:) (generateList (n-1))) ++ (map (2:) (generateList (n-1))) ++ (map (3:) (generateList (n-1)))


removeRedundant :: [[Int]] -> [[Int]]
removeRedundant []= []
removeRedundant (x:xs) | (smallerFour x) = x : (removeRedundant xs)
                       | otherwise = removeRedundant xs

smallerFour:: [Int] -> Bool
smallerFour [] = True
smallerFour (x:[]) = True
smallerFour (x:y:xs) | x+y > 4 = False 
                     | otherwise = smallerFour xs

main = do 
    let res = removeRedundant $ generateList 4
    putStrLn ( show res)
