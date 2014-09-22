quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lower) ++ [x] ++ (quicksort higher)
	where lower = filter (\y -> y <= x) xs
	      higher = filter (\y -> y > x) xs
