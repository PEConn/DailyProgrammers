-- Splits a list in half
split :: [a] -> ([a], [a])
split s = (take n s, drop n s)
	where n = ((length s) `div` 2)

-- Merges two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
	| x < y  = x:(merge xs (y:ys))
	| x >= y = y:(merge (x:xs) ys)

-- Sorts a list
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort x = merge (mergesort left) (mergesort right)
	where (left, right) = split x

