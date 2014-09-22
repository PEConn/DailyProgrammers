import Data.List

lookNSay :: [Char] -> [Char]
lookNSay x = concat $ map lookAt $ group x
	where lookAt g = (show (length g)) ++ [ g!!0 ]

lookNSayIterate :: [Char] -> Int -> [Char]
lookNSayIterate x 0 = x
lookNSayIterate x n = lookNSayIterate (lookNSay x) (n-1)
