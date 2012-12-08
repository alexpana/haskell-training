-- Recursive Maximum Function
maximum' :: (Ord a) => [a] -> Maybe a
maximum' [] = Nothing
maximum' [x] = Just x
maximum' (h:t)
	| h > maxTail = Just h
	| otherwise = Just maxTail
	where maxTail = maximum t

-- Recursive Fibonacci Function
fibo :: (Num a) => a -> a
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- Replicate
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x : replicate' (n-1) x

-- Take
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' 0 _	= []
take' n (h:t) = h : take' (n-1) t

-- Reverse
reverse' :: [a] -> [a]
reverse' [] 	= []
reverse' (h:t) 	= reverse t ++ [h]

-- Repeat
repeat' :: a -> [a]
repeat' a = a : repeat' a

-- Zip
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h1:t1) (h2:t2) = (h1, h2) : zip' t1 t2

-- Elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t)
	| x == h = True
	| otherwise = elem' x t

-- QuickSort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (h:t) =
	smaller ++ [h] ++ greater
	where
		smaller = quicksort [ a | a <- t, a <= h ]
		greater = quicksort [ a | a <- t, a > h ]
