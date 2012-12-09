-- Alpha Numeric predicate
isAlphaNum :: Char -> Bool
isAlphaNum = (`elem` (['a'..'z'] ++ ['A'..'Z']))

-- Quicksort using filter
quicksort :: ( Ord a ) => [a] -> [a]
quicksort [] = []
quicksort (h:t) = quicksort (filter (<h) t) ++ [h] ++ quicksort (filter (>=h) t)

-- Collatz sequence
collatz :: (Integral a ) => a -> [a]
collatz 1 = [1]
collatz x 
    | odd x = x : collatz (x*3 + 1)
    | even x = x : collatz ( div x 2 )

-- Elem function implemented with left folding
elem' :: (Eq a) => a -> [a] -> Bool
elem' e = foldl (\acc x -> x == e || acc ) False

-- Reversing a list using right folding
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []


