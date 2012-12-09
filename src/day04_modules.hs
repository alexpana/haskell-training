-- Import the list module
import Data.List

-- Count the unique elements in a list using the nub function
countUniques :: (Eq a) => [a] -> Int
countUniques = length . nub


