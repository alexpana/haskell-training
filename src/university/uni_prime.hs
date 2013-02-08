-- Versiune Originala

prim1 n m = if m > n `div` 2 then True
            else if n `mod` m == 0 then False
                                   else prim1 n (m + 2)

prim n = if n == 2 then True
         else if n `mod` 2 == 0 then False
                                else prim1 n 3
cifra c = case c of
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"

intreg n = if n < 10 then cifra n
           else intreg ( n `div` 10 ) ++ cifra ( n `mod` 10 )

tabprim1 n m = if n < m then ""
               else if prim m then intreg m ++ " " ++ tabprim1 n ( m + 2 )
                    else tabprim1 n ( m + 2 )

tabprim n = if n < 2 then " "
            else if n == 2 then "2"
                 else "2 " ++ tabprim1 n 3


-- Versiune Rescrisa

primeIter :: Int -> Int -> Bool
primeIter n m
    | m > n `div` 2     = True
    | n `mod` m == 0    = False
    | otherwise         = primeIter n ( m + 2 )

isPrime :: Int -> Bool
isPrime n
    | n == 2            = True
    | n `mod` 2 == 0    = False
    | otherwise         = primeIter n 3

printPrimes :: Int -> String
printPrimes n
    | n < 2     = ""
    | isPrime n = printPrimes (n - 1) ++ " " ++ (show n)
    | otherwise = printPrimes (n - 1)

-- Versiune Functionala

printPrimes2 n = 
    let 
        primes = filter isPrime [1..n]
    in
        foldl (\ acc x -> acc ++ (show x) ++ " " ) "" primes
