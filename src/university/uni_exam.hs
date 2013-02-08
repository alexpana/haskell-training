-- P01 Sa se defineasca o functie care insumeaza toate numerele naturale pare mai mici sau egale cu n
sum_n :: Int -> Int
sum_n n
	| n < 0 = error "Negative input"
	| otherwise = foldr (\x y -> x + y) 0 [ x | x <- [1..n], (mod x 2) == 0 ]

-- P02 Numarul de permutari, aranjamente si combinari
p :: Int -> Int
p n
	| n < 0 = error "Negative input"
	| n == 0 = 1
	| otherwise = n * (p $ n - 1)

c :: Int -> Int -> Int
c n k
	| k > n = error "Invalid input"
	| otherwise = p n `div` (p k * p (n-k))
	
a :: Int -> Int -> Int
a n k
	| k > n || k == 0 = error "Invalid input"
	| otherwise = p n `div` p ( n - k )
	
-- P03 An bisect
bisect :: Int -> Bool
bisect y
	| y < 0 || y > 4900 = error "Outside range"
	| y <= 1582 && y `mod` 4 == 0 = True
	| y `mod` 4 == 0  && y `mod` 100 /= 0 = True
	| y `mod` 400 == 0 = True
	| otherwise = False
	
-- P04 Definiti operatori pentru numere complexe reprezentate prin tuple
(+++) :: (Float, Float) -> (Float, Float) -> (Float, Float)
a +++ b = ( fst a + fst b, snd a + snd b)
	
(###) :: (Float, Float) -> (Float, Float) -> (Float, Float)
a ### b = ( fst a - fst b, snd a - snd b)

(***) :: (Float, Float) -> (Float, Float) -> (Float, Float)
a *** b = (ax * bx + ax * by, ay * bx + ay * by )
	where
		ax = (fst a)
		ay = (snd a)
		bx = (fst b)
		by = (snd b)

(===) :: (Float, Float) -> (Float, Float) -> Bool
a === b = fst a == fst b && snd a == snd b
	
-- P05 Suma cifrelor
sumcif :: Int -> Int
sumcif n
	| n < 0 = sumcif (-n)
	| n < 10 = n
	| otherwise = n `mod` 10 + sumcif ( n `div` 10 )
	
-- P06 Ridicare la butere
(^^*) :: Int -> Int -> Int
b ^^* e
	| e < 0 = error "Exponent negativ"
	| e == 0 = 1
	| otherwise = b * ( b ^^* (e-1))
	
-- P07 Transformati un Int intr-un String
itos :: Int -> String
itos n = show n

-- P08 Sa se simplifice o fractie
cmmdc :: Int -> Int -> Int
cmmdc a b
	| a `mod` b == 0 = b
	| otherwise = cmmdc a (a `mod` b)

simplify :: Int -> Int -> (Int, Int)
simplify x y
	| y == 0 = error "Divizion by zero"
	| otherwise = ( x `div` s, y `div` s)
		where
			s = cmmdc x y

-- P09 Sa se rezolve ecuatia de gradul I "ax + b = 0"
solve :: Float -> Float -> Float
solve a b
	| a == 0 && b == 0 = error "Ecuatie nedeterminata"
	| a == 0 && b /= 0 = error "Ecuatia nu are solutie"
	| otherwise = (-b) / a

-- P10 Sa se verifice daca un numar este prim sau nu
prim :: Int -> Bool
prim n
	| n < 2 = error "Input is less than 2"
	| otherwise = not $ foldr (\x y -> x || y) False (map (\x -> (n `mod` x) == 0) factors)
	where
		factors = [ x | x <- [2 .. n `div` 2], x == 2 || x `mod` 2 == 1]

-- P11 Sa se returneze al n-lea numar din sirul Fibonacci
fibo :: Int -> Int
fibo n
	| n < 1 = error "Invalid index"
	| n <= 2 = 1
	| otherwise = fibo (n-1) + fibo (n-2)

-- P12 Sa se calculeze cmmmc folosind relatia cmmmc(m,n) = m * n / cmmdc(m,n)
cmmmc :: Int -> Int -> Int
cmmmc m n
	| m == 0 || n == 0 = error "Null input"
	| otherwise = m * ( n `div` cmmdc m n )