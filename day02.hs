ceva x y = x * x * x

lucky :: (Integral a) => a -> String
lucky 7 = "Very lucky"
lucky 3 = "Most unlucky"
lucky x = "Not lucky"

_first [] = error "Can't work with an empty list!"
_first (x : y) = x

sumOfAll :: (Num a) => [a] -> a
sumOfAll [] = 0
sumOfAll [x] = x
sumOfAll (h:t) = h + sumOfAll t

bmi w h = w / (h*h)

bmiTell w h
	| bmi < snd skinny 	= "Skinny"
	| bmi < snd normal 	= "Normal"
	| bmi < snd	fat		= "Fat"
	| otherwise 		= "I don't know"
	where
		bmi = w / (h * h)
		skinny 	= (0,18)
		normal 	= (18,25)
		fat 	= (25,30)

head' :: [a] -> a
head' x =
	case x of
		[] 		-> error "Nope, Chuck Testa!"
		(h:_) 	-> h

-- swap two elements in a tuple
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

lswap :: (a, a) -> [a]
lswap (x, y) = x : [y]
