-- Simple functions

doubleMe x = x + x

doubleSmallNumber x = 
	if x > 100
	  then x
	  else doubleMe x

oddp x = 
	if x `mod` 2 == 1
	  then True
	  else False
