--input 2 binary numbers (a,b), output binary number x, where x is the smallest number with
--the most digits (1) in range a....b

mostones :: [Int] -> [Int] ->[Int]
mostones2 :: [Int] -> [Int] ->[Int]
check :: [Int]->[Int]
firstcase :: [Int] -> [Int]

mostones ls rs = check (mostones2 ls rs)

check [0] = [0]
check (y:rs) 	| y==0 = rs
				|otherwise = (y:rs)

mostones2 [0] [0] = [0]
mostones2 [0] (y:rs) |y==1 = firstcase (y:rs)
					|otherwise = mostones2 [0] rs

mostones2 (x:ls) (y:rs) 	|x==1 && y==1 && length (x:ls) < length (y:rs) 
							= firstcase (y:rs)
						|x==1 && y==1 && length (x:ls) == length (y:rs)
							= [1] ++ mostones2 ls rs 
						|x==1 && y==0
							= mostones2 (x:ls) rs
						|x==0 && y==1
							= mostones2	ls (y:rs)
						|x==0 && y==0 
							= [0] ++ mostones2 ls rs 
							
firstcase [] = []							
firstcase (z:zs) 	|z==1 = firstcase zs ++ [1]
					|otherwise = [0] ++ addrest zs 

addrest [] = []
addrest (z:zs) = addrest zs ++ [1]
