--give 2 graphs ([(1,3),(3,5),(5,1),(5,7)] for example) and check if they
--are isomorphic "NOT COMPLETED"
--example run "isomorphic [(1,3),(3,5),(5,1),(5,7)] [(4,5),(2,3),(3,4),(4,2)] --> True"

isomorphic ::[(Int,Int)]->[(Int,Int)]->Bool
ch_edges ::[(Int,Int)]->[(Int,Int)]->Bool
ch_vert ::[(Int,Int)]->[Int]->[Int]
degree :: [Int] -> [Int] ->[Int]
ch_rest :: [Int]->[Int]->[(Int,Int)]->[(Int,Int)]->Bool

isomorphic xs ys	|ch_edges xs ys == True && length (ch_vert xs []) == length(ch_vert ys []) 
						= ch_rest (ch_vert xs []) (ch_vert ys []) xs ys
					|otherwise = False
					
ch_edges xs ys 	| length xs == length ys = True
				| otherwise = False

ch_vert [] ver = ver				
ch_vert ((x,y):xs) ver 	| member x ver == True && member y ver == True = ch_vert xs ver
						| member x ver == True && member y ver == False = ch_vert xs (ver ++ [y])
						| member x ver == False && member y ver == True = ch_vert xs (ver ++ [x])
						| member x ver == False && member y ver == False = ch_vert xs (ver ++ [y]++[x])

ch_rest verx very xs ys | ch_degree (degree verx (merg (unzip xs))) (degree very (merg (unzip ys))) == True = True
						|otherwise = False
						
ch_degree [] [] = True
ch_degree (x:xs) (y:ys) | x==y = ch_degree xs ys
						|otherwise = False
						
degree [] xs =[]						
degree (v:ver) (x:xs) = mysort ([length [i|i<-(x:xs),i-v==0]] ++ degree ver (x:xs))


merg ((x:xs),(y:ys)) = (x:xs)++(y:ys) 

mydel x [] = []
mydel x (y:ys) 	| x==y = mydel x ys
				| otherwise = (y:mydel x ys)

myless x [] = []
myless x (y:ys) | y<x = (y:myless x ys)
				|otherwise = myless x ys
				
mygreateq x [] = []
mygreateq x (y:ys) 	| y>=x = (y:mygreateq x ys)
					|otherwise = mygreateq x ys
					
mysort [] = []
mysort (x:xs) = (mysort (myless x xs)) ++ [x] ++ (mysort (mygreateq x xs))

member x [] = False
member x (y:xs) | x == y = True
				|otherwise = member x xs 
