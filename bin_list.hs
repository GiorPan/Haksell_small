--infinite list of binary numbers increasing in value and length
--run "allbin"

allbin = [0] : [1] : [ys | ys <- mybin (tail allbin)]
mybin (x:xs) = if length x == sum x then [i-i|i<-[0..length x]]:mybin xs else if last x ==1 then 
  ((take (length x - (length (takeWhile (==1) (reverse x)) +1)) x)
    ++[1]++[i-i|i<-[1..(length  (takeWhile (==1) (reverse x) ) ) ]])
      :mybin xs else (init x ++ [1]):mybin xs
