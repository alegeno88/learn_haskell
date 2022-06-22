-- liste
length' [] = 0
length' (_:xs) = 1 + length' xs

head' [] = error "Non ci sono elementi in questa lista"
head' (x:xs) = x

tail' [] = error "Non ci sono elementi in questa lista"
tail' (x:xs) = xs


last' [] = error "Non ci sono elementi in questa lista"
last' xs = head' (reverse' xs)


init' [] = error "Non ci sono elementi in questa lista"
init' [x] = []
init' (x:xs) = x:init' xs 

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' 0 _ = []
take' 1 xs = head' xs
take' n (x:xs) = x ++ take' (n-1) xs 

drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

--null' xs = length' xs == 0 
null' [] = True
null' _ = False

elem' _ [] = False
elem' y (x:xs) = (y == x) || elem' y xs 

replicate' n x = [x | _ <- [1..n]]

maximum' [] = error "Non ci sono elementi in questa lista"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

mininum' [] = error "Non ci sono elementi in questa lista"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

sum' [] = error "Non ci sono elementi in questa lista"
sum' [x] = x
sum' (x:xs) = x + sum' xs

product' [] = error "Non ci sono elementi in questa lista"
product' [x] = x
product' (x:xs) = x * product' xs

-- tuple
fst' (a,_) = a
snd' (_, a)= a

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

unzip' [] = ([],[])
unzip' ((a,b):xs) = (a:fst'(unzip' xs), b:snd'(unzip' xs))