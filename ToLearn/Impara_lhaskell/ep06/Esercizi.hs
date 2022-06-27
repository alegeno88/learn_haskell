-- liste
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

head' :: [a] -> a
head' [] = error "Non ci sono elementi in questa lista"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = error "Non ci sono elementi in questa lista"
tail' (x:xs) = xs
                                        
last' :: [a] -> a
last' [] = error "Non ci sono elementi in questa lista"
last' xs = head' (reverse' xs)


init' :: [a] -> [a]
init' [] = error "Non ci sono elementi in questa lista"
init' [x] = []
init' (x:xs) = x:init' xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: Int -> [a] -> [a]  
take' 0 _ = []
take' _ [] = []
-- take' 1 xs = head' xs
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

--null' xs = length' xs == 0 
null' :: [a] -> Bool
null' [] = True
null' _ = False


elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = y == x || elem' y xs 

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]


maximum' :: Ord a => [a] -> a
maximum' [] = error "Non ci sono elementi in questa lista"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

minimum' :: Ord a => [a] -> a
mininum' [] = error "Non ci sono elementi in questa lista"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)


sum' :: Num a => [a] -> a
sum' [] = error "Non ci sono elementi in questa lista"
sum' [x] = x
sum' (x:xs) = x + sum' xs


product' :: Num a => [a] -> a
product' [] = error "Non ci sono elementi in questa lista"
product' [x] = x
product' (x:xs) = x * product' xs

-- tuple
fst' :: (a,b) -> a
fst' (a,_) = a

snd' :: (a,b) -> b
snd' (_, a)= a    

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((a,b):xs) = (a:fst'(unzip' xs), b:snd'(unzip' xs))