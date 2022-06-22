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

take' = undefined
drop' = undefined
null' = undefined


reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


elem' = undefined
replicate' = undefined
maximum' = undefined
mininum' = undefined
sum' = undefined
product' = undefined

-- tuple
fst' (a,_) = a
snd' = undefined

zip' = undefined
unzip' = undefined