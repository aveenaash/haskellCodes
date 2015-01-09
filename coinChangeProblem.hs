type Coin = Int
type Change= [(Int,Coin)]

checkIfChange :: Int->Int->Bool
checkIfChange x y|a>0 = True
                 |otherwise = False
                   where a=x `div` y

change :: Int-> [Coin]->Change
change 0 _ = []
change _ [] = []
change t (x:xs)| checkIfChange t x = (a, x) : (change b xs)
               | otherwise = (change t xs)
                    where a= t `div` x
                          b= t `mod` x

t2a=change 9 [20,10,5,4,1]
t2b=changeOptimalHelper 99 [20,10,5,4,1]


changeOptimalHelper :: Int-> [Coin]->[Change]
changeOptimalHelper 0 _ = []
changeOptimalHelper _ [] = []
changeOptimalHelper t all@(x:xs) | t >= x    = map ((quot t x, x):) (changeOptimalHelper (t `mod` x) xs) ++ changeOptimalHelper t xs
                                 | otherwise = changeOptimalHelper t xs
                                  
-- t2a'=changeOptimal 10 [20,10,5,4,1]
-- t2b'=changeOptimal 20 [20,10,5,4,1]

main=do print t2a
        print t2b
       -- print t2a'
       -- print t2b'
