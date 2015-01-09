-- Implementation of Drop
myDrop :: Int->[a]->[a]
myDrop _ []=[]
myDrop 0 l=l
myDrop n (x:xs)=myDrop (n-1) xs

-- Implementation of take
myTake :: Int->[a]->[a]
myTake _ []=[]
myTake 0 l=[]
myTake n (x:xs)=x : myTake (n-1) xs

-- Implementation of dropWhile
myDropWhile ::(a->Bool)->[a]->[a]
myDropWhile _ []=[]
myDropWhile f (x:xs)| f x= myDropWhile f xs
                    | otherwise= x: xs

-- myDropWhileHOF ::(a->Bool)->[a]->[a]
-- myDropWhileHOF _ []=[]
-- myDropWhileHOF f xs= filter (not.f) xs

-- Implementation of takeWhile
myTakeWhile :: (a->Bool)->[a]->[a]
myTakeWhile _ []=[]
myTakeWhile f (x:xs)| f x = x: myTakeWhile f xs
                        | otherwise= myTakeWhile f []

-- myTakeWhileHOF :: (a->Bool)->[a]->[a]
-- myTakeWhileHOF _ []=[]
-- myTakeWhileHOF f xs=filter f xs

-- Implementation of length
myLength :: [a]->Int
myLength []=0
myLength [x] =1
myLength (x:xs)= myLength [x] + myLength(xs)

-- Implementaion of scanl
myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl _ value [] = [value]
myScanl f value (x:xs) = value: myScanl f (f value x) xs

-- scanr' :: (a -> b -> b) -> b -> [a] -> [b]
-- scanr' _ z [] = [z]
-- scanr' f z (x:xs) =  (f x z):(f x (scanr' f z xs))

--eg=scanr' (+) 0 [1,2,3]

-- Implementation of concat
myConcat :: [[a]] -> [a] 
myConcat []=[]
myConcat (x:xs)= x ++ myConcat(xs)

concatexample=myConcat [[1,2,3],[1,3,4],[2,3,4]]
