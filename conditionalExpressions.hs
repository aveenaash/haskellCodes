absVal n= if n >= 0 then n else -n

signNum x= if x<0 then -1 else
                  if x==0 then 0 else 1

absValGuard n | n>=0 =n
              | otherwise = -n

main= do print $ absVal(-5) 
         print (signNum (-5))
         print $ absValGuard(-5)
