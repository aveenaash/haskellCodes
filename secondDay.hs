
fac 0=1
fac n= n* fac(n-1)

combination1 m n= fac(m) / (fac n * fac(m-n))

combination2 m 0=1
combination2 m n=(m/n)* (combination2(m-1)(n-1))

-- this algorithm loops infinitely beacuase it doesnt have base condition for m=0
--combination3 m 0=1
--combination3 m n= combination3(m-1)(n) + combination3(m-1)(n-1)

main= do print $ combination1 4 3
         print $ combination2 4 3
         --print $ combination3 4 3
