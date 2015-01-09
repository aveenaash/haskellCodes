
-- square of numbers
numSquares=[x*x| x <-[1..10]]

--A function that counts and lists all the perfect triples up to 100
triples=[(a,b,c)| a<-[1..100],b<-[1..100],c<-[1..100],a*a+b*b==c*c]
tripleCount=length triples

lsum []=0
lsum (x:xs)=x + lsum(xs)

average []=0
average (xs)= fromIntegral(lsum(xs)) / fromIntegral(length(xs))

splitList xs=[filter even xs, filter odd xs]

partList xs p = [l1,l2]
                    where l1=[x|x<-xs,p x]
                          l2=[x|x<-xs,not(p x)]

circleArea r = let pi=3.14159
                  in r*r*pi

fac n = fac' n 1
        where fac' 1 a = a
              fac' n a = fac' (n-1) (n*a)

main=do print numSquares
        print triples
        print tripleCount
        print (average[1..10])
        print $ splitList[1..20]
