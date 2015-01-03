
stepOneIncrement a 0 =a
stepOneIncrement a x =0.5*(x + a/x)

sqroot a 0 initialGuess = initialGuess
sqroot a n initialGuess= sqroot a (n-1) (stepOneIncrement a initialGuess)

allResults=[sqroot 2 n 1| n<-[1..10]]



-- This method is similar to sqroot except it has nested function
--stepIncrement value steps = stepImprovement value steps 1 where
--    stepImprovement value 0 i = i
--    stepImprovement value steps i = stepImprovement value (steps-1) (stepOneIncrement value i)

main=do print $ stepOneIncrement 2 0
        print $ sqroot 2 10 1  
--        print $ stepIncrement 2 10
        print allResults
