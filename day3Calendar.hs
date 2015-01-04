--import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

days = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"]

bar= '+': (concat (take 7 $ repeat "---+"))


fction s d= s ++ d ++ "|"
--daysRow= concat $ zipWith (++) days (repeat "|")  
-- Above function is also right
daysRow= foldl fction "|" days

getDay (year, week, day) = day

-- gets number of days in a month
noOfDaysInMonth year month= gregorianMonthLength year month

-- starting date of a month (always 1)
startingDate year month=fromGregorian year month 1



--statrting day of a month
--startDay = ((getDay weekInfo) `mod` 7) + 1

--startingDay= ((getDay weekInfo) `mod` 7) + 1
--weekInfo = toWeekDate (startingDate 2014 10)
                                 

emptyBoxes  n = concat ["|   "| x<-[1..n]]

printCalendar year month= firstWeekOfMonth ++bar++"\n"++ (restOfTheMonth (9-startingDay))++"\n"++bar
                           where startingDay= ((getDay weekInfo) `mod` 7) + 1
                                 weekInfo = toWeekDate (startingDate year month)
                                 firstWeekOfMonth = emptyBoxes (startingDay-1)++ concat ["|  "++ show(x)| x<-[1..(8-startingDay)]] ++ "|\n"
                                 restOfTheMonth restStart | restStart > (noOfDaysInMonth year month) = emptyBoxes (35-(noOfDaysInMonth year month)-(startingDay-1)) ++ "|"
                                                          | (restStart `mod` 7) ==(9-startingDay) = (dayWithOneOrTwoDigit restStart) ++ restOfTheMonth (restStart+1)
                                                          | (restStart `mod` 7) ==(8-startingDay) = (dayWithOneOrTwoDigit restStart)++ "|\n"++ bar++"\n" ++ restOfTheMonth (restStart+1)
                                                          | otherwise = (dayWithOneOrTwoDigit restStart) ++ restOfTheMonth (restStart+1)
dayWithOneOrTwoDigit n | n>=10 = "| "++ show(n)
                       | otherwise = "|  "++ show(n)

main=do putStrLn bar
        putStrLn daysRow
        putStrLn bar
        putStrLn $ printCalendar 2014 10
