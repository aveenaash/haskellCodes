
-- Question 1 solution
--type Name=String
type Age= Int
type Year=Int
data Major= Fr|So|Jr|Sr
  deriving (Show,Eq)

data Student = Student {name ::String, age:: Age, year:: Year, major:: Major}
  deriving (Show)

checkSenior:: Student->Bool
checkSenior (Student name age year major)| major==Sr = True
                                         | otherwise = False

stdArray=[(Student "Abinash" 23 2012 Fr),(Student "Ram" 23 2014 Sr),(Student "HariRam" 22 2014 Jr),(Student "Mulaaa" 23 2013 Sr)]

findAllSeniors= filter checkSenior stdArray

-- Student is made using recordSyntax so we can use the accessor method called name to get the name of student
-- for example
-- name (Student "Abinash" 23 2012 Fr) will give Abinash

-- map is a HOF that applies a function to a list and gives a list
-- In below code map applies 'name' function to findAllSeniors list and gives list of String
nameOfAllSeniors=map name findAllSeniors
