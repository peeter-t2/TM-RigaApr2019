#Originally compiled by David Lorenz 2017, see R_Glossary v1-4.pdf
#Adapted for R tidyverse workshop in Tartu Winter School 2018, by Peeter Tinits

# Run* these commands one by one (line by line). Mostly, you will be able to figure out what's going on, even without fully understanding the "language"...

## *You can either copy-paste a line into the R console and hit Enter. Or run it from this script by CNTRL+R (Windows) or CMD+Enter (Mac). This executes the line that the cursor is in. (Or highlight several lines, then these are executed at once.)

#Recommended to turn on soft-wrap:
#Tools -> Global options -> Code -> Editing -> Soft-wrap R source files (turn it on)


#Simple operations
1+1
4-2
4*2
27*17
459/17
sqrt(25)

#Making variables/containers
x <- 4
y <- 2
x_2 = 4
y_2 = 2
x
y

#Operations with variables
x+y
x*y

#Check if matches
1==1
1==2
x==y
x==y*2

#Make complex variables
z <- x+y*2
z <- (x+y)*2
z

#Make and read vectors
1:10
myFirstVector <- c(1:10)
myFirstVector
mySecondVector <- c("a", "b", "c", "d","e","f","g","h","i","j")
mySecondVector
class(myFirstVector)
class(mySecondVector)
myThirdVector <- c(a,b,c,d)  # does not work - why?
myThirdVector <- c("a","b","c","d")


myFirstVector[6]
mySecondVector[c(2,3,5)]  # what does c() inside the square brackets do?
myFirstVector[6,8,9] # does not work, needs c()


myFirstVector %in%  mySecondVector #no matches
mySecondVector %in%  myThirdVector #first four match


length(myFirstVector)

#make simple tables
anotherVector <- c(11:20)
myFirstMatrix <- cbind(myFirstVector, anotherVector) 
myFirstMatrix 
mySecondMatrix <- rbind(myFirstVector, anotherVector)
mySecondMatrix  # what's the difference between cbind and rbind?
myFirstMatrix[3,2]
myFirstMatrix[3,]
myFirstMatrix[,2] # what do the square brackets do now?

#simple table information
myFirstDataFrame <- data.frame(myFirstMatrix)
nrow(myFirstDataFrame)
ncol(myFirstDataFrame)
dim(myFirstDataFrame)
str(myFirstDataFrame)   # Data frame is the format we will mostly use.

#Operations with vectors
myFirstVector + anotherVector  # what is this?
myFirstDataFrame$sums <- myFirstDataFrame$myFirstVector + myFirstDataFrame$anotherVector
myFirstDataFrame$sums  # see what this did?
str(myFirstDataFrame)  # what has changed?

myFirstDataFrame$letters <- mySecondVector  # recall that 'mySecondVector' was "a", "b", "c", "d", etc.

str(myFirstDataFrame)  # what has changed?

# now check this out:
QuickDataFrame <- data.frame(letters=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), numbers=1:10)
QuickDataFrame

ls()   # 'ls' stands for 'list space'...

#Save a file
save(myFirstDataFrame, file="myFirstDataFrame.RData")
#hm, cool trick

getwd()   ## This shows the directory on your computer that is currently your 'working directory'. Check if there's a file "myFirstDataFrame.RData"!

# Now close R, then double-click the file "myFirstDataFrame.RData"
