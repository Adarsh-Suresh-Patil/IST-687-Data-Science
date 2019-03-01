#Basics of R

#1. Define variables
#Define a vector with values 4.0, 3.3 and 3.7
grades <- c(4.0, 3.3, 3.7)

#Define vector with values "Bio", "Math", "History"
course <- c("Bio", "Math", "History")

#Define a variable of value 3
betterthanB <- 3

#2. Calculate statistics using R
#Avg of grades
avggrades <- mean(grades)
avggrades

#calculate length of vector
lengrades <- length(grades)
lengrades

#calculate sum of grades
sumgrades <- sum(grades)
sumgrades

#calculate min and max of grades
highestgrade <- max(grades)
highestgrade
lowestgrade <- min(grades)
lowestgrade

#add 0.3 to all elements of vector and calculate average
bettergrades <- grades + 0.3
bettergrades

avgbettergrades <- mean(bettergrades)
avgbettergrades

#Conditional IF statement in R
#calculate max of grades is higher than 3.5
if(highestgrade > 3.5) "Yes" else "No"

#calculate min of grades is higher than betterthanB
if (lowestgrade > betterthanB) "Yes" else "No"

#Accessing elements in a vector
#Access 2nd element of vector course
course[2]
