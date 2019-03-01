#Functions (continued), loops, and Sampling

#Functions:
#Create a function to print the following information for the vector supplied in the argument:
#a.	Mean: average
#b.	Median: line bisecting a data set into two equal halves
#c.	Min & Max
#d.	Standard deviation: SUM((value - mean)square)/No Of Observations
#e.	0.05 and 0.95 quantiles (Use the quantile( ) function)

Myfunc1 <- function(vec)
{
  #mean(x,trim = y, na.rm = TRUE/FALSE)
  #If trim is set to 0.3, then after sorting the vector is ascending order 3 values of both ends are removed
  #If there are null values in a vector then mean function would return NA. By default, it is set to FALSE
  #if na.rm = TRUE, R would calulate mean of the vector by removing the NULL values in the vector.
  #If you don't want to proceed with null values in a vector then give na.rm = FALSE so that NA is returned
  avgvec <- mean(vec, na.rm = FALSE)
  print(paste( "mean of the vector: ", avgvec))
  
  #median(x,na.rm=TRUE/FALSE)
  midvec <- median(vec)
  print(paste( "Median of the vector: ", midvec))
  
  #min and max
  minvec <- min(vec, na.rm = FALSE)
  print(paste( "Min Value of the vector: ", midvec))
  
  maxvec <- max(vec)
  print(paste( "Max Value of the vector: ", maxvec))
  
  #Standard deviation
  sdvec <- sd(vec)
  print(paste( "Standard Deviation of the vector: ", sdvec))
  
  #Quantile
  #quantile(vec, probs = c(0.05, 0.95))
  lowquanvec <- quantile(vec, 0.05)
  print(paste( "5% of the vector is: ", lowquanvec))
  
  highquanvec <- quantile(vec, 0.95)
  print(paste( "95% of the vector is: ", highquanvec))
}

vec <- c(1,2,3,4,5,6,6,7,8,9,0)
Myfunc(vec)

#Function to clean data set having information about states
Myfunc2 = function(){
  dfStates <- read.csv("scprc-est2017-18+pop-res.csv", stringsAsFactors = FALSE)
  dfStates <- dfStates[-c(1,nrow(dfStates)),-c(1:4)]
  colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18")
  return(dfStates)
}

states <- Myfunc2()
states
Myfunc1(states$population)

#Read about law of large numbers to understand concept of sampling

#Sampling
samplevec <- sample(states$population, size = 20, replace = TRUE)
samplevec

#See details of sample vector
Myfunc1(samplevec)

#Vector to hold the number of times to repeat the loop
var1 <-2

#initial counter for the loop
cnt <- 1
cnt

#Just a demostration of using repeat loop
repeat{
  samplevec <- sample(states$population, size = 20, replace = TRUE)
  Myfunc1(samplevec)
  cnt <- cnt + 1
  if(cnt>var1){
    break
  }
}

#Just a demostration of using for loop

#Vector to hold the number of times to repeat the loop
cnt1 <- 1:2
cnt1

for(i in cnt1){
  samplevec <- sample(states$population, size = 20, replace = TRUE)
  Myfunc1(samplevec)
}

#Sample1
samplevec1 <- sample(states$population, size = 20, replace = TRUE)
Myfunc1(samplevec1)
hist(samplevec1)

#Sample2
samplevec2 <- sample(states$population, size = 20, replace = TRUE)
Myfunc1(samplevec2)
hist(samplevec2)

#On comparing two histograms of 2 samples resp we cannot come to a conclusion about the distribution of data.
#This explains the law of large numbers and hints to repeat the process of sampling more number of times to
#check if we can establish something substaintial about the data set
#Therefore, let us repeat the sampling process 2000 times to see if we can get to some concrete results

repsam1 <- replicate(2000, sample(states$population, size = 20, replace = TRUE))
Myfunc1(repsam1)
hist(repsam1)

repsam2 <- replicate(2000, sample(states$population, size = 20, replace = TRUE))
Myfunc1(repsam2)
hist(repsam2)

repsam3 <- replicate(2000, sample(states$population, size = 20, replace = TRUE))
Myfunc1(repsam3)
hist(repsam3)

#If we look at histograms of just sampling (without using replicate function) we couldn't see similar results from
#the reults. Two histrigrams looked dissimilar. However, if we compare histograms created after using replicate 
#functions, we can see stablished results and it could be compared to the actual results of the populator vector 
#of states data frame. Thus, it is proven that more repeatation of the sampling process of large data sets, closer 
#we would get to the actual results