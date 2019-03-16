#Association Rules -- Apriori

install.packages("RJSONIO")
library(RJSONIO)

#Part A: Explore Data Set
#1)	Load the dataset: hotelSurveyBarriot.json 
#1. Set working directory where json file is placed
setwd("/Users/Shared/Personal/Syracuse/Data Science/HW9")
#JSON data set is loaded from the local directory
dataset <- "hotelSurveyBarriot.json"
hoteldata <- fromJSON(dataset, simplify = TRUE, nullValue = NA)
hotelSurvey <- data.frame(hoteldata)


#2)	Name the dataframe hotelSurvey
#Part B: Explore Data Set
#1)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command
View(hotelSurvey)
str(hotelSurvey)
summary(hotelSurvey)

#2)	Map each numeric attribute to a category  - Since we want to create rules, 
#we should convert the attributes that have a numeric range into buckets (ex. low or high)

#As per my observation by looking at the range of each attribute of the dataset 
#overallCustSat,checkInSat, hotelClean, hotelFriendly can be categorised using below function
BucketCat <- function(vec){
  vBuckets <- replicate(length(vec),"Average") #converting every value to "Average"
  vBuckets[vec > 7] <- "High"
  vBuckets[vec < 7] <- "Low"
  return(vBuckets)
}

#Used above function categorize values in each attribute
CustSat <- BucketCat(hotelSurvey$overallCustSat)
CheckinSat <- BucketCat(hotelSurvey$checkInSat)
hotelFriendly <- BucketCat(hotelSurvey$hotelFriendly)
hotelClean <- BucketCat(hotelSurvey$hotelClean)


#For the attributes that could be categorized using above function, 
#it could be cateogorized using quartile function
quantBuckets <- function(v)
{
  q <- quantile(v, c(0.4, 0.6))
  vBuckets <- replicate(length(v), "Average")
  vBuckets[v <= q[1] ]<- "Low"
  vBuckets[v > q[2]] <- "High"
  return(vBuckets)
}

#Use above function to categorize the attributes
hotelSize <- quantBuckets(hotelSurvey$hotelSize)
Age <- quantBuckets(hotelSurvey$guestAge)
lengthOfStay <- quantBuckets(hotelSurvey$lengthOfStay)
whenBookedTrip <- quantBuckets(hotelSurvey$whenBookedTrip)

#3)	Count the people in each category of for the age and friendliness attributes

#Count number of observations for each unique value of the attribute
table(Age)
table(hotelFriendly)

#Count number of observations considering two attributes 
newdf <- table(Age,hotelFriendly)

#4)	Express the results of problem 3 as percentages by sending the results of the table( ) command 
#into the prop.table( ) command
prop.table(newdf)*100 # multiply by 100 in order to get percentage

#5)	Show a "contingency table" of percentages for the age and the overall satisfaction variables together. 
#Write a block comment about what you see.
newdf2<- table(Age,CustSat)
prop.table(newdf2)*100 # multiply by 100 in order to get percentage

#Part C: Coerce the data frame into transactions
#6)	Install and library two packages: arules and arulesViz.
install.packages('arules')
library(arules)

install.packages('magrittr')
library(magrittr)

#7)	Coerce the hotelSurvey data frame into a sparse transactions matrix using:
#hotelSurveyX <- as(hotelSurvey,"transactions")
#Create new data frame with the vectors created using bucket functions
rule1df <- data.frame(CustSat,CheckinSat,hotelClean,hotelFriendly,lengthOfStay,hotelSize,Age,whenBookedTrip)

#8)	Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands to explore the contents of hotelSurveyX.
hotelSurveyX <- as(rule1df,"transactions")

hotelSurveyX

# Looking at the sparse matrix using methods in the arules package
inspect(hotelSurveyX)
itemFrequency(hotelSurveyX)
itemFrequencyPlot(hotelSurveyX)

#Part C: Use arules to discover patterns
#Support is the proportion of times that a particular set of items occurs 
#relative to the whole dataset. Confidence is proportion of times that the 
#consequent occurs when the antecedent is present.
#
#9)	Run the apriori command to try and predict happy customers (as defined by their overall satisfaction being high - above 7).

#1st iteration
ruleset <- apriori(hotelSurveyX,parameter = list(support=0.1,confidence=0.5),appearance = list(default="lhs",rhs=("CustSat=High")))
inspect(ruleset)
#This generates 73 rules with RHS being our dependent attribute "CustSat". 
#The goal is to pick out maximum lift.
#lhs                                                                                rhs            support confidence lift     count
#{CheckinSat=High, hotelClean=High,hotelFriendly=Average,whenBookedTrip=High}   => {CustSat=High}  0.1083  0.9926673 2.089386  1083

#This rule states that customers are most happy when the hotels are highly clean, Average friendly and freqeuntly booked

#2nd iteration
#increasing the support to reduce the number of rules
ruleset2 <- apriori(hotelSurveyX,parameter = list(support=0.2,confidence=0.5),appearance = list(default="lhs",rhs=("CustSat=High")))
inspect(ruleset2)

#10)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset. 
#lhs                                                            rhs           support confidence lift     count
#{CheckinSat=High,hotelClean=High,whenBookedTrip=High}      => {CustSat=High} 0.2186  0.9014433  1.897376 2186
#Observation1: the number of rules have reduced to 25, 
#Observation2: Maximum lift is 1.89 

#11)	 If you had to provide two rules to the hotel owner (in terms of what helps drive high overall customer satisfaction, what would those two rules be?
#Below are the two rules that drive the overall customer satisfaction:-
#lhs                                                                            rhs                support confidence lift     count
#{CheckinSat=High,hotelClean=High, hotelFriendly=Average, whenBookedTrip=High}   => {CustSat=High} 0.1083  0.9926673  2.089386  1083
#{hotelClean=High,hotelFriendly=Average,whenBookedTrip=High}                     => {CustSat=High} 0.1141  0.9904514  2.084722  1141 


#Observation: customers are highly satisfied with the hotel when checkinSat is high, friendly quotient is average, cleanliness is high
#and when most frequently booked trip is high