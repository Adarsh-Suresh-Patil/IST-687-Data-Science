#Accident Analysis using JSON

#install.packages("RCurl")   #required to use the function getURL
library(RCurl)

#Load data from a URL
url <- "http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
data <- getURL(url)
View(data)

#install.packages("RJSONIO") 
library("RJSONIO")

#Read R documentation to understand use of simplify and nullValue
result <- fromJSON(data, simplify = FALSE, nullValue = NA)
View(result)
View(result[1]) #contains meta data
View(result[2]) #contains actual data

#Lists is similar to structures in C. It is a collection of different objects.
#source of definition: https://www.statmethods.net/input/datatypes.html
list <- result[[2]]
numRows <- length(list)


#two different functions for the same task
df <- data.frame(matrix(unlist(list), nrow=numRows, byrow=T), stringsAsFactors = FALSE)
View(df)

df1 <- as.data.frame(matrix(unlist(list), nrow=numRows, byrow=T), stringsAsFactors = FALSE)
View(df1)

#Verify df and df1 are same
nrow(df)
ncol(df)

nrow(df1)
ncol(df1)

str(df)
str(df1)

#Clean the data frame
df <- df[,-1:-8]
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
colnames(df) <- namesOfColumns
View(df)

#Gather Business Intelligence on analyzing the data frame

#SQLDF
install.packages("sqldf")
library(sqldf)
#Total number of injuries
bi1 <- sqldf("select count(INJURY) from df where injury = 'YES'")
bi1

#How many accidents happened on Sunday?
SunAcc <- sqldf("select count(case_number) from df where day_of_week like '%SUNDAY%'")
SunAcc

#How many injuries occurred each day of the week?
InjEchWeek <- sqldf("select Day_of_week, count(case_number) from df where injury like '%YES%' group by trim(day_of_week)")
InjEchWeek

#All above business questions could also be found out using another package
install.packages("dplyr")
library("dplyr")

#Total number of Injuries
df1 <- df %>% filter(INJURY == 'YES')
totinj <- summarise(df1,count=n())
totinj

totinj1 <- nrow(df1)
totinj1

#How many accidents happened on Sunday?
df2 <- df %>% filter(trimws(df$DAY_OF_WEEK)=='SUNDAY')
accsun <- summarise(df2,count=n())
accsun

#How many injuries occurred each day of the week?
df.GroupBydays <- group_by(df2, trimws(df2$DAY_OF_WEEK))
View(df.GroupBydays)

InjEchWeek2 <- count(filter(df.GroupBydays,INJURY=="YES"))
InjEchWeek2

#What is the distribution of the number of vehicles in accidents on Friday?
# (use a histogram and quantile)
install.packages("tidyr")
library(tidyr)
df3 <- filter(df, trimws(df$DAY_OF_WEEK) == 'FRIDAY')
View(df3)
NumVehFrivec <- replace_na(df3$VEHICLE_COUNT,0)
View(NumVehFrivec)
NumVehFri<- strtoi(NumVehFrivec)
NumVehFri
hist(NumVehFri) 
quantile(NumVehFri,prob=c(0.25,0.5,0.75))
summary(NumVehFri)
#Observation: The number of vehicles are very less. Therefore, chances of any collisions or accidents 
#happening on friday is rare.

# 12.	How does this distribution compare with the distribution of the number of vehicles in accidents on Sunday?  
#(use a histogram and quantile)
df4 <- filter(df, trimws(df$DAY_OF_WEEK) == "SUNDAY")
View(df4)
NumVehSunvec<- replace_na(df4$VEHICLE_COUNT,0) 

cnvnum <- as.numeric(NumVehSunvec)
summary(cnvnum)
?as.numeric

NumVehSun<- strtoi(NumVehSunvec) #histogram function needs x to be numeric so convert vector to numeric type
summary(NumVehSun)
?strtoi

hist(NumVehSun) 
quantile(NumVehSun,prob=c(0.25,0.5,0.75))

#Observation: Histogram shows that number of vehicles on sunday is very less. Hence, it implies that chances of 
#an accident is very low

#Compare friday and sunday results obtained
#> summary(NumVehSun)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   1.000   1.492   2.000   8.000 
#> summary(NumVehFri)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   2.000   1.777   2.000   7.000 

