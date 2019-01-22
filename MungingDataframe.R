#Munging Dataframes

#setwd() can be used to set working directory. It can also be done through session tab of R studio


dfStates <- read.csv("scprc-est2017-18+pop-res.csv", stringsAsFactors = FALSE)
View(dfStates)
tail(dfStates)
head(dfStates, -5)

#Make sure there are exactly 51 rows (one per state + the district of Columbia).
#Make sure there are precisely 4 columns, with the following names:
#  stateName, population, popOver18, percentOver18. 

dfStates<- dfStates[-1,]
dfStates <- dfStates[-nrow(dfStates),]
View(dfStates)


dfStates <- dfStates[,-1:-4]
View(dfStates)

colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18")

#Do data cleaning as above through a function
myfunc = function(){
  dfStates <- read.csv("scprc-est2017-18+pop-res.csv", stringsAsFactors = FALSE)
  dfStates <- dfStates[-c(1,nrow(dfStates)),-c(1:4)]
  colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18")
}

#function call
myfunc()

#Average Population
avgpop <- mean(dfStates$population)
avgpop

#highest population using which.max
highpop <- dfStates[which.max(dfStates$population),]
highpop

#Use below command to make sure the numbers on axes are not represented in terms of e
options(scipen = 100)

#Historgram
popfreq <- hist(dfStates$population, main = "Histogram of State population", xlab = "population")


#Sort Data frame based on population
sortdf <- dfStates[order(dfStates$population),]

#Create barplot for population of states
?barplot()

statepop <- barplot(sortdf$population, main = "State Population", xlab = "US States", ylab = "population")

statepopover18 <- barplot(sortdf$popOver18, main = "State Population of Age Over 18", xlab = "US States", ylab = "population")

statepercover18 <- barplot(sortdf$population, main = "Population Percentage Over 18", xlab = "US States", ylab = "population")
