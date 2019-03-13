#Data Visualization Using R

install.packages("ggplot2")
library("ggplot2")

#Read a data set
#1)	Read in the census dataset
myfunc = function(){
  dfStates <- read.csv("scprc-est2017-18+pop-res.csv", stringsAsFactors = FALSE)
  dfStates <- dfStates[-c(1,nrow(dfStates)),-c(1:4)]
  colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18")
  return(dfStates)
}

States <- myfunc()
View(States)

#Initialize a data frame
arrests <- USArrests
View(arrests)

#Have state names as an attribute of the data frame so that it can be merged with other details of the state
rownames(arrests)
arrests$Name <- rownames(arrests)
View(arrests)

rownames(arrests) <- NULL

#Merge two data sets based on a matching column
MergeDf <- merge(States,arrests, by.x = "stateName", by.y = "Name" ) 
#if by.x & by.y is not mentioned then R will try to match the columns of two dataframes having same column name(case sensitive)

View(MergeDf)

#Plot histogram of population using ggplot
pophist <- ggplot(mergedf, aes(x=population)) + geom_histogram(binwidth = 500000)
#binwidth is number of bins we want to have in X axis
#geom_histogram plots histogram
pophist

#Plot histogram of murder rate using ggplot
murderhist <- ggplot(mergedf, aes(x=Murder)) + geom_histogram()
murderhist

#Plot boxplot of population using ggplot
popbox <- ggplot(mergedf, aes(y=population)) + geom_boxplot() +ggtitle("Population of Different States")
#ggtitle is used to give a heading to the plot
#geom_boxplot() plots box plot
popbox

#Plot boxplot of murder rate using ggplot
murderbox <- ggplot(mergedf, aes(y=Murder)) + geom_boxplot() + ggtitle("Murder rate in Different States")
murderbox

#We can see that histogram gives better conclusive picture about population and murder rate in different states
#of United States. Therefore, we can draw a conclusion from above four visualizations that choosing a correct 
#visualization is extremely important to understand the stats that we are trying to analyze.

#Number of Murders per states
MergeDf$NoOfMurders <- (MergeDf$population * MergeDf$Murder)/100000
View(MergeDf)

murderstatebar <- ggplot(MergeDf, aes(x=stateName, y=NoOfMurders)) + geom_col() + ggtitle("Total Murders in Different States")
#geom_col is used to generate a bar plot
murderstatebar
#we cannot read the state names in the Box Plot

murderstatebar1 <- murderstatebar + theme(axis.text.x = element_text(angle = , hjust = 1))
#theme command is used here to rotate the text at the x axis. It has more functions which can be explored further
murderstatebar1
#Now we can clearly see the names of the states

#Sort X axis by Murders
murderstatebar2 <- ggplot(MergeDf, aes(x=reorder(stateName,NoOfMurders), y=NoOfMurders)) 
#by default the reorder functions sorts in ascending order.
murderstatebar2 <- murderstatebar2 + geom_col() + ggtitle("Total Murders in Different States")
murderstatebar2 <- murderstatebar2 + xlab("States in US") + ylab("Number of Murders")
#Xlab() and Ylab() helps to label the two axis with appropriate names
murderstatebar2 <- murderstatebar2 + theme(axis.text.x = element_text(angle = 270, hjust = 1))
murderstatebar2

#Scatter Plot to analyze murders of adults
murderscat <- ggplot(MergeDf, aes(x=population, y = percentOver18)) + ggtitle("Murders of Adults")
murderscat <- murderscat + geom_point(aes(size= Murder, color = Murder)) 
#geom_point is used to plot scatter plot
murderscat <- murderscat + xlab("Population of States") + ylab("Percent of Adults")
murderscat
