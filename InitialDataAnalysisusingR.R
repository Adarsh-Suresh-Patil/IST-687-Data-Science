#Initial Data Analysis using R

#Initialize a data frame
arrests <- USArrests
View(arrests)

#to find description of USArrests
?USArrests

#Find states having best and poor assualt rate
#state with least assualt = best state
beststate <- rownames(arrests)[arrests$Assault == min(arrests$Assault)]
beststate
#state with max assualt = poor state
poorstate <- rownames(arrests)[arrests$Assault == max(arrests$Assault)]
poorstate

#Sort Data Frame on descending murder rate
?sort
descmurderrate <- sort(arrests$Murder, na.last = TRUE, decreasing = TRUE)
descmurderrate
sortdf <- arrests[order(-arrests$Murder),]
View(sortdf)

#show top 10 states with highest murder rate
head(sortdf, 10)

#What is the value of the 20'th row, third column (in the sorted dataframe)? 
#Use R code (not visual inspection)
sortdf[20,3]

#Find least safe state
#Step1: Normalize the data set using the formula:
# (x-mean(x))/sd(x)
scaled.df <- scale(arrests, scale = TRUE)
View(scaled.df)
#Step2:Add row values
#one way
a <- rowSums(scaled.df, na.rm = FALSE, dims = 1)
View(a)
#output of rowsums is a list

#second way
b <- transform(scaled.df, crimerate = rowSums(scaled.df))
View(b)
#b is a dataframe

#Step3: Least safe state would be with least crime rate
safeststate <- rownames(b)[b$crimerate == min(b$crimerate)]
safeststate
