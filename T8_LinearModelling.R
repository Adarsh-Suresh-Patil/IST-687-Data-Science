#Linear Modelling

install.packages("RJSONIO")
library(RJSONIO)

install.packages("ggplot2")
library(ggplot2)


#1. Set working directory where json file is placed
setwd("/Users/Shared/Personal/Syracuse/Data Science/HW8")

#load the json dataset 
dataset <- "hotelSurveySherison.json"
hoteldata <- fromJSON(dataset, simplify = TRUE, nullValue = NA)
hoteldatadf <- data.frame(hoteldata)

#2.	Use the str command to make sure you can see the following attributes
str(hoteldatadf)

#Part B Explore the data  
#1.	Create bivariate plots for each of the attributes.

#GGplot using hotelsize to see how overallcustsat changes
hotelplot1 <- ggplot(hoteldatadf, aes(y=overallCustSat, x=hotelSize)) + geom_point()
hotelplot1
hoteldatadf$overallCustSatJ <- jitter (hoteldatadf$overallCustSat)
hoteldatadf$hotelSizeJ <- jitter(hoteldatadf$hotelSize)
rehotelplot1 <- ggplot(hoteldatadf, aes (x=hotelSizeJ, y=overallCustSatJ)) + geom_point()
rehotelplot1

#GGplot using checkInSat to see how overallcustsat changes
hotelplot2 <- ggplot(hoteldatadf,aes(x=checkInSat,y=overallCustSat)) + geom_point()
hotelplot2
hoteldatadf$checkInSatJ <- jitter(hoteldatadf$checkInSat)
rehotelplot2 <- ggplot(hoteldatadf,aes(x=checkInSatJ,y=overallCustSatJ)) + geom_point()
rehotelplot2

#GGplot using hotelClean to see how overallcustsat changes
hotelplot3 <- ggplot(hoteldatadf,aes(x=hotelClean,y=overallCustSat)) + geom_point()
hotelplot3
hoteldatadf$hotelCleanJ <- jitter(hoteldatadf$hotelClean)
rehotelplot3 <- ggplot(hoteldatadf,aes(x=hotelCleanJ,y=overallCustSatJ)) + geom_point()
rehotelplot3

#GGplot using hotelFriendly to see how overallcustsat varies
hotelplot4 <- ggplot(hoteldatadf,aes(x=hotelFriendly,y=overallCustSat)) + geom_point()
hotelplot4
hoteldatadf$hotelFriendlyJ <- jitter(hoteldatadf$hotelFriendly)
rehotelplot4 <- ggplot(hoteldatadf,aes(x=hotelFriendlyJ,y=overallCustSatJ)) + geom_point()
rehotelplot4

#GGplot using guestAge to see how overallcustsat varies
hotelplot5 <- ggplot(hoteldatadf,aes(x=guestAge,y=overallCustSat)) + geom_point()
hotelplot5
hoteldatadf$guestAgeJ <- jitter(hoteldatadf$guestAge)
rehotelplot5 <- ggplot(hoteldatadf,aes(x=guestAgeJ,y=overallCustSatJ)) + geom_point()
rehotelplot5

#GGplot using lengthofstay to see how overallcustsat varies
hotelplot6 <- ggplot(hoteldatadf,aes(x=lengthOfStay,y=overallCustSat)) + geom_point()
hotelplot6
hoteldatadf$lengthOfStayJ <- jitter(hoteldatadf$lengthOfStay)
rehotelplot6 <- ggplot(hoteldatadf,aes(x=lengthOfStayJ,y=overallCustSatJ)) + geom_point()
rehotelplot6

#GGplot using whenBookedTrip to see how overallcustsat varies
hotelplot7 <- ggplot(hoteldatadf,aes(x=whenBookedTrip,y=overallCustSat)) + geom_point()
hotelplot7
hoteldatadf$whenBookedTripJ <- jitter(hoteldatadf$whenBookedTrip)
rehotelplot7 <- ggplot(hoteldatadf,aes(x=whenBookedTripJ,y=overallCustSatJ)) + geom_point()
rehotelplot7

#GGplot using hotelState to see how overallcustsat varies
hotelplot8 <- ggplot(hoteldatadf,aes(x=hotelState,y=overallCustSat)) + geom_point()
hotelplot8 <- hotelplot8+ theme(axis.text.x=element_text(angle = 90, hjust = 1))
hotelplot8

#GGplot using gender to see how overallcustsat varies
hotelplot9 <- ggplot(hoteldatadf,aes(x=gender,y=overallCustSat)) + geom_point()
hotelplot9

#2.	What do you observe from the plots? Note via a block comment.
#1. When multiple points are plotted at the same coordinates it doesn't help us know where the majority of points are there. 
#Jitter function adds noise to each point which helps us know where the cluster of points are
#2. From above plots it is difficult to determine which are the main factors affecting the overall customer
#satisfaction.

#Part B: Generate a linear model  
#1.	Next, create one regression model predicting the overall customer satisfaction from the other variables (but not the freeText response). 
#Make sure to include all predictors in one model
#remove freeText and jitter
View(hoteldatadf)
hoteldatadf <- hoteldatadf[,-11:-20] #removing free text and jitter columns added for previous steps
str(hoteldatadf)

m1 <- lm(formula = overallCustSat ~ ., data = hoteldatadf)
summary(m1)

#Multiple R-squared:  0.6702,	Adjusted R-squared:  0.6682 
#The statistically significant predictors are checkInSat, hotelClean, hotelFriendly, guestAge,lengthOfStay, whenBookedTrip
# checkInSat                     -2.381e-01  5.544e-03 -42.940  < 2e-16 ***
# hotelClean                      4.042e-02  6.941e-03   5.824 5.93e-09 ***
# hotelFriendly                   1.122e+00  8.863e-03 126.557  < 2e-16 ***
# guestAge                       -1.205e-01  1.815e-03 -66.400  < 2e-16 ***
# lengthOfStay                   -3.284e-01  1.677e-02 -19.575  < 2e-16 ***
# whenBookedTrip                  6.421e-03  1.005e-03   6.387 1.77e-10 ***


#In first step it was clear that we cannot judge overall customer satisfaction on single variables.
#By linear modeling, we obtained the Rsquare value and astericks which helped us to determing which factor is
#more important to determine overall customer satisfaction. 
#checkinSat, hotelClean and hotelFriendly are the three independent variables which are more significant to determine
#overall customer satisfaction(dependent variable).


#6.Create a different regression model predicting the overall customer satisfaction from the one variable you think is best. 

m2 <- lm(formula = overallCustSat ~ hotelFriendly, data = hoteldatadf)
summary(m2)

m2plot <- plot(hoteldatadf$hotelFriendly, hoteldatadf$overallCustSat)
abline(m2) 

m3 <- lm(formula = overallCustSat ~ guestAge, data = hoteldatadf)
summary(m3)

m3plot <- plot(hoteldatadf$guestAge, hoteldatadf$overallCustSat)
abline(m3) 

#Then create another LM using two variables.
m4 <- lm(formula = overallCustSat ~ hotelFriendly+ guestAge, data = hoteldatadf)
summary(m4)

#7.	Write a block comment comparing the two lm models.

#The best independent variable used for prediction of the overall customer satisfaction is the hotelFriendly variable.
#Since we get the best R-squared value and the#lowest residual error on trying all the combinations of significant predictors, 
#using hotelFriendly variable is the best independent variable.
#