#Data Visualizations Continued

#Read in the census dataset
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
View(MergeDf)

#Gather more information of state from other vectors
statedetails <- data.frame(state.name, state.area, state.center)
View(statedetails)
colnames(statedetails) <- c("stateName", "state.area","centerx","centery")

#Merge vector with data frame to analyze more details about the state
compstatedet <- merge(MergeDf, statedetails, by.x = "stateName", by.y = "stateName")
View(compstatedet)

#Task: Generate color coded map, based on the area of the state

#Lower case the names of the states in stateName column as ggmap supports lowercase
compstatedet$stateName <- tolower(compstatedet$stateName) 
View(compstatedet)

#R package gives us the map data of each state
us <- map_data("state")
View(us)

#Create a color coded map highlighting the state area of each state in US
usstatearea <- ggplot(compstatedet,aes(map_id=stateName))  
usstatearea <- usstatearea + geom_map(map=us,aes(fill=compstatedet$state.area)) #fill map according to state area
usstatearea <- usstatearea + expand_limits(x=us$long,y=us$lat) # limit the map based on the coordinates
usstatearea <- usstatearea + coord_map() + ggtitle("Basic Map") #coord_map() is used to project earth on a flat 2D plain
usstatearea

#Create a color coded map highlighting murders in each state of US
usmurderstate <- ggplot(compstatedet, aes(map_id = stateName))
usmurderstate <- usmurderstate + geom_map(map=us, aes(fill=compstatedet$Murder))
usmurderstate <- usmurderstate + expand_limits(x= us$long, y=us$lat)
usmurderstate <- usmurderstate + coord_map() + ggtitle("Murders in Different States")
usmurderstate <- usmurderstate + xlab("Longitude") + ylab("Latitude")
usmurderstate

#Create a color coded map highlight the murders at each state & 
#population of each state with a point proprtional to the size of the population
statepopcenter <- ggplot(compstatedet,aes(map_id=stateName)) # defining the aesthetics 
statepopcenter <- statepopcenter + geom_map(map=us,aes(fill=compstatedet$Murder)) #fill map according to Murder rate
statepopcenter <- statepopcenter + expand_limits(x=us$long,y=us$lat) # limit the map based on the coordinates
statepopcenter <- statepopcenter + geom_point(aes(x=centerx,y=centery,size=compstatedet$population)) #circling the points on the map using the center
statepopcenter <- statepopcenter + coord_map() + ggtitle("Population at Different States") # Give title to the map
statepopcenter