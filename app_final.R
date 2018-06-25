#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if(!require(xlsx)) { install.packages("xlsx"); library(xlsx); }
if(!require(genalg)) { install.packages("genalg"); library(genalg); }

if(!require(leaflet)) { install.packages("leaflet"); library(leaflet); }
if(!require(shinydashboard)) { install.packages("shinydashboard"); library(shinydashboard); }
if(!require(rpgm)) { install.packages("rpgm"); library(rpgm); }
if(!require(lpSolve)) { install.packages("lpSolve"); library(lpSolve); }
if(!require(plotly)) { install.packages("plotly"); library(plotly); }

setwd("")
wd <- getwd()

load('cngStations_BE.Rdata')
load('lngStations_BE.Rdata')
load('petrolStations_BE.Rdata')
load('Roads.Rdata')

roads <- BELGIUM
rm(BELGIUM)

terminals <- read.xlsx('Location_terminals.xlsx', 1, colIndex=seq(2,4))

lngStations$name_gasstation <- as.character(lngStations$name_gasstation)
rownames(lngStations)[2] <- 2

####################################################
## Calculate traffic for each station
####################################################

#Calculating cars traffic
cars <- numeric(nrow(roads))
for (i in 1:nrow(roads)) {
  cars[i] <- roads[i,3] - roads[i,2]
}
roads$cars_daily_avg <- cars
rm(cars)

#Dividing in squares 100x100
latMin <- min(petrolStations$lat)
latMax <- max(petrolStations$lat)
lonMin <- min(petrolStations$lon)
lonMax <- max(petrolStations$lon)

latDiff <- latMax - latMin
lonDiff <- lonMax - lonMin

maxDiff <- max(lonDiff, latDiff)

div <- 100

steps <- maxDiff / div

#Initializing squares matrix
squares <- as.data.frame(matrix(NA, nrow=div^2, ncol=7))
colnames(squares) <- c("latmin", "latmax", "lonmin", "lonmax", "avgcars", "avgtrucks", "avgtotal")

row <- 1

for (i in 1:div) {
  for (j in 1:div) {
    squares[row,]$latmin <- latMin
    squares[row,]$latmax <- latMin + steps
    squares[row,]$lonmin <- lonMin
    squares[row,]$lonmax <- lonMin + steps
    row <- row + 1
    lonMin <- lonMin + steps
  }
  lonMin <- min(petrolStations$lon)
  latMin <- latMin + steps
}

#Assigning traffic to squares
cars <- numeric(nrow(squares))
trucks <- numeric(nrow(squares))
total <- numeric(nrow(squares))

for (i in 1:nrow(squares)) {
  cars[i] <- mean(roads[(squares[i,1] <= roads[5] & roads[5] < squares[i,2]) & (squares[i,3] <= roads[4] & roads[4] < squares[i,4]),6])
  trucks[i] <- mean(roads[(squares[i,1] <= roads[5] & roads[5] < squares[i,2]) & (squares[i,3] <= roads[4] & roads[4] < squares[i,4]),2])  
  total[i] <- mean(roads[(squares[i,1] <= roads[5] & roads[5] < squares[i,2]) & (squares[i,3] <= roads[4] & roads[4] < squares[i,4]),3])
}
squares$avgcars <- cars
squares$avgtrucks <- trucks
squares$avgtotal <- total

rm(cars)
rm(trucks)
rm(total)

#Assigning traffic to stations
cars <- numeric(nrow(petrolStations))
trucks <- numeric(nrow(petrolStations))
total <- numeric(nrow(petrolStations))

for (i in 1:nrow(petrolStations)) {
  c <- squares[(squares[1] <= petrolStations[i,2] & petrolStations[i,2] < squares[2]) & (squares[3] <= petrolStations[i,3] & petrolStations[i,3] < squares[4]),5]
  tr <- squares[(squares[1] <= petrolStations[i,2] & petrolStations[i,2] < squares[2]) & (squares[3] <= petrolStations[i,3] & petrolStations[i,3] < squares[4]),6]
  tot <- squares[(squares[1] <= petrolStations[i,2] & petrolStations[i,2] < squares[2]) & (squares[3] <= petrolStations[i,3] & petrolStations[i,3] < squares[4]),7]
 
  if(!is.na(c) && length(c) > 0) cars[i] <- c
  else cars[i] <- 0
   
  if(!is.na(tr) && length(tr) > 0) trucks[i] <- tr
  else trucks[i] <- 0
  
  if(!is.na(tot) &&  length(tot) > 0) total[i] <- tot
  else total[i] <- 0
}

petrolStations$cars <- cars
petrolStations$trucks <- trucks
petrolStations$total <- total

rm(cars)
rm(trucks)
rm(total)

cars <- numeric(nrow(cngStations))
trucks <- numeric(nrow(cngStations))
total <- numeric(nrow(cngStations))

for (i in 1:nrow(cngStations)) {
  c <- squares[(squares[1] <= cngStations[i,5] & cngStations[i,5] < squares[2]) & (squares[3] <= cngStations[i,6] & cngStations[i,6] < squares[4]),5]
  tr <- squares[(squares[1] <= cngStations[i,5] & cngStations[i,5] < squares[2]) & (squares[3] <= cngStations[i,6] & cngStations[i,6] < squares[4]),6]
  tot <- squares[(squares[1] <= cngStations[i,5] & cngStations[i,5] < squares[2]) & (squares[3] <= cngStations[i,6] & cngStations[i,6] < squares[4]),7]
  
  if(!is.na(c) && length(c) > 0) cars[i] <- c
  else cars[i] <- 0
  
  if(!is.na(tr) && length(tr) > 0) trucks[i] <- tr
  else trucks[i] <- 0
  
  if(!is.na(tot) &&  length(tot) > 0) total[i] <- tot
  else total[i] <- 0
}

cngStations$cars <- cars
cngStations$trucks <- trucks
cngStations$total <- total

rm(cars)
rm(trucks)
rm(total)

cars <- numeric(nrow(lngStations))
trucks <- numeric(nrow(lngStations))
total <- numeric(nrow(lngStations))

for (i in 1:nrow(lngStations)) {
  c <- squares[(squares[1] <= lngStations[i,5] & lngStations[i,5] < squares[2]) & (squares[3] <= lngStations[i,6] & lngStations[i,6] < squares[4]),5]
  tr <- squares[(squares[1] <= lngStations[i,5] & lngStations[i,5] < squares[2]) & (squares[3] <= lngStations[i,6] & lngStations[i,6] < squares[4]),6]
  tot <- squares[(squares[1] <= lngStations[i,5] & lngStations[i,5] < squares[2]) & (squares[3] <= lngStations[i,6] & lngStations[i,6] < squares[4]),7]
  
  if(!is.na(c) && length(c) > 0) cars[i] <- c
  else cars[i] <- 0
  
  if(!is.na(tr) && length(tr) > 0) trucks[i] <- tr
  else trucks[i] <- 0
  
  if(!is.na(tot) &&  length(tot) > 0) total[i] <- tot
  else total[i] <- 0
}

lngStations$cars <- cars
lngStations$trucks <- trucks
lngStations$total <- total

rm(cars)
rm(trucks)
rm(total)

#Cleanup, renaming
cngStations$name_gasstation <- as.character(cngStations$name_gasstation)
cngStations[grepl('dats', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Dats24-BE"
cngStations[grepl('devagro', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Devagro"
cngStations[grepl('enora', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Enora"
cngStations[grepl('gps', tolower(cngStations$name_gasstation)),]$name_gasstation <- "GPS"
cngStations[grepl('maes', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Maes-BE"
cngStations[grepl('q8', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Q8-BE"
cngStations[grepl('eke-nazareth', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Other"
cngStations[grepl('electrabel mechelen', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Other"
cngStations[grepl('elva aalst', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Other"
cngStations[grepl('imog harelbege', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Other"
cngStations[grepl('zellik', tolower(cngStations$name_gasstation)),]$name_gasstation <- "Other"

#Making dataframe of all available stations
allAvailable <- petrolStations
for (i in 1:nrow(cngStations)) {
  if(cngStations[i,]$fueltype == 'CNG') allAvailable[nrow(allAvailable)+1,] <- list(cngStations[i,1], cngStations[i,5], cngStations[i,6], levels(cngStations[,4])[cngStations[i,4]], cngStations[i,7], cngStations[i,8], cngStations[i,9])
}

#Renaming
allAvailable$name_gasstation <- gsub("-BE", "", allAvailable$name_gasstation)

####################################################
## Initializing values for data exploration tab
####################################################

r_min <- min(roads$total_daily_avg)
r_max <- max(roads$total_daily_avg)

def_1 <- 20000
def_2 <- 80000

p_min <- min(petrolStations$tot)
p_max <- max(petrolStations$tot)

pdef_1 <- 20000
pdef_2 <- 80000

hidelow <- FALSE

roads$ids <- as.character(rownames(roads))

petrolStations$ids <- as.character(as.integer(rownames(petrolStations))+10000)

lngStations$ids <- as.character(as.integer(rownames(lngStations))+20000)

####################################################
## Functions needed to optimize
####################################################

#Calculate Gaussian distribution (Formula 3 paper)
calcGauss <- function(i, x, y, lng, stdev) {
  gs <- (1/sqrt(2*pi*stdev))*exp(-((((x - lng[i,]$lon)^2) + ((y - lng[i,]$lat)^2))/(2*(stdev^2))))
  return(gs)
}

#Calculate local access (Formula 1 paper)
calcLA <- function(x, y, traffic, lng, stdev) {
  templng <- lng[5:6]
  n <- 0
  d <- 0
  for (i in 1:nrow(templng)) {
    n <- n + (calcGauss(i, x, y, templng, stdev) * traffic * calcGauss(i, x, y, templng, stdev))
    d <- d + calcGauss(i, x, y, templng, stdev)
  }
  res <- n/d
  return(res)
}

#Calculate traffic 'influence' of nearby stations
calcInf <- function(i, x, y, lng, stdev) {
  gs <- (1)*exp(-((((x - lng[i,]$lon)^2) + ((y - lng[i,]$lat)^2))/(2*(stdev^2))))
  return(gs)
}

#Calculate total influence
calcTotalInfluence <- function(x, y, lng, stdev) {
  res <- 0
  for (i in 1:nrow(lng)) {
    res <- res + calcInf(i, x, y, lng, stdev)
  }
  return(res)
}

#Calculate distance between two coordinates
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

####################################################
## Picking LNG stations (optimizing)
####################################################

#Variables
#avSt: dataframe with all available stations
#lngSt: dataframe with current LNG stations
#number: number of new LNG stations to pick
#km: area of influence (to calculate standard deviation)
#choice: traffic to base optimization on (cars=1, trucks=2, total=3)
#maxdis: maximum distance to terminal
#termcons: maximum number of stations that can be connected to a terminal
#acc: accuracy value (low=50, medium=200, high=500)
pickLNGStations <- function(avSt, lngSt, number, km, choice, maxdis, termcons, acc) {
  
  opt <- 0
  stdev <- km/110
  
  #Evaluation function for GA
  eval <- function(starting) {
    #Initializing
    starting <- as.integer(starting)
    templng <- avSt[starting,]
    lng <- lngSt
    
    serv <- levels(lng$status_gasstation)[1]
    ft <- levels(lng$fueltype)[2]
    lng$status_gasstation <- as.character(lng$status_gasstation)
    lng$fueltype <- as.character(lng$fueltype)
    for (i in 1:nrow(templng)) {
      lng[nrow(lng)+1,] <- list(templng[i,]$name_gasstation, serv, levels(lng$country)[1], ft, templng[i,]$lat, templng[i,]$lon, templng[i,]$cars, templng[i,]$trucks, templng[i,]$total, as.character(nrow(lng)+1+20000))
    }
    
    #Initializing distance matrices
    distancesToTherminalKM2 <- data.frame(matrix(NA,nrow=nrow(lng),ncol = 6))
    distanceDF <- data.frame(matrix(NA,nrow=nrow(lng),ncol = 3))
    colnames(distanceDF) <- c('terminalName', 'distanceToterminal', 'acc')
    
    lng$acc <- NA
    totinf <- 0
    la <- 0
    opt <- 0
    
    #Loop through all LNG stations
    for (i in 1:nrow(lng)) {
      #Calculate influence of all stations on current station
      inf <- calcTotalInfluence(lng[i,]$lon, lng[i,]$lat, lng[-i,], stdev)
      acc <- 0
      
      if(choice == 1) {
        #Calculate new available traffic based on total influence
        lng[i,]$cars <- (1-inf)*lng[i,]$cars
        
        #Calculate local access of lng station (based on new traffic)
        acc <- calcLA(lng[i,]$lon, lng[i,]$lat, lng[i,]$cars, lng, stdev)       
      }
      else if(choice == 2) {
        #Calculate new available traffic based on total influence
        lng[i,]$trucks <- (1-inf)*lng[i,]$trucks
        
        #Calculate local access of lng station (based on new traffic)
        acc <- calcLA(lng[i,]$lon, lng[i,]$lat, lng[i,]$trucks, lng, stdev)
      }
      else {
        #Calculate new available traffic based on total influence
        lng[i,]$total <- (1-inf)*lng[i,]$total
        
        #Calculate local access of lng station (based on new traffic)
        acc <- calcLA(lng[i,]$lon, lng[i,]$lat, lng[i,]$total, lng, stdev)
      }
      
      lng[i,]$acc <- acc
    }
    
    #Calculate distance from each station to each terminal
    for (j in 1:nrow(terminals)) {
      distancesToTherminalKM2[,j] <- as.data.frame( earth.dist(lng$lon, lng$lat,  terminals[j,"lon"], terminals[j,"lat"]) )
      names(distancesToTherminalKM2)[j] <- paste0('distanceToTerminal_',j)
    }
    dis <- distancesToTherminalKM2
    
    #Create dataframe for assignment problem
    for(k in 1:termcons) {
      dis[,(1+(6*(k-1))):(6+(6*(k-1)))] <- distancesToTherminalKM2[,1:6]
    }
    
    if(nrow(dis) < ncol(dis)) {
      dis[(nrow(lng)+1):ncol(dis),] <- 1000000 
    }
    
    #LP assignment problem: minimize distances based on given constraints
    solution <- lp.assign(as.matrix(dis), direction='min')$solution
    
    #Fetch assignments
    assignments <- data.frame(matrix(0,nrow=nrow(lng),ncol = 6))
    for (l in 1:nrow(lng)) {
      r <- which(solution[l,] > 0)
      while(r > 6) {
        r <- r - 6
      }
      assignments[l,r] <- 1
    }
    
    #Store distances and assignments
    distancesToTherminalKM2 <- distancesToTherminalKM2 * assignments
    distanceDF$terminalName <- colnames(distancesToTherminalKM2)[apply(distancesToTherminalKM2,1,which.max)]
    distanceDF$distanceToterminal <- rowMaxs(distancesToTherminalKM2 )
    distanceDF$acc <- as.numeric(lng[,]$acc)
    
    #Distance penalty
    distanceDF$acc <- (1-((distanceDF$distanceToterminal/maxdis)*0.5))*distanceDF$acc
    
    #Distance constaint (if higher than maximum distance)
    distanceDF$acc[ distanceDF$distanceToterminal > maxdis] <- -1000000

    #Sum all optimization values
    opt <- sum(distanceDF$acc)
    
    return(-opt)
  }
  
  #GA optimization model (minimization problem)
  model <- rbga(stringMin = rep(1, number), 
                stringMax = rep(nrow(avSt), number), 
                popSize = acc, iters = 10, 
                mutationChance = 0.01, evalFunc = eval, 
                verbose = FALSE)
  
  #Fetch best values
  newStations <- as.integer(model$population[which.min(model$evaluations),])
  
  #Repeat optimization code to calculate values
  
  #Initializing
  starting <- as.integer(newStations)
  templng <- avSt[starting,]
  lng <- lngSt
  
  serv <- levels(lng$status_gasstation)[1]
  ft <- levels(lng$fueltype)[2]
  lng$status_gasstation <- as.character(lng$status_gasstation)
  lng$fueltype <- as.character(lng$fueltype)
  for (i in 1:nrow(templng)) {
    lng[nrow(lng)+1,] <- list(templng[i,]$name_gasstation, serv, levels(lng$country)[1], ft, templng[i,]$lat, templng[i,]$lon, templng[i,]$cars, templng[i,]$trucks, templng[i,]$total, as.character(nrow(lng)+1+20000))
  }
  
  #Initializing distance matrices
  distancesToTherminalKM2 <- data.frame(matrix(NA,nrow=nrow(lng),ncol = 6))
  distanceDF <- data.frame(matrix(NA,nrow=nrow(lng),ncol = 3))
  colnames(distanceDF) <- c('terminalName', 'distanceToterminal', 'acc')
  
  lng$acc <- NA
  totinf <- 0
  la <- 0
  opt <- 0
  
  #Loop through all LNG stations
  for (i in 1:nrow(lng)) {
    #Calculate influence of all stations on current station
    inf <- calcTotalInfluence(lng[i,]$lon, lng[i,]$lat, lng[-i,], stdev)
    acc <- 0
    
    if(choice == 1) {
      #Calculate new available traffic based on total influence
      lng[i,]$cars <- (1-inf)*lng[i,]$cars
      
      #Calculate local access of lng station (based on new traffic)
      acc <- calcLA(lng[i,]$lon, lng[i,]$lat, lng[i,]$cars, lng, stdev)       
    }
    else if(choice == 2) {
      #Calculate new available traffic based on total influence
      lng[i,]$trucks <- (1-inf)*lng[i,]$trucks
      
      #Calculate local access of lng station (based on new traffic)
      acc <- calcLA(lng[i,]$lon, lng[i,]$lat, lng[i,]$trucks, lng, stdev)
    }
    else {
      #Calculate new available traffic based on total influence
      lng[i,]$total <- (1-inf)*lng[i,]$total
      
      #Calculate local access of lng station (based on new traffic)
      acc <- calcLA(lng[i,]$lon, lng[i,]$lat, lng[i,]$total, lng, stdev)
    }
    
    lng[i,]$acc <- acc
  }
  
  #Calculate distance from each station to each terminal
  for (j in 1:nrow(terminals)) {
    distancesToTherminalKM2[,j] <- as.data.frame( earth.dist(lng$lon, lng$lat,  terminals[j,"lon"], terminals[j,"lat"]) )
    names(distancesToTherminalKM2)[j] <- paste0('distanceToTerminal_',j)
  }
  dis <- distancesToTherminalKM2
  
  #Create dataframe for assignment problem
  for(k in 1:termcons) {
    dis[,(1+(6*(k-1))):(6+(6*(k-1)))] <- distancesToTherminalKM2[,1:6]
  }
  
  if(nrow(dis) < ncol(dis)) {
    dis[(nrow(lng)+1):ncol(dis),] <- 1000000 
  }
  
  #LP assignment problem: minimize distances based on given constraints
  solution <- lp.assign(as.matrix(dis), direction='min')$solution
  
  #Fetch assignments
  assignments <- data.frame(matrix(0,nrow=nrow(lng),ncol = 6))
  for (l in 1:nrow(lng)) {
    r <- which(solution[l,] > 0)
    while(r > 6) {
      r <- r - 6
    }
    assignments[l,r] <- 1
  }
  
  #Store distances and assignments
  distancesToTherminalKM2 <- distancesToTherminalKM2 * assignments
  distanceDF$terminalName <- colnames(distancesToTherminalKM2)[apply(distancesToTherminalKM2,1,which.max)]
  distanceDF$distanceToterminal <- rowMaxs(distancesToTherminalKM2 )
  distanceDF$acc <- as.numeric(lng[,]$acc)
  
  #Distance penalty
  distanceDF$acc <- (1-((distanceDF$distanceToterminal/maxdis)*0.5))*distanceDF$acc
  
  #Distance constaint (if higher than maximum distance)
  distanceDF$acc[ distanceDF$distanceToterminal > maxdis] <- -1000000
  
  #Store terminal connections
  lng$terminalName <- colnames(distancesToTherminalKM2)[apply(distancesToTherminalKM2,1,which.max)]
  lng$distanceToterminal <- rowMaxs(distancesToTherminalKM2 )

  dfConnection <- data.frame(matrix(NA,nrow=nrow(lng),ncol = 3))
  
  dfConnection$station.lon <- lng$lon
  dfConnection$station.lat <- lng$lat
  dfConnection$terminalName <- lng$terminalName
  dfConnection$terminalNumber <- as.numeric(gsub("distanceToTerminal_", "", lng$terminalName))
  dfConnection$terminal.lon <- NA
  dfConnection$terminal.lat <- NA
  
  for(i in 1:nrow(dfConnection)){
    dfConnection[i,]$terminal.lon <- terminals[dfConnection[i,]$terminalNumber,]$lon
    dfConnection[i,]$terminal.lat <- terminals[dfConnection[i,]$terminalNumber,]$lat
  }
  
  lng$terminal.lat <- dfConnection$terminal.lat
  lng$terminal.lon <- dfConnection$terminal.lon
  lng$acc <- distanceDF$acc
  
  dfConnection <- dfConnection[, -c(1:3)]
  
  #Return new dataframe of LNG stations
  return(lng)
}

####################################################
## Preparing UI
####################################################

#Loading icons
icons.pump <- icons(iconUrl = 'http://meuris.se/r/gas-pump.svg',
               iconWidth=20, iconHeight=20)
icons.pump.green <- icons(iconUrl = 'http://meuris.se/r/gas-pump-green.svg',
                    iconWidth=20, iconHeight=20)
icons.pump.blue <- icons(iconUrl = 'http://meuris.se/r/gas-pump-blue.svg',
                    iconWidth=20, iconHeight=20)
icons.pump.darkblue <- icons(iconUrl = 'http://meuris.se/r/gas-pump-darkblue.svg',
                    iconWidth=20, iconHeight=20)
icons.terminal <- icons(iconUrl = 'http://meuris.se/r/warehouse.svg',
                    iconWidth=20, iconHeight=20)
icons.road <- icons(iconUrl = ifelse(roads$total_daily_avg <= def_1, 'http://meuris.se/r/road_green.svg',
                                     ifelse(roads$total_daily_avg <= def_2, 'http://meuris.se/r/road_yellow.svg', 'http://meuris.se/r/road_red.svg')),
               iconWidth=20, iconHeight=20)
icons.pump.n <- icons(iconUrl = ifelse(petrolStations$total <= pdef_1, 'http://meuris.se/r/gas-pump-green.svg',
                                       ifelse(petrolStations$total <= pdef_2, 'http://meuris.se/r/gas-pump-yellow.svg', 'http://meuris.se/r/gas-pump-red.svg')),
                      iconWidth=20, iconHeight=20)
colors <- ifelse(is.na(squares$avgtotal), 'transparant', ifelse(squares$avgtotal <= def_1, 'green', ifelse(squares$avgtotal <= def_2, 'yellow', 'red')))

#Function to reload petrol stations markers
reloadStationMarkers <- function() {
  if(!hidelow) {
    leafletProxy("map", deferUntilFlush=FALSE) %>%
      removeMarker(layerId=petrolStations$ids) %>%
      addMarkers(data=petrolStations, lng=~lon, lat=~lat, icon=icons.pump.n, layerId=petrolStations$ids, group="Gasoline", options=markerOptions(zIndexOffset=6000), popup=~paste0(
        '<b>', name_gasstation, '</b><br>',
        'Type: ', fueltype, '<br>',
        'Cars: ', cars, '<br>',
        'Trucks: ', trucks, '<br>',
        'Total: ', total
      ))
  }
  else {
    subs <- petrolStations[petrolStations$total > pdef_1,]
    icons.pump.n <- icons(iconUrl = ifelse(subs$total <= pdef_1, 'http://meuris.se/r/gas-pump-green.svg',
                                           ifelse(subs$total <= pdef_2, 'http://meuris.se/r/gas-pump-yellow.svg', 'http://meuris.se/r/gas-pump-red.svg')),
                          iconWidth=20, iconHeight=20)
    leafletProxy("map", deferUntilFlush=FALSE) %>%
      removeMarker(layerId=petrolStations$ids) %>%
      addMarkers(data=subs, lng=~lon, lat=~lat, icon=icons.pump.n, layerId=subs$ids, group="Gasoline", options=markerOptions(zIndexOffset=6000), popup=~paste0(
        '<b>', name_gasstation, '</b><br>',
        'Type: ', fueltype, '<br>',
        'Cars: ', cars, '<br>',
        'Trucks: ', trucks, '<br>',
        'Total: ', total
      ))
  }
}

#Function to reload LNG markers
reloadLNGMarkers <- function(lng) {
  leafletProxy("map", deferUntilFlush = FALSE) %>%
    removeMarker(layerId=seq(20001, 20000+200, by=1)) %>%
    addMarkers(data=lng, lng=~lon, lat=~lat, icon=icons.pump.blue, layerId=lng$ids, group="LNG", options=markerOptions(zIndexOffset=10000), popup=~paste0(
      '<b>', name_gasstation, '</b><br>',
      'Status: ', status_gasstation, '<br>',
      'Country: ', country, '<br>',
      'Type: ', fueltype
    ))
}


####################################################
## Shiny app
####################################################
ui <- {
  header <- dashboardHeader(
    title = "DirkOil"
  )
  
  body <- dashboardBody(
    fluidRow(
      column(width = 9,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("map", height = 500)
             ),
             box(width=NULL,
                 tableOutput("lngTable")
             )
      ),
      column(width = 3,
             box(width=NULL, status="warning", title="Optimization", collapsible=TRUE, collapsed=FALSE, solidHeader=TRUE,
                 radioButtons("traffic", "Traffic", c("Cars", "Trucks", "Total"), "Total"),
                 radioButtons("acc", "Accuracy (Higher = slower)", c("Low", "Medium", "High"), "Low"),
                 checkboxGroupInput("chFuel", "Stations", levels(as.factor(allAvailable$fueltype)), levels(as.factor(allAvailable$fueltype))),
                 box(width=NULL, title="Brands", status="primary", collapsible=TRUE, collapsed=TRUE, solidHeader=TRUE,    
                    checkboxGroupInput("chSt", "", levels(as.factor(allAvailable$name_gasstation)), levels(as.factor(allAvailable$name_gasstation)))
                 ),
                 textInput("maxdis", "Max distance", "400"),
                 sliderInput("nrstat", "Number of new stations", min=1, max=25, value=8),
                 sliderInput("termcons", "Max terminal connections", min=1, max=5, value=3), 
                 textInput("km", "Kilometers", "2"),
                 actionButton("btncalc", "Calculate")
             ),
             box(width = NULL, status = "warning", title="Exploration", collapsible=TRUE, collapsed=FALSE, solidHeader=TRUE,
                 sliderInput("range", "Counters", min=r_min, max=r_max, value=c(def_1, def_2), width=250), 
                 sliderInput("rangep", "Stations", min=p_min, max=p_max, value=c(pdef_1, pdef_2), width=250), 
                 checkboxInput("hidelow", "Hide low stations")
             )             
    ))
  )
  
  dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
  )
}

server <- function(input, output) {
   output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng=4.33, lat=50.86, zoom=8) %>%
        addMarkers(data=roads, lng=~lon, lat=~lat, icon=icons.road, layerId=roads$ids, group="Counters", options=markerOptions(zIndexOffset=7000), popup=~paste0(
          '<b>', counter_name, '</b><br>',
          'Cars: ', cars_daily_avg, '<br>',
          'Trucks: ', truck_daily_avg, '<br>',
          'Total: ', total_daily_avg
        )) %>%
        addMarkers(data=cngStations, lng=~lon, lat=~lat, icon=icons.pump.darkblue, group="CNG", options=markerOptions(zIndexOffset=8000), popup=~paste0(
          '<b>', name_gasstation, '</b><br>',
          'Status: ', status_gasstation, '<br>',
          'Country: ', country, '<br>',
          'Type: ', fueltype, '<br>',
          'Cars: ', cars, '<br>',
          'Trucks: ', trucks, '<br>',
          'Total: ', total
        )) %>%
        addMarkers(data=lngStations, lng=~lon, lat=~lat, icon=icons.pump.blue, layerId=lngStations$ids, group="LNG", options=markerOptions(zIndexOffset=10000), popup=~paste0(
          '<b>', name_gasstation, '</b><br>',
          'Status: ', status_gasstation, '<br>',
          'Country: ', country, '<br>',
          'Type: ', fueltype, '<br>',
          'Cars: ', cars, '<br>',
          'Trucks: ', trucks, '<br>',
          'Total: ', total
        )) %>%
        addMarkers(data=terminals, lng=~lon, lat=~lat, icon=icons.terminal, group="Terminals", options=markerOptions(zIndexOffset=9000), popup=~paste0(
          '<b>', name_terminal, '</b>'
        )) %>%
        addMarkers(data=petrolStations, lng=~lon, lat=~lat, icon=icons.pump.n, layerId=petrolStations$ids, group="Gasoline", options=markerOptions(zIndexOffset=6000), popup=~paste0(
          '<b>', name_gasstation, '</b><br>',
          'Type: ', fueltype, '<br>',
          'Cars: ', cars, '<br>',
          'Trucks: ', trucks, '<br>',
          'Total: ', total
        )) %>%
        addRectangles(data=squares, lng1=~lonmin, lng2=~lonmax, lat1=~latmax, lat2=~latmin, group="Traffic", fillColor=colors, fillOpacity=0.5, weight=0) %>%
        hideGroup("Gasoline") %>%
        hideGroup("CNG") %>%
        hideGroup("Counters") %>%
        hideGroup("Traffic") %>%
        addLayersControl(
          overlayGroups=c("Counters", "CNG", "LNG", "Terminals", "Gasoline", "Traffic"),
          options=layersControlOptions(collapsed=FALSE)
        )
   })
   
   lngTable <- lngStations[,c(1,2,4,7,8,9)]
   colnames(lngTable) <- c("Name", "Status", "Type", "Cars", "Trucks", "Total")
   output$lngTable <- renderTable(lngTable)
   
   #On change in road slider
   observeEvent(input$range, {
     rng <<- input$range
     def_1 <<- rng[1]
     def_2 <<- rng[2]
     icons.road <- icons(iconUrl = ifelse(roads$total_daily_avg <= def_1, 'http://meuris.se/r/road_green.svg',
                                          ifelse(roads$total_daily_avg <= def_2, 'http://meuris.se/r/road_yellow.svg', 'http://meuris.se/r/road_red.svg')),
                         iconWidth=20, iconHeight=20)
     leafletProxy("map") %>%
       removeMarker(layerId=roads$ids) %>%
       addMarkers(data=roads, lng=~lon, lat=~lat, icon=icons.road, layerId=roads$ids, group="Counters", options=markerOptions(zIndexOffset=7000), popup=~paste0(
         '<b>', counter_name, '</b><br>',
         'Cars: ', cars_daily_avg, '<br>',
         'Trucks: ', truck_daily_avg, '<br>',
         'Total: ', total_daily_avg
       ))
   })
   
   #On change in stations slider
   observeEvent(input$rangep, {
     prng <<- input$rangep
     pdef_1 <<- prng[1]
     pdef_2 <<- prng[2]
     reloadStationMarkers()
   })
   
   observeEvent(input$hidelow, {
     hidelow <<- input$hidelow
     reloadStationMarkers()
   })
   
   observeEvent(input$btncalc, {
     if(!is.na(as.numeric(input$km)) && as.numeric(input$km) > 0) {
       if(!is.na(as.numeric(input$maxdis)) && as.numeric(input$maxdis) > 0) {
         #Check if capacity is enough for new number of stations
         if(input$termcons*6 >= (input$nrstat+2)) {
           #Create new dataframe based on user input
           avst <- allAvailable[allAvailable$name_gasstation %in% input$chSt & allAvailable$fueltype %in% input$chFuel,]
           
           #Check if new dataframe has any rows
           if(nrow(avst) > 0) {
             #Clearing old markers
             leafletProxy("map", deferUntilFlush=FALSE) %>%
               clearShapes()
             reloadLNGMarkers(lngStations)
             
             #Fetching user input
             nr <- input$nrstat
             km <- as.numeric(input$km)
             maxdis <- as.numeric(input$maxdis)
             choice <- input$traffic
             choice <- switch(choice, Cars=1, Trucks=2, Total=3)
             acc <- switch(input$acc, Low=50, Medium=200, High=500)
             termcons <- input$termcons

             lng <- lngStations
             
             #Pick new stations
             withProgress(message="Calculating optimal locations...", value=0, {
               newStations <- pickLNGStations(avst, lng, nr, km, choice, maxdis, termcons, acc)

               #Reload map and draw lines
               reloadLNGMarkers(newStations)
               reloadStationMarkers()
               for(i in 1:nrow(newStations)) {
                 leafletProxy("map") %>%
                   addPolylines(lng=c(newStations[i,]$lon, newStations[i,]$terminal.lon), lat=c(newStations[i,]$lat, newStations[i,]$terminal.lat), layerId=as.character(as.integer(newStations[i,]$ids)+1000))
               }
               
               #Prepare and add table
               lngTable <- newStations[,c(1,2,4,7,8,9,12,13,11)]
               colnames(lngTable) <- c("Name", "Status", "Type", "Cars", "Trucks", "Total", "Terminal", "Distance", "Opt")
               lngTable$Terminal <- paste("Terminal", as.integer(gsub("distanceToTerminal_", "", lngTable$Terminal)))
               lngTable[nrow(lngTable)+1,] <- list("TOTAL", "", "", sum(lngTable$Cars), sum(lngTable$Trucks), sum(lngTable$Total), "", sum(lngTable$Distance), sum(lngTable$Opt))
               output$lngTable <- renderTable(lngTable)
             })
           }
           else {
             showNotification("Error: No stations selected")           
           }
         }
         else {
           showNotification("Error: Number of LNG stations too high for terminal capacity")           
         }
       }
       else {
         showNotification("Error: Max distance must be a numeric value (> 0)")
       }
     }
     else {
       showNotification("Error: Kilometers must be a numeric value (> 0)")
     }
   })
}

####################################################
## Plot functions
####################################################
plots <- function() {
  ## na GA function uit alle loops: 
  
  processDFF <- data.frame(matrix(NA,nrow=30,ncol = 2))
  colnames(processDFF) <- c('opt', 'distance')
  
  for(i in 1:20){
    
    newStations <- pickLNGStations(petrolStations, lngStations, i, 2, 3, 400, 4, acc = 200 )
    
    processDFF[i,]$opt <- sum(newStations$acc)
    processDFF[i,]$distance <- sum(newStations$distanceToterminal)
    processDFF[i,]$sumCars <- sum(newStations$cars)
    processDFF[i,]$sumTrucks <- sum(newStations$trucks)
    processDFF[i,]$sumTotal <- sum(newStations$total)
    
    
  }
  
  ## distance:
  x <- c(1:20)
  distance <- processDFF$distance[1:20]
  dt <- data.frame(x,distance)
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "number of stations added",
    titlefont = f
  )
  y <- list(
    title = "distance",
    titlefont = f
  )
  
  plot_ly(dt, x = ~x, y = ~distance, type = 'scatter', mode = 'lines') %>%
    layout(title = "Distance from the stations to the terminals", xaxis = x, yaxis = y)
  
  ## opt:
  x <- c(1:20)
  opt <- processDFF$opt[1:20]
  dt <- data.frame(x,opt)
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "number of stations added",
    titlefont = f
  )
  y <- list(
    title = "optimality",
    titlefont = f
  )
  
  plot_ly(dt, x = ~x, y = ~distance, type = 'scatter', mode = 'lines') %>%
    layout(title = "Model optimality evolution", xaxis = x, yaxis = y)
}

####################################################
## Run the application 
####################################################

shinyApp(ui = ui, server = server)