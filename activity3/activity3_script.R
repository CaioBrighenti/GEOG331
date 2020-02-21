#create a function. The names of the arguements for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")


#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("activity3/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevent units
sensorInfo <-   read.csv("activity3/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])


# Data QA/QC
library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temperature
length(which(is.na(datW$soil.moisture)))

#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")


#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularily confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)


#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)


#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")

#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

# QUESTION 5
# show that lightscale corresponds with datW
assert(which(datW$lightning.acvitivy == max(datW$lightning.acvitivy)) == 
         which(lightscale == max(lightscale)), "error: indices don't match")


#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))


# filter out storms in wind data
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

# check if worked
assert(!(FALSE %in% is.na(datW$wind.speedQ1[datW$precipitation  >= 2 & datW$lightning.acvitivy >0])),"error: outliers not filtered out")
assert(!(FALSE %in% is.na(datW$wind.speedQ1[datW$precipitation > 5])),"error: outliers not filtered out")

plot(datW$doy, datW$wind.speedQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind speed (m/s)")

# check on soil data
# look at what "normal" values are
quantile(datW$soil.moisture,na.rm=TRUE)
soil_extreme <- quantile(datW$soil.moisture,na.rm=TRUE,probs = c(0.05,0.95))

# first look at last few observations
last_obs <- tail(datW[!is.na(datW$soil.moisture),]$soil.moisture,10)

# which are below/above 0.05/.95 probability?
length(last_obs[last_obs < soil_extreme[1] | last_obs > soil_extreme[2]])

# are these problematic wind/storm observations?
## if false, not a problem obs, if true is
is.na(tail(datW[!is.na(datW$soil.moisture),]$air.tempQ2,10))

# averages table
library(dplyr)
means_table <- datW %>%
  summarize_at(.vars = c("air.tempQ2","wind.speedQ1","soil.moisture","soil.temp"),mean, na.rm = TRUE)

# number of observations
num_obs_table <- datW %>%
  mutate(air.tempQ2 = if_else(is.na(air.tempQ2),0,1),
         wind.speedQ1 = if_else(is.na(wind.speedQ1),0,1),
         soil.moisture = if_else(is.na(soil.moisture),0,1),
         soil.temp = if_else(is.na(soil.temp),0,1)
         )  %>%
  summarize_at(.vars = c("air.tempQ2","wind.speedQ1","soil.moisture","soil.temp"),sum)

# first/last date
## filter our NAs, then grab first and last row and extract DOY
datW %>%
  filter(!is.na(air.tempQ2)) %>%
  filter(row_number() == 1 | row_number()==n()) %>%
  pull(doy)

datW %>%
  filter(!is.na(wind.speedQ1)) %>%
  filter(row_number() == 1 | row_number()==n()) %>%
  pull(doy)

datW %>%
  filter(!is.na(soil.moisture)) %>%
  filter(row_number() == 1 | row_number()==n()) %>%
  pull(doy)

datW %>%
  filter(!is.na(soil.temp)) %>%
  filter(row_number() == 1 | row_number()==n()) %>%
  pull(doy)

# making plots for question 9
par(mfrow=c(2,2))
plot(datW$doy, datW$air.tempQ2, pch=19, xlim=c(163,207), type="b", xlab = "Day of Year",
     ylab="air temperature (degrees c)")

plot(datW$doy, datW$wind.speedQ1, pch=19, xlim=c(163,207), type="b", xlab = "Day of Year",
     ylab="Wind speed (m/s)")

plot(datW$doy, datW$soil.moisture, pch=19, xlim=c(163,207), type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 of water per cm3 of soil)")

plot(datW$doy, datW$soil.temp, pch=19, xlim=c(163,207), type="b", xlab = "Day of Year",
     ylab="Soil temperature (degrees c)")



