#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("activity6/GNPglaciers/GNPglaciers_1966.shp")
g1998 <- readOGR("activity6/GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("activity6/GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("activity6/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

g1966@proj4string

spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))
#read in rgb imagery from landsat
redL <- raster("activity6/glacier_09_05_14/l08_red.tif")
greenL <- raster("activity6/glacier_09_05_14/l08_green.tif")
blueL <- raster("activity6/glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)


# zoom in 
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)


#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("activity6/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

# dfouble plot
par(mfrow=c(1,2),mai=c(1,1,1,1))
plot(NDVIraster[[1]])
plot(g1966,axes=TRUE,xaxs="i",yaxs="i")



#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

### QUESTION 4
length(NDVIraster)
par(mfrow=c(1,1),mai=c(1,1,1,1))
plot(NDVIraster[[13]],axes=FALSE)
plot(g2015p,axes=FALSE,add=TRUE,col=NA,border="black")




#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

  for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}  

# question 5
g2015p$perc.change <- (gAll$a2015m.sq-gAll$a1966m.sq) / gAll$a1966m.sq
spplot(g2015p, "perc.change",main = "Percent change from 1966 to 2015 in glacier area")

# visualize differences
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

# question 6
max_glacier <- gAll[which(g2015p$perc.change == min(g2015p$perc.change)),]
max_perc <- abs(max_glacier$perc.chage * 100)
max_1966 <- subset(g1966, GLACNAME==max_glacier$GLACNAME)
max_1998 <- subset(g1998, GLACNAME==max_glacier$GLACNAME)
max_2005 <- subset(g2005, GLACNAME==max_glacier$GLACNAME)
max_2015 <- subset(g2015, GLACNAME==max_glacier$GLACNAME)

par(mai=c(1,1,1,1),col.axis="white",col.lab="white",tck=0)
plotRGB(rgbL, ext=c(271200,276600,5424500,5430000), stretch="lin", axes = TRUE,
        main = paste("Visualization of",max_glacier$GLACNAME,"loss of",round(max_perc,digits=1),"% of area"))
plot(max_1966, col="palegreen2", border=NA, add=TRUE)
plot(max_1998, col="royalblue3", add=TRUE, border=NA)
plot(max_2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(max_2015, col="tomato3", add=TRUE, border=NA)

# analyzing NDVI
#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)


#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)

#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)


#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)


# get mean change
meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

# question 9
g2015p$mean.change <- meanChange[-1,2]
spplot(g2015p, "mean.change",main = "Mean change in NDVI for each glacier zone in 2015")

# question 10
head(meanChange)
summary(meanChange[,2])

# get biggest
max_mean <- max(abs(meanChange[,2]))

# see range of values
NDVIstack

# years to change NDVI
1 / max_mean

# question 11
NDVIraster
