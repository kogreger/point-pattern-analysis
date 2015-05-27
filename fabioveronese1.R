# load the crime data
library(spatstat)
data <- read.csv("http://www.fabioveronesi.net/Blog/2014-05-metropolitan-street.csv")
str(data)
data <- data[!is.na(data$Longitude) & !is.na(data$Latitude), ]

# identify (locational) duplicates
library(sp)
coordinates(data) = ~ Longitude + Latitude
zero <- zerodist(data)
length(unique(zero[, 1]))

# load the London borough data
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip", 
              destfile = "ne_10m_admin_1_states_provinces.zip")
unzip("ne_10m_admin_1_states_provinces.zip", 
      exdir="NaturalEarth")
library(raster)
border <- shapefile("NaturalEarth/ne_10m_admin_1_states_provinces.shp")
GreaterLondon <- border[paste(border$region) == "Greater London", ]

# assign the boroughs' projection to the crime data (EPSG 4326, WGS Lat/Lon)
projection(data) = projection(border)
# cut crime data to spatial extent of London boroughs
overlay <- over(data, GreaterLondon)
data$over <- overlay$OBJECTID_1
data.London <- data[!is.na(data$over), ]

# plot the London crime data with boroughs
plot(data.London, 
     pch = "+", 
     cex = 0.5, 
     main = "", 
     col = data.London$Crime.type)
plot(GreaterLondon, 
     add = T)
legend(x = -0.53, 
       y = 51.41, 
       pch = "+", 
       col = unique(data.London$Crime.type), 
       legend = unique(data.London$Crime.type), 
       cex=0.4)

# calculate mean center and standard distance
mean_centerX <- mean(data.London@coords[, 1])
mean_centerY <- mean(data.London@coords[, 2])
standard_deviationX <- sd(data.London@coords[, 1])
standard_deviationY <- sd(data.London@coords[, 2])
standard_distance <- sqrt(sum(((data.London@coords[, 1] - mean_centerX)^2 + 
                                   (data.London@coords[, 2] - mean_centerY)^2)) / 
                              (nrow(data.London)))

# plot mean center and standard distance
library(plotrix)
plot(data.London, 
     pch = "+", 
     cex = 0.5, 
     main = "")
plot(GreaterLondon, 
     add=T)
points(mean_centerX, 
       mean_centerY, 
       col = "red", 
       pch = 16)
draw.circle(mean_centerX, 
            mean_centerY, 
            radius = standard_distance, 
            border = "red", 
            lwd=2)

# plot mean center and standard deviations (x, y)
plot(data.London, 
     pch = "+", 
     cex = 0.5, 
     main = "")
plot(GreaterLondon, 
     add = T)
points(mean_centerX, 
       mean_centerY, 
       col = "red", 
       pch = 16)
draw.ellipse(mean_centerX, 
             mean_centerY, 
             a = standard_deviationX, 
             b = standard_deviationY, 
             border = "red", 
             lwd = 2)

# filter for drug-related crimes
Drugs <- data.London[data.London$Crime.type == 
                         unique(data.London$Crime.type)[3], ]
Drugs <- remove.duplicates(Drugs)

# convert all data to UTM (EPSG 32632, WGS 84 UTM zone 32N)
library(maptools)
library(rgdal)
GreaterLondonUTM <- spTransform(GreaterLondon, 
                                CRS("+init=epsg:32630"))
DrugsUTM <- spTransform(Drugs, 
                        CRS("+init=epsg:32630"))
window <- as.owin(GreaterLondonUTM)
DrugsUTM.ppp <- ppp(x = DrugsUTM@coords[, 1], 
                    y = DrugsUTM@coords[,2 ], 
                    window=window)

# calculate overall intensity
DrugsUTM.ppp$n/sum(sapply(slot(GreaterLondonUTM, "polygons"), slot, "area"))

# calculate and plot quadratcount
plot(DrugsUTM, 
     pch = "+", 
     cex = 0.5, 
     main = "Drugs")
plot(GreaterLondonUTM, 
     add = T)
plot(quadratcount(DrugsUTM.ppp, 
                  nx = 4, 
                  ny = 4), 
     add = T, 
     col = "blue")

# calculate local intensity per borough
Local.Intensity <- data.frame(Borough = factor(), 
                              Number = numeric())
for(i in unique(GreaterLondonUTM$name)) {
    sub.pol <- GreaterLondonUTM[GreaterLondonUTM$name == i, ]
    sub.ppp <- ppp(x = DrugsUTM.ppp$x, 
                   y = DrugsUTM.ppp$y, 
                   window = as.owin(sub.pol))
    Local.Intensity <- rbind(Local.Intensity, 
                             data.frame(Borough = factor(i, 
                                                         levels = GreaterLondonUTM$name), 
                                        Number = sub.ppp$n))
}

# barplot of local intensities per borough
colorScale <- color.scale(Local.Intensity[order(Local.Intensity[, 2]), 2], 
                          color.spec = "rgb", 
                          extremes = c("green", "red"), 
                          alpha=0.8)
barplot(Local.Intensity[order(Local.Intensity[, 2]), 2], 
        names.arg = Local.Intensity[order(Local.Intensity[, 2]), 1], 
        horiz = T, 
        las = 2, 
        space = 1, 
        col = colorScale)

# plot various kernel density estimators
plot(density.ppp(DrugsUTM.ppp, 
                 sigma = bw.diggle(DrugsUTM.ppp), 
                 edge = T), 
     main = paste("h =", 
                  round(bw.diggle(DrugsUTM.ppp), 2)))
plot(density.ppp(DrugsUTM.ppp, 
                 sigma = bw.ppl(DrugsUTM.ppp), 
                 edge=T), 
     main=paste("h =", round(bw.ppl(DrugsUTM.ppp), 2)))
plot(density.ppp(DrugsUTM.ppp, 
                 sigma = bw.scott(DrugsUTM.ppp)[2], 
                 edge=T), 
     main=paste("h =", round(bw.scott(DrugsUTM.ppp)[2], 2)))
plot(density.ppp(DrugsUTM.ppp, 
                 sigma = bw.scott(DrugsUTM.ppp)[1], 
                 edge=T), 
     main=paste("h =",round(bw.scott(DrugsUTM.ppp)[1], 2)))

# plot G statistic
plot(Gest(DrugsUTM.ppp), 
     main="Drug Related Crimes")