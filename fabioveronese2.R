# run preceeding steps first
source("fabioveronese1.R")

# use Google Maps
library("plotGoogleMaps")
GreaterLondon.Google <- GreaterLondonUTM[, "name"]
Borough <- GreaterLondonUTM[, "name"]
for(i in unique(GreaterLondonUTM$name)) {
    sub.name <- Local.Intensity[Local.Intensity[, 1] == i, 2]
    Borough[Borough$name == i, "Intensity"] <- sub.name
    Borough[Borough$name == i, "Intensity.Area"] <- round(sub.name / 
                                                              (GreaterLondonUTM[GreaterLondonUTM$name==i, ]@polygons[[1]]@area / 10000), 4)
}
plotGoogleMaps(Borough, 
               zcol = "Intensity", 
               filename = "Crimes_Boroughs.html", 
               layerName = "Number of Crimes", 
               fillOpacity = 0.4, 
               strokeWeight = 0, 
               mapTypeId = "ROADMAP")

# use Leaflet
Borough.Leaflet <- toGeoJSON(Borough)
map.style <- styleGrad(pro = "Intensity", 
                       breaks = seq(min(Borough$Intensity), 
                                    max(Borough$Intensity) + 15, 
                                    by = 20), 
                       style.val = cm.colors(10), 
                       leg = "Number of Crimes", 
                       fill.alpha = 0.4, 
                       lwd = 0)
leaflet(Borough.Leaflet, 
        popup = c("name", "Intensity", "Intensity.Area"), 
        style = map.style)

# plot kernel density on Google Maps
Density <- density.ppp(DrugsUTM.ppp, 
                       sigma = 500, 
                       edge = T, 
                       W = as.mask(window, 
                                   eps = c(100,100)))
Density.raster <- raster(Density)
projection(Density.raster) = projection(GreaterLondonUTM)
plotGoogleMaps(Density.raster, 
               filename = "Crimes_Density.html", 
               layerName = "Number of Crimes", 
               fillOpacity = 0.4, 
               strokeWeight = 0, 
               colPalette = rev(heat.colors(10)))

# create contour lines and plot on Leaflet
Contour <- rasterToContour(Density.raster, 
                           maxpixels = 100000, 
                           nlevels = 10)
Contour.Leaflet <- toGeoJSON(Contour)
colour.scale <- color.scale(1:(length(Contour$level) - 1), 
                            color.spec = "rgb", 
                            extremes = c("red", "blue"))
map.style <- styleGrad(pro = "level", 
                       breaks = Contour$level, 
                       style.val = colour.scale, 
                       leg = "Number of Crimes", 
                       lwd = 2)
leaflet(Contour.Leaflet, 
        style = map.style, 
        base.map = "tls")