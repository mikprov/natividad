# Plot Isla Natividad
setwd("C:/Users/Mikaela/Documents/GitHub/natividad")
       
workshop.packages <- c("sp", "rgdal", "rgeos", "raster", "dismo", "leaflet", "RColorBrewer", "classInt","XML","ggmap")
lapply(workshop.packages, library, character.only = TRUE)         


#load in the provinces data (downloaded from naturalearthdata.com)
s <- shapefile("shapefiles/MEX.shp")
s
pts <- shapefile("shapefiles/colectores_11_25_08.shp")
pts
nat <- shapefile("shapefiles/FEDECOOP.shp")

# many files associated with shapefile. As a group, make up the shapefile
# look for coordinate ref system, it should be fine
plot(s)
zoom(s, ext=drawExtent(), new=FALSE) 
plot(nat, add=TRUE, col="red")
plot(pts, add=TRUE, col="red",lwd=3)

leaflet() %>% addTiles() %>% setView(lng=-115.187317, lat=27.876204, zoom=4) %>% addCircles(data=pts)
leaflet(nat) %>% addTiles() 
amap <- addTiles(amap) # The default is OpenStreetMap, you can see all the options by typing "name(providers)"

#amap %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
amap <- addMarkers(amap, popup="A. sagrei") #can also add a marker by specifying lng=,lat=
amap
#see https://rstudio.github.io/leaflet/ for more options

#*********CHALLENGE: Create a leaflet of Davis,CA centered around a point labeled YOU ARE HERE!
amap.davis <- leaflet()
amap.davis <- addTiles(amap.davis) 
amap.davis <- addMarkers(amap.davis, lng=-121.748913, lat=38.539784, popup="you are here") 
# coordinates came from Google maps(?)
amap.davis
