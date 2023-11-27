library(leaflet)
setwd("D:/STU/STU 3")

crs = leafletCRS(crsClass = "L.CRS.Simple")                                     # code = "EPSG:4326"
rec = 0.9

coo = data.frame(lat = c(1, 60, 120, 120, 1, 60, 1, 2), lng = c(1, 60, 120, 1, 60, 1, 2, 1))
coords = coo * 1.937 + .78

data = read.csv2("coords.csv")
head(data)

system = subset(data, Feldtyp=="System")
system$x = as.numeric(sapply(strsplit(system$Koords, "|", fixed=T), function(x) x[1])) * 1.937 + .78
system$y = as.numeric(sapply(strsplit(system$Koords, "|", fixed=T), function(x) x[2])) * 1.937 + .78

nebel = subset(data, Feldtyp=="Nebel")
nebel = nebel[!duplicated(nebel$Koords),]
nebel$x = as.numeric(sapply(strsplit(nebel$Koords, "|", fixed=T), function(x) x[1])) * 1.937 + .78
nebel$y = as.numeric(sapply(strsplit(nebel$Koords, "|", fixed=T), function(x) x[2])) * 1.937 + .78

pal.nebel = colorFactor(c("white", "black"), domain = c("passierbar", "unpassierbar"))

lab.nebel = sprintf("%s<br/>%s", nebel$Koords, nebel$System) %>% lapply(htmltools::HTML)
lab.system = sprintf("%s<br/>%s", system$Koords, system$System) %>% lapply(htmltools::HTML)


m = leaflet(options = leafletOptions(minZoom = 1, maxZoom = 4, crs = crs)) %>% 
  
  addTiles(urlTemplate = "https://kehilaa.github.io/stu/{z}/{x}/{y}.png",
           options = tileOptions(noWrap = T)) %>% 
  
  addRectangles(lng1=nebel$x - rec, lat1=nebel$y*(-1) - rec, 
                lng2=nebel$x + rec, lat2=nebel$y*(-1) + rec, 
                fillColor = "transparent", color=pal.nebel(nebel$Detail), opacity=.9, weight=.7, 
                label=lab.nebel, labelOptions = labelOptions(
                  style = list("font-weight" = "normal"),
                  textsize = "14px"), 
                group = "Nebel") %>% 
  
  addCircles(lng=system$x, lat=system$y*(-1), radius=0.75, 
             fillColor = "transparent", color="green", weight=3, opacity=.8, 
             label=lab.system, labelOptions = labelOptions(
               style = list("font-weight" = "normal"),
               textsize = "14px"), 
             group = "Systeme") %>% 
#  addCircles(lng=coords$lng, lat=coords$lat*(-1), radius=0.75) %>% 
  
  addLayersControl(overlayGroups = c("Nebel", "Systeme"), 
                   options = layersControlOptions(collapsed=F))

m


# 3752 x 3750



