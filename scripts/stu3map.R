############ STU 3 Map ##############

# https://leafletjs.com/examples/crs-simple/crs-simple.html
# https://rstudio.github.io/leaflet/basemaps.html
# https://stackoverflow.com/questions/45722157/render-custom-tiles-in-r-leaflet-from-local-directory-i-e-not-from-a-git-repos
# https://stackoverflow.com/questions/59174298/r-using-addresourcepath-for-rendering-local-leaflet-tiles
# https://rpubs.com/walkerke/custom_tiles
# https://stackoverflow.com/questions/5960405/convert-a-single-large-image-overlay-to-tiles-for-google-maps
# https://cran.r-project.org/web/packages/tiler/vignettes/tiler-intro.html
# https://stackoverflow.com/questions/3955095/creating-raster-images-using-r


library(tiler)

setwd("D:/STU/STU 3")

tiler_options()
tiler_options(osgeo4w = "D:/QGIS/OSGeo4W.bat", python = "D:/QGIS/apps/Python39/python.exe")

tile_dir = file.path("D:/STU/STU 3", "tiles")
map = "D:/STU/STU 3/Trench.png"
tile(map, tile_dir, "0-3", georef=F)

tile_viewer(tile_dir, "1-4")
view_tiles(tile_dir)

library(leaflet)
library(shiny)
library(knitr)
library(rmarkdown)

ui = fluidPage(leafletOutput("map", height = "100vh"))

server = function(input, output, session) {
  addResourcePath("mytiles", "D:/STU/STU 3/tiles")
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(crs = leafletCRS("L.CRS.Simple"), minZoom = 1, maxZoom = 4)) %>% 
      addTiles(urlTemplate = "/mytiles/{z}/{x}/{y}.png",
               options = tileOptions(noWrap = T)) %>% 
      setView(120, -120, 2)
  })
}

shinyApp(ui, server)

server1 = function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 1, maxZoom = 4)) %>% 
      addTiles(urlTemplate = "https://kehilaa.github.io/stu/{z}/{x}/{y}.png",
               options = tileOptions(noWrap = T))
  })
}

shinyApp(ui, server1)



tiles <- "https://leonawicz.github.io/tiles/st2/tiles/{z}/{x}/{y}.png"
tiles <- "https://kehilaa.github.io/stu/{z}/{x}/{y}.png" 
leaflet(
  options = leafletOptions(height = "100%", crs = leafletCRS("L.CRS.Simple"), 
    minZoom = 1, maxZoom = 4, attributionControl = FALSE), width = "100%") %>%
  addTiles(tiles, options = tileOptions(noWrap = T)) %>% 
  setView(60, -60, 2) %>%
  addCircles(lng=120, lat=-120) 





