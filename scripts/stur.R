library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(stringr)

setwd("H:/pers. Verwaltung/sonstiges/Schmarn")

data = read.csv2("H:/pers. Verwaltung/sonstiges/Schmarn/coords.csv", encoding="latin1")
data$System[data$System=="Wa??sa-Nebel"] = "Wałęsa-Nebel"
data$System[data$System=="Brz?czyszczykiewicz-Sturmwolke"] = "Brzęczyszczykiewicz-Sturmwolke"
data$Detail = ifelse(data$Detail=="Nebel passierbar", "passierbar", 
                     ifelse(data$Detail=="Nebel unpassierbar", "unpassierbar", 
                            ifelse(data$Detail=="Nebel aus Quelltext", "unbekannt", data$Detail)))
data$x = as.numeric(sapply(strsplit(data$Koords, "|", fixed=T), function(x) x[1])) * 1.937 + .78
data$y = as.numeric(sapply(strsplit(data$Koords, "|", fixed=T), function(x) x[2])) * 1.937 + .78
data$Detail[data$Detail=="Nebel aus QuelltextNebel passierbar"] = "passierbar"
data$detail1 = ifelse(data$Detail %in% c("passierbar", "unbekannt", "unpassierbar"), data$Detail, "System")
data$weight = ifelse(data$Feldtyp=="Nebel", 1, 3)
data$opacity = ifelse(data$Feldtyp=="Nebel", .9, .8)
data$System = gsub("-System", "", data$System)
data$Detail = gsub("Binärsystem, ", "", data$Detail)
data = data[order(data$Feldtyp, data$System),]

crs = leafletCRS(crsClass = "L.CRS.Simple")
pal = colorFactor(c("cyan", "green", "red", "white"), data$detail1, ordered=T)
#previewColors(pal, unique(data$detail1))
rec = 0.9


ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      style = "overflow-y:scroll; max-height: 99vh; position:relative;",
      fluidRow(
        column(width=3, 
               checkboxGroupInput("check", "", unique(data$detail1[data$Feldtyp=="Nebel"]), 
                                  selected=unique(data$detail1[data$Feldtyp=="Nebel"]))), 
        column(width=9,
               selectInput("neb", "Region", unique(subset(data, Feldtyp=="Nebel")$System), multiple=T, selectize=T))
        ), 
      hr(), 
      DT::dataTableOutput("tbl"), 
      hr(), 
      textAreaInput("rein", "Koordinateneingabe", height="200px"),       
      fluidRow(
        column(width=5, 
               selectInput("col", "Circlecolor", c("yellow", "gold", "violet", "deeppink", "limegreen", "skyblue", "lightgrey"))),
        column(width=3, 
               materialSwitch("switch", "Circle/Marker"))
        ), 
      verbatimTextOutput("text") #, tableOutput("df")
      ), 
    mainPanel(
      leafletOutput("map", height = "100vh")
      )
    )
  )

server1 = function(input, output, session) {
  nebel = reactive({
      if(is.null(input$neb)) subset(data, Feldtyp=="Nebel" & data$detail1 %in% input$check) 
    else data[data$System %in% input$neb & data$detail1 %in% input$check, ] # 
  })

  syst = data[data$Feldtyp=="System", ]
  
  output$tbl = DT::renderDataTable(
    syst %>% select(System, Koords, Detail), 
    extensions="Scroller", style="bootstrap", class="compact", width="100%", 
    options=list(deferRender=TRUE, scrollX=T, scrollY="50vH", scroller=TRUE)
  )
  
  output$map = renderLeaflet({
    leaflet(height = "calc(100vh - 50px)", width = "100%", 
            options = leafletOptions(minZoom = 1, maxZoom = 4, 
                                     attributionControl = FALSE, crs = crs)) %>% 
      addTiles(urlTemplate = "https://kehilaa.github.io/stu/{z}/{x}/{y}.png",
               options = tileOptions(noWrap = T)) %>% 
      setView(120, -120, 2)  
  })
  
  observe({
    sysfilt = if(is.null(input$tbl_rows_selected))
      subset(data, Feldtyp=="System") 
    else 
      subset(data, Feldtyp=="System") [input$tbl_rows_selected,]
   
    leafletProxy("map") %>% 
      
      clearShapes() %>% 
      
      addRectangles(lng1=nebel()$x - rec, lat1=nebel()$y*(-1) - rec, 
                    lng2=nebel()$x + rec, lat2=nebel()$y*(-1) + rec, 
                    fillColor = pal(nebel()$detail1), #fillColor = "transparent", 
                    color = pal(nebel()$detail1), 
                    weight = nebel()$weight, opacity = nebel()$opacity,
                    label = paste(nebel()$Koords,"<br/>", nebel()$System) %>% lapply(htmltools::HTML), #lab.nebel, 
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal"),
                      textsize = "14px"), 
                    group = "Nebel") %>% 
      
      addCircles(lng=sysfilt$x, lat=sysfilt$y*(-1), radius=0.75, 
                 fillColor = pal(sysfilt$detail1), 
                 color=pal(sysfilt$detail1), 
                 weight=sysfilt$weight, opacity=sysfilt$opacity, 
                 label=paste(sysfilt$Koords,"<br/>", sysfilt$System) %>% lapply(htmltools::HTML), #lab.system, 
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal"),
                   textsize = "14px"), 
                 group = "Systeme") %>%
 
      addLayersControl(overlayGroups = c("Nebel", "Systeme", "Eingabe"), 
                       position = "topleft", 
                       options = layersControlOptions(collapsed=T))

  })
  
  observe({
    if(input$rein == "") 
      eingabe = data.frame(rein = "-1|-1") 
    else
      eingabe = data.frame(rein = unlist(strsplit(str_replace_all(input$rein, "[\\r\\n\\t\\s]+", " "), " ", fixed=T)))
    
    eingabe$x = suppressWarnings(as.numeric(sapply(strsplit(eingabe$rein, "|", fixed=T), function(x) x[1])) * 1.937 + .78)
    eingabe$y = suppressWarnings(as.numeric(sapply(strsplit(eingabe$rein, "|", fixed=T), function(x) x[2])) * 1.937 + .78)
    eingabe = eingabe[complete.cases(eingabe), ]
    
    output$text = renderText({ input$rein })
#    output$df = renderTable(eingabe)
    
    proxy = leafletProxy("map") %>% 
      clearGroup("Eingabe")

    if(input$switch){
      proxy %>% 
        addMarkers(lng=eingabe$x, lat=eingabe$y*(-1), 
                   label=paste(eingabe$rein) %>% lapply(htmltools::HTML), #lab.system, 
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal"),
                     textsize = "14px"), 
                   group = "Eingabe")
      }
    else{
      proxy %>% 
        addCircles(lng=eingabe$x, lat=eingabe$y*(-1), radius=0.6, 
                   fillColor = input$col, fillOpacity = .5,
                   color = input$col, 
                   weight = 2, opacity = .9,  
                   label=paste(eingabe$rein) %>% lapply(htmltools::HTML), #lab.system, 
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal"),
                     textsize = "14px"), 
                   group = "Eingabe")
      }
  })
  
}

shinyApp(ui, server1)

