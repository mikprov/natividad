library(shiny)
library(ggplot2)
library(Cairo)
library(tidyverse)
library(tmap)
library(rgdal)
library(readr)
library(broom)
library(RColorBrewer)

# https://shiny.rstudio.com/articles/plot-interaction-advanced.html

workshop.packages <- c("sp", "rgdal", "rgeos", "raster", "dismo", "leaflet", "RColorBrewer", "classInt")
lapply(workshop.packages, library, character.only = TRUE)         
setwd("C:/Users/Mikaela/Documents/GitHub/natividad/shapefiles")


#1. read in kelp data
k <- read.csv(file="C:/Users/Mikaela/Box Sync/Natividad/kelp_bio_baja_2009_2019.csv",stringsAsFactors = FALSE)


k_locations <- k[,c("lat","long")]
k_locations$id <- as.character(seq(from=1,to=length(k[,1]),by=1))
dd <- k[,-c(1:2)] #remove lat long 
d1 <- as.matrix(dd)
d1 <- matrix(d1,ncol=ncol(dd),dimnames = NULL)
rownames(d1) <- k_locations$ID
rm(dd)


rr <- apply(d1,MARGIN=1,function(x) rle(x))
xx <- lapply(rr, function(x) as.data.frame(do.call("cbind",x)))
xx <- lapply(xx, function(x) x[complete.cases(x),] ) #rm rows with nan
xx <- lapply(xx, function(x) x[x$values==0,]) #keep only zeros, no kelp
names(xx) <- k_locations$id
rm(rr)
x <- bind_rows(xx,.id='id')
x$twoyr <- ifelse(x$lengths >= 8,1,0) #flag instances longer than 2 years
x$oneyr <- ifelse(x$lengths >= 4,1,0) #flag instances longer than 1 years

x_tally <- x %>% group_by(id) %>% summarise(freq2yr = sum(twoyr))
k_locations1 <- left_join(k_locations,x_tally,by="id")
k_locations1$freq2yr[is.na(k_locations1$freq2yr)] = 0
k_locations1$relativesize <- k_locations1$freq2yr/4

# get the shapefile
s <- shapefile("countries.shp")
f <- shapefile("FEDECOOP.shp")

# transform fedecoop projection to match countries projection
newcrs <-CRS( "+proj=longlat +datum=WGS84 +no_defs")
f <- spTransform(f, newcrs)
s
f

# latitude plots
#tiff("C:/Users/Mikaela/Documents/GitHub/natividad/plots/kelp_map1.tif", res = 300, width = 7, height = 6, units = 'in' )
# ggplot(s, aes(x=long, y=lat, group=group)) +
#   geom_polygon(alpha=0.5,fill="lightgrey") +
#   geom_path(color="black") +
#   geom_polygon(data=f,aes(x=long,y=lat,group=group),alpha=0.5) +
#   theme_minimal() +
#   geom_point(data=k_locations1,aes(x=long,y=lat,color=freq2yr),inherit.aes = FALSE) +
#   coord_fixed(ylim=c(26,33), xlim=c(-118,-110)) 
#dev.off()




ui <- fluidPage(
  fluidRow(
    
    column(width = 8, class = "well",
           h4("Left plot controls right plot"),
           fluidRow(
             column(width = 6,
                    plotOutput("plot2", height = 400,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               )
                    )
             ),
             column(width = 6,
                    plotOutput("plot3", height = 400)
             )
           )
    )
    
  )
)

server <- function(input, output) {
  

  
  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(s, aes(x=long, y=lat, group=group)) +
      geom_polygon(alpha=0.5,fill="lightgrey") +
      geom_path(color="black") +
      geom_polygon(data=f,aes(x=long,y=lat,group=group),alpha=0.5) +
      theme_minimal() +
      geom_point(data=k_locations1,aes(x=long,y=lat,color=freq2yr),inherit.aes = FALSE) +
      coord_fixed(ylim=c(26,33), xlim=c(-118,-110))
    #ggplot(mtcars, aes(wt, mpg)) +geom_point() 
  })
  
  output$plot3 <- renderPlot({
    ggplot(s, aes(x=long, y=lat, group=group)) +
      geom_polygon(alpha=0.5,fill="lightgrey") +
      geom_path(color="black") +
      geom_polygon(data=f,aes(x=long,y=lat,group=group),alpha=0.5) +
      theme_minimal() +
      geom_point(data=k_locations1,aes(x=long,y=lat,color=freq2yr),inherit.aes = FALSE) +
      #coord_fixed(ylim=c(26,33), xlim=c(-118,-110)) +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    
    # ggplot(mtcars, aes(wt, mpg)) +
    #   geom_point() +
    #   coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
}

shinyApp(ui = ui, server = server)

