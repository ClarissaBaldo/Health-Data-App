####################################################################################
# geospatial health data visualization in R
####################################################################################

# find instructions at:
# https://www.paulamoraga.com/book-geospatial/sec-spatialdataandCRS.html

# load packages
library(sp)
library(rgdal) 
library(ggplot2)
library(maptools)
library(raster)

setwd("C:/Documents/Spatial_Health_Data/")
map <- readOGR("./wards_birmingham_osgb/", layer = "Wards_BHAM_polygon") # read shape file  

class(map) # it is a SpatialPolygonsDataFrame (sp object)
head(map@data)
plot(map) # plot the map

crs(map) # retrieve coordinate reference system - "OSGB36 / British National Grid"
extent(map) # check spatial extent (extremes) 
proj4string(map) # get the coordinate reference system (projection)

####################################################################################
# reproject sp objects
# set the WGS84 longitude/latitude projection 
geo.prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
map_WGS84 <- spTransform(map, CRS(geo.prj)) 
proj4string(map_WGS84) # check the new projections

class(map_WGS84) # it is a SpatialPolygonsDataFrame (sp object)
head(map_WGS84@data)
plot(map_WGS84) # plot the map

####################################################################################
# convert the map to a simple feature object of class sf 

library(sf)
map_sf <- st_as_sf(map_WGS84)

####################################################################################
# Build an app with shiny to plot health spatial data
####################################################################################

# introduction to shiny https://www.paulamoraga.com/book-geospatial/sec-shiny.html
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/
# https://appsilon.com/leaflet-vs-tmap-build-interactive-maps-with-r-shiny/
# https://github.com/r-tmap/tmap/issues/566
# https://stackoverflow.com/questions/36203200/how-to-make-a-dataset-reactive-in-shiny
# https://community.rstudio.com/t/tmap-in-r-shiny-based-on-user-input/59839
# https://rdrr.io/cran/tmap/man/renderTmap.html
# https://community.rstudio.com/t/shiny-reactive-input-for-multiple-input-selectors/35157/5

####################################################################################
# build datasets with dummy health data

# health data scenario 1
health_data_scenario1 <- data.frame(map_WGS84@data$wd16nm)
colnames(health_data_scenario1) <- "Wards"
health_data_scenario1$`Early deaths` <- sample(100, size = nrow(health_data_scenario1), replace = TRUE)
health_data_scenario1$`Heart disease` <- sample(70, size = nrow(health_data_scenario1), replace = TRUE) # add second dummy variable
health_data_scenario1$`Lung cancer` <- sample(150, size = nrow(health_data_scenario1), replace = TRUE) # add second dummy variable

library(reshape2)
health_data_scenario1_melt <- melt(health_data_scenario1)

# health data scenario2
health_data_scenario2 <- data.frame(map_WGS84@data$wd16nm)
colnames(health_data_scenario2) <- "Wards"
health_data_scenario2$`Early deaths` <- sample(100, size = nrow(health_data_scenario2), replace = TRUE)
health_data_scenario2$`Heart disease` <- sample(70, size = nrow(health_data_scenario2), replace = TRUE) # add second dummy variable
health_data_scenario2$`Lung cancer` <- sample(200, size = nrow(health_data_scenario1), replace = TRUE) # add second dummy variable

library(reshape2)
health_data_scenario2_melt <- melt(health_data_scenario2)

####################################################################################
# build a dataset with customize legend titles, breaks and colors for plotting using tmap
l1 <- c(0, 10, 25, 50, 75, 100)
l2 <- c(0, 5, 10, 35, 50, 70)
l3 <- c(0, 25, 50, 100, 150, 200)

my_breaks_list <- list(`Early deaths` = l1, `Heart disease` = l2, `Lung cancer` = l3)
my_colors_list <- list(`Early deaths`="Oranges",`Heart disease`="Purples", `Lung cancer`="Greys")
my_legend_title_list <- list(`Early deaths`="Early deaths",`Heart disease`="Heart disease", `Lung cancer`="Lung cancer")

library(dplyr)
library(tmap) # for maps
library(shiny)
library(DT) # for tables
library(readr)

setwd("C:/Documents/Spatial_Health_Data/")

####################################################################################
# Build the app 

# design the user interface
ui <- fluidPage(
  
  # add title
  titlePanel(strong("Health burden from air pollution exposure in Birmingham", style = "color:#6495ED")),
  
  # set sidebar layout
  sidebarLayout(position = "left",
                
                # customize the sidebar panel
                sidebarPanel(
                  
                  # add some text at the top of the panel
                  p("View the annual health burden attributed to air pollution exposure in Birmingham 
                    by selecting a variable of interest:"),
                  
                  # insert a selection box
                  selectInput(
                    inputId = "variableselected",
                    label = "Select variable",
                    choices = c("Early deaths", "Heart disease", "Lung cancer")),
                  
                  # add some text below the selection box in bold
                  strong("Use the tab to switch from the current health burden to the expected values following
                    air pollution reduction below the WHO guideline levels", style = "color:#6495ED"),
                  
                  br(), # add space
                  br(),
                  br(),
                  br(),
                  
                  # add WM-Air logo
                  tags$img(src = "wm_air_logo_rgb.jpg", height = 830/10, width = 1455/10)),
                
                # customize the main panel
                mainPanel(
                  
                  # create two top tab panels to show interactive maps corresponding to scenarios 1-2
                  tabsetPanel(
                    tabPanel("Annual Health Burden", tmapOutput(outputId = "map1")), # scenario 1
                    tabPanel("WHO-guideline Scenario", tmapOutput(outputId = "map2"))), # scenario 2
                  
                  # add the download button 
                  downloadButton('downloadData', 'Download data'),
                  
                  # create two bottom tab panels to show data corresponding to scenarios 1-2
                  tabsetPanel(
                    tabPanel("Annual Health Burden", DTOutput(outputId = "table1")),
                    tabPanel("WHO-guideline Scenario", DTOutput(outputId = "table2")))
                  )))

# work on the server
server <- function(input, output) {
  
  # create reactive data to input variables 
  # to use in the output table and interactive map corresponding to scenario 1
  data_scenario1 <- reactive({
    health_data_scenario1_melt <- filter(health_data_scenario1_melt, variable %in% input$variableselected)
  })
  
  # create reactive data to input variables 
  # to use in the output table and interactive map corresponding to scenario 2
  data_scenario2 <- reactive({
    health_data_scenario2_melt <- filter(health_data_scenario2_melt, variable %in% input$variableselected)
  })
  
  # create reactive data to input variables 
  # this combines the reactive data from scenario 1 and 2 into a unique dataset
  # which will be possible to downloaded for each selected input variable
  data_save <- reactive({
    data_1 <- data_scenario1()
    data_2 <- data_scenario2()
    
    colnames(data_1)[3] <- "Annual Health Burden"
    colnames(data_2)[3] <- "WHO-guideline scenario"
    
    data_save <- full_join(data_1, data_2)
  })
  
  # create reactive breaks to input variables
  my_breaks <- reactive({
    c(unlist(my_breaks_list[input$variableselected]))
  })
  
  # create reactive colors to input variables
  my_colors <- reactive({
    unlist(my_colors_list[input$variableselected])
  })
  
  # create reactive legend titles to input variables
  my_legend_title <- reactive({
    my_legend_title_list[input$variableselected]
  })
  
  # create reactive interactive map to input variables for scenario 1
  dataTmap_scenario1 <- reactive({
    mymap_info <- map_sf[,c("wd16nm","objectid","wd16cd","wd16nmw","lad16cd","lad16nm","bng_e","bng_n","long","lat","st_areasha","st_lengths","geometry")]
    colnames(mymap_info)[1] <- "Wards"
    mymap <- full_join(mymap_info, data_scenario1())
  })
  
  # create reactive interactive map to input variables for scenario 2
  dataTmap_scenario2 <- reactive({
    mymap_info <- map_sf[,c("wd16nm","objectid","wd16cd","wd16nmw","lad16cd","lad16nm","bng_e","bng_n","long","lat","st_areasha","st_lengths","geometry")]
    colnames(mymap_info)[1] <- "Wards"
    mymap <- full_join(mymap_info, data_scenario2())
    
  })
  
  # define output table reactive to selected input variables for scenario 1
  output$table1 <- renderDT(data_scenario1())
  
  # define output table reactive to selected input variables for scenario 2
  output$table2 <- renderDT(data_scenario2())
  
  # define output interactive map reactive to selected input variables for scenario 1
  output$map1 <- renderTmap({
    tmap_mode("view")
    tm_shape(dataTmap_scenario1()) +
      tm_polygons(col = "value", breaks = my_breaks(), palette = my_colors(), title = my_legend_title()) 
  })
  
  # define output interactive map reactive to selected input variables for scenario 2
  output$map2 <- renderTmap({
    tmap_mode("view")
    tm_shape(dataTmap_scenario2()) +
      tm_polygons(col = "value", breaks = my_breaks(), palette = my_colors(), title = my_legend_title()) 
  })
  
  # define output download data
  # through the download button it will be possible to save out 
  # data for both scenarios in one unique dataset
  output$downloadData <- downloadHandler(
    
    # build a function to assign the file name depending on the selected input variable
    filename = function() {
      paste("data-", Sys.Date(), "-", my_legend_title(), ".csv", sep="")
    },
    
    # save the content reactive to selected input variables as .csv
    content = function(file) {
      write.csv(data_save(), file)
    }
  )
}

# launch the shinyApp()
shinyApp(ui = ui, server = server)

# launch the shinyApp() directly running the saved script using R
# library(shiny)
# runApp("C:/Documents/Spatial_Health_Data/Health_Data_App.R")

