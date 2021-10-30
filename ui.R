#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(data.table)
library(shiny)
library(leaflet)
library(readr)
library(cowplot)


datos <- fread("datos1718.csv", encoding = "UTF-8")
colnames(datos) <- c("PLATFORM", "ARGOS_ID",  "LATITUDE", "LONGITUDE",
                     "PRESSURE",     "TEMPERATURE", "SALINITY",
                     "HOUR", "DAY", "MONTH", "YEAR")



shinyUI(navbarPage("Analysis type ",
                   tabPanel(h4("Horizontal Profile"),
  
    

  # Application title
  titlePanel("Salinity and Temperature "),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 
                 sliderInput("LONGITUD", "Longitude",
                             round = -1, step = 0.1,           
                             min = round(min(datos$LONGITUDE),1),
                             max = round(max(datos$LONGITUDE),1),
                             value = c(-84, -80)),
                 
                 sliderInput("LATITUD", "Latitude",
                             round = -1, step = 0.1, 
                             min = round(min(datos$LATITUDE),1),
                             max = round(max(datos$LATITUDE),1),
                             value = c(-10, -8)),
                 
                 numericInput(inputId = "PresionMinima",
                              label = "Minimum pressure (meters):",
                              value = 0),
                 
                 numericInput(inputId = "PresionMaxima",
                              label = "Maximum pressure (meters):",
                              value = 100),
                 
                 sliderInput("year", "Years",
                             round = 1, step = 1, 
                             min = min(datos$YEAR),
                             max = max(datos$YEAR),
                             value = c(min(datos$YEAR),max(datos$YEAR))),
                 
                 sliderInput("MES", "Months",
                             round = 1, step = 1, 
                             min = min(datos$MONTH),
                             max = max(datos$MONTH),
                             value = c(1,12)),
                 
                 sliderInput("DIA", "Day",
                             round = 1, step = 1, 
                             min = min(datos$DAY),
                             max = max(datos$DAY),
                             value = c(1,31)),
                 
                 sliderInput("interp2", "Interpolation",
                             round = 1, step = 10, 
                             min = 10,
                             max = 300,
                             value = c(30)),
                 
    
                numericInput(inputId = "TempMin",
                             label = "Minimum Temperature:",
                             value = 0),
    
                numericInput(inputId = "TempMax",
                            label = "Maximum Temperature:",
                            value = 100),
                
                downloadButton("BajarData", "Download data")
                
                 
    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(navbarPage(h4("Tabs"),
                         
                         tabPanel(h4("Geographic boundaries"),
                                  leafletOutput("peru"),
                                  h4("Summary"),
                                  verbatimTextOutput("caracteristicas")
                         ),
                         
                         tabPanel(h4("Salinity"),
                                  h5("Salinity vs. Date for selected depth range and coordinates "),
                                  plotOutput("plotSal"),
                                 
                         ),
                         tabPanel(h4("Temperature"),
                                  h5("Temperature vs. Date for selected depth range and coordinates "),
                                  plotOutput("plotTemp"),
                                  
                         ),
                         
                         tabPanel(h4("T-S"),
                                  h5("Temperature - Salinity Diagram "),
                                  plotOutput("diaTempSal"),
                                  verbatimTextOutput("TempSal")
                         ),
                         
                         tabPanel(h4("Isolines"),
                                 plotOutput("isoTemp")
                         ),
                         
                         tabPanel(h4("Table"),
                                  dataTableOutput("tabla1")
                         )
    )
  ))),
  tabPanel(h4("Vertical Profile"),
           
           sidebarLayout(
             sidebarPanel(width = 3,
                          
                          
                          sliderInput("LONGITUD2", "Longitude",
                                      round = -1, step = 0.1,           
                                      min = round(min(datos$LONGITUDE),1),
                                      max = round(max(datos$LONGITUDE),1),
                                      value = c(-84, -80)),
                          
                          sliderInput("LATITUD2", "Latitude",
                                      round = -1, step = 0.1, 
                                      min = round(min(datos$LATITUDE),1),
                                      max = round(max(datos$LATITUDE),1),
                                      value = c(-10, -8)),
                          
                          numericInput(inputId = "PresionMinima2",
                                       label = "Minimum pressure (meters):",
                                       value = 0),
                          
                          numericInput(inputId = "PresionMaxima2",
                                       label = "Maximum pressure (meters):",
                                       value = 100),
                          
                          sliderInput("año2", "Years",
                                      round = 1, step = 1, 
                                      min = min(datos$YEAR),
                                      max = max(datos$YEAR),
                                      value = c(max(datos$YEAR))),
                          
                          sliderInput("MES2", "Months",
                                      round = 1, step = 1, 
                                      min = min(datos$MONTH),
                                      max = max(datos$MONTH),
                                      value = c(1,12)),
                          
                          sliderInput("DIA2", "Day",
                                      round = 1, step = 1, 
                                      min = min(datos$DAY),
                                      max = max(datos$DAY),
                                      value = c(1,31)),
                          
                          sliderInput("interp", "Interpolation",
                                      round = 1, step = 10, 
                                      min = 10,
                                      max = 300,
                                      value = c(30)),
                          
                          numericInput(inputId = "TempMin2",
                                       label = "Minimum Temperature:",
                                       value = 0),
                          
                          numericInput(inputId = "TempMax2",
                                       label = "Maximum Temperature:",
                                       value = 100),
                          
                          downloadButton("BajarData2", "Download data")
                          
                          
                          
                          
             ),
             
             # Show a plot of the generated distribution
             mainPanel(navbarPage(h4("Tabs"),
                                  
                                  tabPanel(h4("Geographic boundaries"),
                                           leafletOutput("peru2"),
                                           h4("Summary"),
                                           verbatimTextOutput("caracteristicas2"),
                                           verbatimTextOutput("resumen3")
                                  ),
                                  
                                  tabPanel(h4("Isolines"),
                                           h5("Vertical profiles. Select specific year and range of months"),
                                           plotOutput("plotSalVert")
                                           
                                  ),
                                  
                                  tabPanel(h4("Table"),
                                           dataTableOutput("tabla2")
                                  )
                                 
             )
             )
           
))))
