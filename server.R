gc()

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(leaflet)
library(leaflet.extras)

library(ggplot2)
library(akima)
library(viridis)
library(ggthemes)

datos <- fread("datos1718.csv",  encoding = "UTF-8")
colnames(datos) <- c("PLATFORM", "ARGOS_ID",  "LATITUDE", "LONGITUDE",
                     "PRESSURE",     "TEMPERATURE", "SALINITY",
                     "HOUR", "DAY", "MONTH", "YEAR")

datos$capa <- ifelse(datos$PRESSURE <= 20, 1, 
                     ifelse(datos$PRESSURE > 20 & datos$PRESSURE <=50,2,
                            ifelse(datos$PRESSURE > 50 &datos$PRESSURE <= 200, 3,
                                   ifelse(datos$PRESSURE > 200 & datos$PRESSURE < 600, 4,
                                          ifelse(datos$PRESSURE > 600 & datos$PRESSURE < 1000, 5, 6)))))



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  datasetInput <- reactive({
    
    datos <- base::subset(datos, datos$PRESSURE >= input$PresionMinima & datos$PRESSURE <= input$PresionMaxima & datos$SALINITY > 33 & datos$TEMPERATURE<35 & datos$SALINITY < 37 & datos$TEMPERATURE > 0 & datos$LONGITUDE >= input$LONGITUD[1] & datos$LONGITUDE <= input$LONGITUD[2] & datos$LATITUDE >= input$LATITUD[1] & datos$LATITUDE <= input$LATITUD[2] & datos$YEAR  >= input$year[1] & datos$YEAR <= input$year[2] & datos$MONTH >= input$MES[1] & datos$MONTH <= input$MES[2] & datos$DAY  >= input$DIA[1] & datos$DAY <= input$DIA[2])
  
     })
  
  
  
  output$peru <- renderLeaflet({
    
    dat2 <- datasetInput()
    dat3 <- aggregate(dat2, by=list(dat2$YEAR, dat2$PLATFORM), max)
    dat4 <- aggregate(dat3, by=list(dat3$MONTH, dat3$PLATFORM), max)
    dat5 <- aggregate(dat4, by=list(dat4$DAY, dat4$PLATFORM), max)
    dat6 <- aggregate(dat5, by=list(dat5$HOUR, dat5$PLATFORM), max)
    
    dat6$content <- paste(sep = "",
                          "<b><a href='http://www.ifremer.fr/co-argoFloats/float?detail=false&ptfCode=", 
                          dat6$PLATFORM,"'>",dat6$PLATFORM,"</a></b>")
    
    
  #  http://www.ifremer.fr/co-argoFloats/float?detail=false&ptfCode=1901925&active=true&ocean=A&lang=en
    #http://www.ifremer.fr/co-argoFloats/float?detail=false&ptfCode=3901188&active=false&ocean=P&lang=en
    
    mapa <- leaflet() %>%
      setView(lng = mean(datos$LONGITUDE), lat = mean(datos$LATITUDE), zoom = 4) %>%
      addProviderTiles("Stamen.TonerLite",
                       group = "Toner", 
                       options = providerTileOptions(minZoom = 4, maxZoom = 8)) %>%
      addTiles(group = "OSM",
               options = providerTileOptions(minZoom = 4, maxZoom = 8)) %>%
      addProviderTiles("Esri.WorldTopoMap",    
                       group = "Topo") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") %>%
      addProviderTiles("CartoDB.Positron",     group = "CartoDB") %>%
      addLayersControl(baseGroups = c("Toner", "OSM", "Topo", "Mapnik", "CartoDB"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addGraticule(interval = 10, options = pathOptions(pointerEvents = "none", clickable = TRUE)) %>%
      
      addRectangles(lng1=input$LONGITUD[1], lat1=input$LATITUD[1],
                    lng2=c(input$LONGITUD[2]), lat2=input$LATITUD[2],
                    fillColor = "transparent")%>%
      addMarkers(data = dat6, ~LONGITUDE-1.8, ~LATITUDE, popup = dat6$content, label = ~as.character(PLATFORM),clusterOptions = markerClusterOptions())
    
  })
  
  output$caracteristicas <- renderPrint({
    
    dat2 <- datasetInput()
    
    par_fecha <- subset(dat2, dat2$YEAR == min(dat2$YEAR))
    par_fecha <- subset(par_fecha, par_fecha$MONTH == min(par_fecha$MONTH))
    par_fecha <- subset(par_fecha, par_fecha$DAY == min(par_fecha$DAY))
    
    
    par_fecha2 <- subset(dat2, dat2$YEAR == max(dat2$YEAR))
    par_fecha2 <- subset(par_fecha2, par_fecha2$MONTH == max(par_fecha2$MONTH))
    par_fecha2 <- subset(par_fecha2, par_fecha2$DAY == max(par_fecha2$DAY))
    
    Ta <- paste("Your selection has", length(dat2$SALINITY), "entries corresponding to", length(unique(dat2$PLATFORM)), "floats.")
    Tb <- paste("The data was taken from ", min(par_fecha$DAY), min(par_fecha$MONTH), min(par_fecha$YEAR), sep = "/")
    Tc <- paste("until", max(par_fecha2$DAY), max(par_fecha2$MONTH), max(par_fecha2$YEAR), sep = "/")
    
    cat(paste(Ta, Tb, Tc, sep="\n"))
  })
  
  
  output$resumen <- renderPrint({
    

    dat2 <- datasetInput()
  

    summary(dat2[,6:7])
  })
  
  
  
  output$plotSal <- renderPlot({
    
    dat2 <- datasetInput()
    dat2$momento <- (dat2$YEAR-2010)*12 + dat2$MONTH 
    
    promedio <- aggregate(dat2$SALINITY, by=list(dat2$momento), mean)
    colnames(promedio) <- c("momento", "promedio")
    
    ploteando <- function(dependiente, independiente, promedio, momento, color) {
      
      plot(dependiente ~ independiente, col = color , pch = 19, xlab = "MONTH", ylab = "SALINITY")
      points(promedio ~ momento , type = "l", col = "red", lwd =4)
    
    }
    
    ploteando(dat2$SALINITY, dat2$momento, promedio$promedio, promedio$momento, dat2$YEAR)
    
    
  })
  
  
  
  
    output$plotTemp <- renderPlot({
    
    
    dat2 <- datasetInput()
    #dat2 <- datos
    dat2$momento <- (dat2$YEAR-2010)*12 + dat2$MONTH 
    
    promedio <- aggregate(dat2$TEMPERATURE, by=list(dat2$momento), mean)
    colnames(promedio) <- c("momento", "promedio")
    
    ploteando <- function(dependiente, independiente, promedio, momento, color) {
      
      plot(dependiente ~ independiente, col = color , pch = 19, xlab = "MONTH", ylab = "TEMPERATURE")
      points(promedio ~ momento , type = "l", col = "red", lwd =4)
      
    }
    
    ploteando(dat2$TEMPERATURE, dat2$momento, promedio$promedio, promedio$momento, dat2$YEAR)
    
    
  })
  
 
  
   
  output$diaTempSal <- renderPlot({
    
    prueba2 <- datasetInput()
    prueba <- datos
    ploteandoTS <- function(datos) {
      
      #par(mfrow=c(1,2))
      
      
      par(mar=c(5,4,2,10))
      par(xpd=TRUE)
      
      Temperature <- seq(0,30,1)
      Salinity <- seq(33,36,0.10)
      
      
      plot(Temperature~Salinity, xaxt = "n", yaxt ="n", type ="n", yaxs="i", xaxs="i")
      
      axis(1, at=seq(33,36,0.2))
      axis(2, at=seq(0,30,0.5))
      
     
      
      prueba <- subset(prueba2, prueba2$MONTH >= input$MES[1] & prueba2$MONTH <= input$MES[2])
      prueba <- subset(prueba, prueba$DAY >= input$DIA[1] & prueba$DAY <= input$DIA[2])
      
      
      points(prueba$TEMPERATURE~prueba$SALINITY, pch =1, col = prueba$capa)
      
 
      
      
      # ATS
      xx = c(33, 33.8, 33.8, 33)
      yy = c(18,18, 30, 30)
      polygon(xx,yy, border = "red", lwd = 3)
     
      
      # AES
      xx = c(33.8, 34.8, 34.8, 33.8)
      yy = c(17,17, 30, 30)
      polygon(xx,yy, border="purple",lwd = 3)

      # ACF
      xx = c(34.8, 35.1, 35.1, 34.8)
      yy = c(17,17, 22, 22)
      polygon(xx,yy, border="hotpink",lwd = 3)

      # ASS
      xx = c(35.1, 36, 36, 35.1)
      yy = c(17, 17, 30, 30)
      polygon(xx,yy, border="sienna1",lwd = 3)

      # ATSA
      xx = c(34.8, 34.6, 34.6, 34.8)
      yy = c(13, 13, 15, 15)
      polygon(xx,yy, border="green2",lwd = 3)

      # AESS
      xx = c(34.9, 35.2, 35.2, 34.9)
      yy = c(13, 13, 17, 17)
      polygon(xx,yy, border="turquoise1",lwd = 3)

      # AEP
      xx = c(34.6, 34.9, 34.9, 34.6)
      yy = c(7, 7, 13, 13)
      polygon(xx,yy, border="yellow",lwd = 3)

      # AAI
      xx = c(34.45, 34.6, 34.6, 34.45)
      yy = c(4,4,7, 7)
      polygon(xx,yy, border="lightblue",lwd = 3)

      
      legend(36, 28, legend = c("0-20", "20-50", "50-200", "200-600", "600-1000", ">1000" ), fill=c(1,2,3,4,5,6), title = "Depth")
      legend(36, 15, legend = c("ATS", "AES", "ACF", "ASS", "ATSA", "AESS", "AEP", "AAI" ), fill=c("red", "purple", "hotpink", "sienna1", "green2", "turquoise1", "yellow", "lightblue"), title = "Water bodies")
      
     }
    
    ploteandoTS(datos)
    
    
    
    
    
  })
  

  
  output$TempSal <- renderPrint({
    
    prueba <- datasetInput()
    mod <- lm(prueba$TEMPERATURE ~ prueba$SALINITY)
    summary(mod)
    
  })
  
  output$isoTemp <- renderPlot({
    
   
      prueba <- datasetInput()    
      prueba <- subset(prueba, prueba$TEMPERATURE <= input$TempMax & prueba$TEMPERATURE >= input$TempMin )
      
      

      
      di <- interp(prueba$LONGITUD, prueba$LATITUD, prueba$TEMPERATURE,
                   xo=seq(min(prueba$LONGITUD), max(prueba$LONGITUD), length=input$interp2),
                   yo=seq(min(prueba$LATITUD), max(prueba$LATITUD), length=input$interp2),
                   duplicate = "mean")
      interp1 <- data.frame(expand.grid(x=di$x, y=di$y), z=c(di$z))
      
      
      di2 <- interp(prueba$LONGITUD, prueba$LATITUD, prueba$SALINITY,
                   xo=seq(min(prueba$LONGITUD), max(prueba$LONGITUD), length=input$interp2),
                   yo=seq(min(prueba$LATITUD), max(prueba$LATITUD), length=input$interp2),
                   duplicate = "mean")
      interp2 <- data.frame(expand.grid(x=di2$x, y=di2$y), z=c(di2$z))
      
      
      ploteandoIso <- function(interp1, interp2) {
        
      plot1 <- ggplot(interp1) +
        aes(x=x, y=y, z=z, fill=z) +
        labs(x = "Longitud", y = "Latitud", fill = "Temperatura", title = "Isotermas para area y años seleccionados")+
        geom_tile() +
        stat_contour(color="white", size=0.15) +
        scale_fill_gradientn(colours = c("blue", "yellow", "red"), space = "rgb")+
        theme_tufte(base_family="Helvetica")
        
      plot2 <- ggplot(interp2) +
        aes(x=x, y=y, z=z, fill=z) +
        labs(x = "Longitud", y = "Latitud", fill = "Salinidad", title = "Isohalinas para area y años seleccionados")+
        geom_tile() +
        stat_contour(color="white", size=0.15) +
        scale_fill_viridis() +
        theme_tufte(base_family="Helvetica")
      
      plot_grid(plot1, plot2, labels = "AUTO")
      }
      ploteandoIso(interp1, interp2)
    
    
  })
  
  output$tabla1 <- renderDataTable({
    
    prueba <- datasetInput()
    prueba <- subset(prueba, prueba$MONTH >= input$MES[1] & prueba$MONTH <= input$MES[2])
    prueba <- subset(prueba, prueba$DAY >= input$DIA[1] & prueba$DAY <= input$DIA[2])
    
    data.table(prueba)
  })
  
  ##########################
  ##########################
  ##### Perfiles Verticales#
  ##########################
  ##########################
  
  datasetInput2 <- reactive({
    
    
    
    dat <- base::subset(datos, datos$PRESSURE >= input$PresionMinima2 & datos$PRESSURE <= input$PresionMaxima2)
    dat2 <- dat
    dat2 <- subset(dat2, dat2$SALINITY > 33 & dat2$TEMPERATURE <35 & dat2$SALINITY < 37 & dat2$TEMPERATURE > 0)
    dat2 <- subset(dat2, dat2$LONGITUDE >= input$LONGITUD2[1] & dat2$LONGITUDE <= input$LONGITUD2[2] & dat2$LATITUDE >= input$LATITUD2[1] & dat2$LATITUDE <= input$LATITUD2[2])
    dat2 <- subset(dat2, dat2$YEAR  == input$year & dat2$MONTH >= input$MES2[1] & dat2$MONTH <= input$MES2[2] & dat2$DAY >= input$DIA2[1] & dat2$DAY <= input$DIA2[2] & dat2$TEMPERATURE <= input$TempMax2 &  dat2$TEMPERATURE >= input$TempMin2)

  })
  
  
  output$peru2 <- renderLeaflet({
    
    dat2 <- datasetInput2()
    dat3 <- aggregate(dat2, by=list(dat2$YEAR, dat2$PLATFORM), max)
    dat4 <- aggregate(dat3, by=list(dat3$MONTH, dat3$PLATFORM), max)
    dat5 <- aggregate(dat4, by=list(dat4$DAY, dat4$PLATFORM), max)
    dat6 <- aggregate(dat5, by=list(dat5$HOUR, dat5$PLATFORM), max)
    

    
    dat6$content <- paste(sep = "",
                          "<b><a href='http://www.ifremer.fr/co-argoFloats/float?detail=false&ptfCode=", 
                          dat6$PLATFORM,"'>",dat6$PLATFORM,"</a></b>")
    
    mapa <- leaflet() %>%
      setView(lng = mean(datos$LONGITUDE), lat = mean(datos$LATITUDE), zoom = 4) %>%
      addProviderTiles("Stamen.TonerLite",
                       group = "Toner", 
                       options = providerTileOptions(minZoom = 4, maxZoom = 8)) %>%
      addTiles(group = "OSM",
               options = providerTileOptions(minZoom = 4, maxZoom = 8)) %>%
      addProviderTiles("Esri.WorldTopoMap",    
                       group = "Topo") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") %>%
      addProviderTiles("CartoDB.Positron",     group = "CartoDB") %>%
      addLayersControl(baseGroups = c("Toner", "OSM", "Topo", "Mapnik", "CartoDB"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addGraticule(interval = 10, options = pathOptions(pointerEvents = "none", clickable = TRUE)) %>%
      
      addRectangles(lng1=input$LONGITUD2[1], lat1=input$LATITUD2[1],
                    lng2=input$LONGITUD2[2], lat2=input$LATITUD2[2],
                    fillColor = "transparent")%>%
      addMarkers(data = dat6, ~LONGITUDE-1.8, ~LATITUDE, popup = dat6$content, label = ~as.character(PLATFORM),clusterOptions = markerClusterOptions())
    
  })
  
  
  output$plotSalVert <- renderPlot({
    
    
    prueba <- datasetInput2()
    
    
    
    di <- interp(prueba$LONGITUDE, prueba$PRESSURE, prueba$TEMPERATURE,
                 xo=seq(min(prueba$LONGITUDE), max(prueba$LONGITUDE), length=input$interp),
                 yo=seq(min(prueba$PRESSURE), max(prueba$PRESSURE), length=input$interp),
                 duplicate = "mean")
    interp1 <- data.frame(expand.grid(x=di$x, y=di$y), z=c(di$z))
    
    
    di2 <- interp(prueba$LONGITUDE, prueba$PRESSURE, prueba$SALINITY,
                  xo=seq(min(prueba$LONGITUDE), max(prueba$LONGITUDE), length=input$interp),
                  yo=seq(min(prueba$PRESSURE), max(prueba$PRESSURE), length=input$interp),
                  duplicate = "mean")
    interp2 <- data.frame(expand.grid(x=di2$x, y=di2$y), z=c(di2$z))
    
    
    ploteandoIso <- function(interp1, interp2) {
      
      plot1 <- ggplot(interp1) +
        aes(x=x, y=y, z=z, fill=z) +
        labs(x = "Longitude", y = "Pressure", fill = "TEMPERATURE", title = "Vertical Temperature Profile")+
        geom_tile() +
        stat_contour(color="white", size=0.04) +
        scale_fill_gradientn(colours = c("blue", "yellow", "red"), space = "rgb")+   
        scale_y_reverse(lim=c(max(prueba$PRESSURE), min(prueba$PRESSURE)))+
        theme_tufte(base_family="Helvetica")
      
      plot2 <- ggplot(interp2) +
        aes(x=x, y=y, z=z, fill=z) +
        labs(x = "Longitude", y = "Pressure", fill = "SALINITY", title = "Vertical profile of Salinity")+
        geom_tile() +
        stat_contour(color="white", size=0.04) +
        scale_fill_viridis() +
        scale_y_reverse(lim=c(max(prueba$PRESSURE), min(prueba$PRESSURE)))+
        theme_tufte(base_family="Helvetica")
      plot_grid(plot1, plot2, labels = "AUTO")
      
    }
    ploteandoIso(interp1, interp2)
    
    
  })
  
  output$resumen3 <- renderPrint({
    dat2 <- datasetInput2()
   # paste("Estamos jugando con", length(prueba$PRESSURE), "entradas correspondientes a", length(unique(prueba$PLATFORM)), "flotadores")
  
    par_fecha <- subset(dat2, dat2$YEAR == min(dat2$YEAR))
    par_fecha <- subset(par_fecha, par_fecha$MONTH == min(par_fecha$MONTH))
    par_fecha <- subset(par_fecha, par_fecha$DAY == min(par_fecha$DAY))
    
    
    par_fecha2 <- subset(dat2, dat2$YEAR == max(dat2$YEAR))
    par_fecha2 <- subset(par_fecha2, par_fecha2$MONTH == max(par_fecha2$MONTH))
    par_fecha2 <- subset(par_fecha2, par_fecha2$DAY == max(par_fecha2$DAY))
    
    Ta <- paste("Your selection has", length(dat2$SALINITY), "entries corresponding to", length(unique(dat2$PLATFORM)), "floats.")
    Tb <- paste("The data was taken from ", min(par_fecha$DAY), min(par_fecha$MONTH), min(par_fecha$YEAR), sep = "/")
    Tc <- paste("until", max(par_fecha2$DAY), max(par_fecha2$MONTH), max(par_fecha2$YEAR), sep = "/")
    
    cat(paste(Ta, Tb, Tc, sep="\n"))
    
    })
  
  output$tabla2 <- renderDataTable({
    prueba <- datasetInput2()
    data.table(prueba)
  })
  
 
})

