#Server side of durham crime map
library(shiny)
library(leaflet)
library(tidyverse)
library(rgdal)
library(raster)
library(RColorBrewer)

#Load our data
load('Data/crimedat.final.RData')
#4 data sets
#crime - shapefile point dataset of crimes from 2016
#crime.rate - rasterized version of individual crimes
#neighb - Census block neighborhoods from 2005
#octcrime - Crime rate for october only


shinyServer(function(input, output) {
  #Setup neighborhood parcels to be colored by a unique id
  neighb$id <- seq(1,nrow(neighb),by=1)
  cols <- brewer.pal(10,name='Spectral')
  neighb.col <-colorNumeric(cols,
                           domain = neighb$id)
  
  #Extract a crime rate by neighborhood.
  neighb$rate <- extract(crime.rate,
                         neighb,
                         fun=mean,
                         na.rm=T)
  

  #Render our leaflet map
  output$durm <- renderLeaflet({
    #Look familiar? Same map from leaflet tutorial

    #Generate leaflet map
    leaflet() %>% 
      addProviderTiles('CartoDB.Positron', group='Streets') %>%
      addProviderTiles('Esri.WorldImagery',group='Imagery') %>%
      addLayersControl(baseGroups=c('Streets','Imagery'),
                       options = layersControlOptions(collapsed = F, autoZIndex =
                                                        T)) %>% 
      addPolygons(data=neighb,
                  color=neighb.col(neighb$id),
                  popup=as.character(neighb$id),
                  layerId=neighb$id) %>% 
      addLegend(position='topleft',
                values=neighb$id,
                pal=neighb.col) %>%
      setView(lat=35.9940, lng= -78.8986,zoom=12) 
  })
  
  #Plot crime rate by neighborhood. 
  output$durm.crime <- renderPlot({

    xaxt <- input$xaxis
    #Plot data
    as.data.frame(neighb) %>%
    ggplot(aes_string(x=xaxt,y='rate')) + geom_point()
    
  })
  
  output$neighb.crime <- renderPlot({
    #Ask user to click on a neighborhood
    validate(
      need(
        input$durm_shape_click != "",
        "Please select a neighborhood to look at data from individual neighborhoods"
      )
    )
    #Extract neighborhood id from user click
    sub <- as.numeric(input$durm_shape_click$id)
    #Subset data to be only small neighborhood
    sub.neighb <- neighb[neighb$id == sub,]
    #Subset full crime dataset to be only from individual neighborhood
    #Really nice method here, that just asks for the full crime data to be
    #clipped to the dat shapefile
    sub.crime <- crime[sub.neighb,]
    print(unique(sub.crime$monthstamp))
    com.crime <- row.names(sort(table(crime$chrgdesc),decreasing=T)[1:15])
    #Plot data
    as.data.frame(sub.crime) %>%
      #Filter out all but the top 15 most common crimes
      filter(chrgdesc %in% com.crime) %>% 
      #Make months plot in order by giving them a factor
      mutate(month=factor(monthstamp,levels=c(as.character(1:11)))) %>%
      #Plot a stacked crime bar
      ggplot(aes(x=month,fill=chrgdesc)) + geom_bar()
  })
})



