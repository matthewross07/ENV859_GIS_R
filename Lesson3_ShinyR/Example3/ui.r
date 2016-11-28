library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  # Application title.
  titlePanel("Durham Crime"),
  
  #Sidebary layout style
  sidebarLayout(
    sidebarPanel(width=4,
      leafletOutput('durm',height='700px')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Overall Crime Trends by Neighborhood',
                 selectInput('xaxis','Choose X axis',
                             choices=c('TRACT','BLKGRP','id'),selected='id'),
                 plotOutput('durm.crime')),
        tabPanel('Crime Trends Within a Neighborhood',
                 plotOutput('neighb.crime'))
      )
    )
  )
))