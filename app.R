library(shiny)
library(ggplot2)
library(scales)
library(DT)
library(dplyr)
library(leaflet)
library(sf)
library(magrittr)
library(geojsonio)
library(htmltools)
library(htmlwidgets)
library(stringi)
library(RColorBrewer)


source('global.R', local = T)

source('server.R')
source('ui.R')


shinyApp(ui, server)