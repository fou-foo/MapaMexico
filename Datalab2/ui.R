
library(shiny)
library(leaflet)
library(ggvis)
shinyUI(fluidPage(
  
  h6("Precio de canasta basica por estado"),

      mainPanel(
      	leafletOutput("Marcas"),
      	leafletOutput('Scala'), 
      	leafletOutput('Experimentar'),
      	ggvisOutput('serie')
      	    )
  	)
)