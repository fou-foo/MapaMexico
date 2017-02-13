library(shiny)
library(ggvis)
library(lubridate)
library(dplyr)

# recoleccion del mapa - en un futura hay que saber de donde bajar el mapa- 
#tmp <- tempdir()
#url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"
#file <- basename(url)
#download.file(url, file)
#unzip(file, exdir = tmp)
#mexico <- readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")
load(file =  'dats40.Rdata')
load(file = 'serie.Rdata')
#load(file = 'mexico.Rdata')
load(file = 'mexicoprocesado.Rdata')
cohort <- dats %>% select(Estado,Precio, lat, lng) %>% group_by(Estado) %>% 
 	summarize(canasta = mean(Precio, na.rm = TRUE), lat = mean(lat, na.rm =TRUE), lng = mean(lng, , na.rm =TRUE)) %>% arrange(Estado)
 cohort <- na.omit(cohort)
# x <- mexico@data
# x$name <- as.character(x$name)
# index <-grep('^[" "a-zA-Z]+$',  x$name )
# x$name[index] <- toupper(x$name[index])
# x <- merge( x, cohort, all.x = TRUE, by.x = "name", by.y ="Estado")
# 
# mexico@data  <- x
# save(mexico, file='mexicoprocesado.Rdata')
iconos <- makeIcon(iconUrl = 'https://www.mexicanbusinessweb.mx/wp-content/uploads/2015/11/canasta-b%C3%A1sica.jpg',
				   iconWidth = 31*215/(230), iconHeight = 31,
				   iconAnchorX = 31*215/(230*2), iconAnchorY = 16)
pal <- colorQuantile('Purples', NULL, n = 10)
state_popup <- paste0("<strong>Estado: </strong>", 
					  mexico$name, 
					  "<br> <strong> Precio canasta basica </strong>", 
					  round(mexico$canasta, 2))

# se recolectan los datos para la serie 
data <- serie 
data$fecha <- ymd(data$X7)
data$precio <- data$X6 
data$Estado <- data$X12
y <- data %>% group_by( Estado, fecha) %>% summarise(precioCanasta = sum(precio, na.rm =TRUE)) 

vis_names <- function(x)
{
	if(is.null(x)) return(NULL)
	paste0('precio Canasta: ',round(x['precioCanasta'],2), '<br /> fecha: ',round(x['fecha'],2),
		   collapse="<br />")
	#http://stackoverflow.com/questions/27992078/displaying-information-using-ggvis
}
point_obj <- y %>% ggvis( x= ~fecha, y = ~precioCanasta) %>%# , size = ~media, opacity = ~max ) %>% 
	layer_lines(stroke = ~ Estado) %>%    hide_legend("size") %>%
	add_axis("x", title = "Tiempo", title_offset = 50) %>%
	add_axis("y", title = "Precio", title_offset = 50) %>%
	add_axis("x", orient = "top",  title = "Costo Canasta en el tiempo",
			 properties = axis_props( axis = list(stroke = "white"),
			 						 labels = list(fontSize = 0)))


shinyServer(function(input, output) {
	
	output$Marcas <- renderLeaflet({
		leaflet(data = mexico) %>%  addProviderTiles("Stamen.Toner") %>%
			addCircleMarkers(data = cohort, radius = ~ log(cohort$canasta)*3,
							 color = c("green"), stroke = TRUE, fillOpacity = 0.5 ) %>%
			addMarkers(data = cohort, icon = iconos, clusterOptions = markerClusterOptions())	
		
	})
	output$Scala <- renderLeaflet({
		leaflet(data = mexico) %>%
			addProviderTiles("CartoDB.Positron") %>%
			addPolygons(fillColor = ~pal(canasta), 
						fillOpacity = 0.8, 
						color = "#BDBDC3", 
						weight = 1, 
						popup = state_popup)
	})
	output$Experimentar <- renderLeaflet({
		leaflet(data = mexico) %>%
			addProviderTiles("CartoDB.Positron") %>%
			addPolygons(fillColor = ~pal(canasta), 
						fillOpacity = 0.8, 
						color = "#BDBDC3", 
						weight = 1, 
						popup = state_popup) %>%
			addCircleMarkers(data = cohort, radius = ~ log(cohort$canasta)*3,
							 color = c("green"), stroke = TRUE, fillOpacity = 0.5 ) %>%
			addMarkers(data = cohort, icon = iconos, clusterOptions = markerClusterOptions())	
		
	})
	
	##
	point_obj %>% add_tooltip(vis_names,"hover") %>% 	bind_shiny("serie")
	
})
