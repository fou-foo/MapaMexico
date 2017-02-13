setwd('/home/fou/Desktop/wizeline/MapitaMex')
library(rgdal)
library(leaflet)
library(dplyr)
library(readr)
tmp <- tempdir()
url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)
mexico <- readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")
save(mexico, file ='/home/fou/Desktop/wizeline/MapitaMex/Datalab2/mexico.Rdata')

archivo <-'all_data.csv'
lee <- function(index)
{
	data <- read_csv(archivo, n_max =  100000, skip = index, col_names =FALSE)
	#return(data[, c('X1', 'X2', 'X6', 'X12', 'X13', 'X14', 'X15' )])
	return(data)
	#data <- read.csv(archivo, nrows = 500, skip = index, header = FALSE )
	#return (data[, c('V1', 'V2', 'V6', 'V12', 'V13', 'V14', 'V15' )])
	
}

#t <- Sys.time()
#a <- lee(30000000)
#t <- t -Sys.time()


index <- seq(from = 1, to = 3000000, by = 500000)
t <- Sys.time()
lectura <- lapply(  index, lee)
t <- t - Sys.time()
library(data.table)
dats <- rbindlist(lectura)
nombres <- names(dats)
nombres[6:7] <- c("lat", "lng")
nombres[c(3,4)] <- c("Precio", "Estado")
names(dats) <- nombres
serie <- dats
save(serie,file =  'serie.Rdata')

cohort <- dats %>% select(Estado,Precio, lat, lng) %>% group_by(Estado) %>% 
	summarize(canasta = mean(Precio, na.rm = TRUE), lat = mean(lat, na.rm =TRUE), lng = mean(lng, , na.rm =TRUE)) %>% arrange(Estado)
cohort <- na.omit(cohort)
x <- mexico@data
x$name <- as.character(x$name)
index <-grep('^[" "a-zA-Z]+$',  x$name )
x$name[index] <- toupper(x$name[index])
x <- merge( x, cohort, all.x = TRUE, by.x = "name", by.y ="Estado")

mexico@data  <- x
iconos <- makeIcon(iconUrl = 'https://www.mexicanbusinessweb.mx/wp-content/uploads/2015/11/canasta-b%C3%A1sica.jpg',
				   iconWidth = 31*215/(230), iconHeight = 31,
				   iconAnchorX = 31*215/(230*2), iconAnchorY = 16)

marcas <- leaflet(data = mexico) %>%  addProviderTiles("Stamen.Toner") %>%
	addCircleMarkers(data = cohort, radius = ~ log(cohort$canasta)*3,
					 color = c("purple"), stroke = TRUE, fillOpacity = 0.5 ) %>%
	addMarkers( data = cohort, icon = iconos, clusterOptions = markerClusterOptions())	
marcas
#######
pal <- colorQuantile('Purples', NULL, n = 10)
state_popup <- paste0("<strong>Estado: </strong>", 
						   mexico$name, 
						   "<br> <strong> Precio canasta basica </strong>", 
						   round(mexico$canasta, 2))

salida <- leaflet(data = mexico) %>%
	addProviderTiles("CartoDB.Positron") %>%
	addPolygons(fillColor = ~pal(canasta), 
				fillOpacity = 0.8, 
				color = "#BDBDC3", 
				weight = 1, 
				popup = state_popup)
salida




