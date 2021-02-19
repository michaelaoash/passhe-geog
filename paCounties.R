pdf(file="paCounties.pdf",width=10)
## load libraries
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(gpclib)
library(maptools)
gpclibPermit()
library(scales)
library(dplyr)
library(stringi)  ## for padding numeric
library(gtools) ## for smartbind

## the location data for the PASSHE universities


## create a layer name for the shapefile (filename before .shp/.shx/.dbf extension)
layerName <- "co42_d00"

## read data into a SpatialPolygonsDataFrame object
paCounties.spdf <- readOGR(dsn="/home/mash/Desktop/pa-apscuf-krc/co42_d00_shp/", layer=layerName)


## add to the database portion of the spdf object
## a new column termed "id" composed of the rownames of data
## two new columns for latitude and longitude of centroid ("internal point")
paCounties.spdf@data$id <- rownames(paCounties.spdf@data)
paCounties.spdf@data$intptlon <- coordinates(paCounties.spdf)[,1]
paCounties.spdf@data$intptlat <- coordinates(paCounties.spdf)[,2]

## create a data.frame from the spatial object
paCounties.df <- fortify(paCounties.spdf, region = "id")

## merge the "fortified" geographic data with the database portion of the spatial object
paCounties.df <- merge(paCounties.df, paCounties.spdf@data, by = "id")

paCounties.df  <- paCounties.df[order(paCounties.df$order),]

## ggplot
paCounties.gg <- ggplot(data = paCounties.df,
                       aes(x=long, y=lat, group = COUNTY, fill=as.numeric(COUNTY))) + geom_polygon()



## paCounties.gg <- ggplot(data = paCounties.df,
##                        aes(x=long, y=lat, group = COUNTY, fill=as.numeric(margin))) + geom_polygon() +
##                            geom_path(aes(color = as.numeric(Delegates))) +
##                                scale_fill_gradient2(midpoint=0, low=muted("red"), mid="white", high=muted("blue"), space ="Lab", name="Margin") +
##                                    scale_color_gradient2(midpoint=0.5, low="white", mid="gray", high="black", space ="Lab", name="Delegates (border)", guide="none") +
##                                        coord_equal()  + theme(legend.position = "bottom", axis.title = element_blank(),axis.text = element_blank()) +
##                                            geom_text(data=paCounties.spdf@data,aes(x=as.numeric(intptlon),y=as.numeric(intptlat),group=NULL,label=ALTNAME),size=1.2) 
                                               ## geom_text(data=junk,aes(x=as.numeric(intptlon),y=as.numeric(intptlat),group=NULL,label=ALTNAME),size=1.2) +
                                               ##     annotate(geom="text", label="Retirees and Higher Ed Units", x=-73.1,y=41.9,size=2)



##  Print with alternative projections
print(paCounties.gg)
print(paCounties.gg + coord_map("ortho"))
## print(paCounties.gg + coord_map("ortho", orientation=c(41, -74, 0)))
## print(paCounties.gg + coord_map("ortho", orientation=c(42, -73, 0)))
print(paCounties.gg + coord_map("stereographic"))
print(paCounties.gg + coord_map("conic", lat0 = 30))

