pdf(file="passhe.pdf",paper="USr",height=0, width=0)
library(tidyverse)
library(readxl)
library(foreign)
library(ggplot2)
library(sp)
library(rgdal)
## library(rgeos)
library(gpclib)
library(maptools)
gpclibPermit()
library(scales)
library(dplyr)
library(stringi)  ## for padding numeric
library(gtools) ## for smartbind
options(scipen=10000,width=200,tibble.print_max=40)



## the location data for the PASSHE universities
ipeds  <- readRDS("ipeds-passhe.RData")
ipeds  <- ipeds %>% rename(STUSAB=STATE)
ipeds  <- ipeds %>% mutate(name=gsub(" of P.*","",NAME))

(nms <- names(read_excel("cz00_eqv_v1.xls", sheet=1, n_max = 0)))
(ct <- ifelse(grepl("Population", nms), "guess", "text"))
cz <- read_excel("cz00_eqv_v1.xls", sheet=1, col_types = ct )
cz  <- cz %>% mutate(STATE = substr(FIPS,1,2),
                     COUNTY = substr(FIPS,3,5),
                     county = paste(FIPS, `County Name`))

czpa  <- cz %>% filter(STATE=="42")

passhe  <- left_join(x=ipeds, y=czpa, by=c("CNTY"="FIPS") )

## Find the list of all counties in a commuting zone with a university
temp  <- left_join(x=passhe, y=czpa, by="Commuting Zone ID, 2000", suffix=c(".x",""))
temp %>% group_by(county) %>% summarize(CZ=first(`Commuting Zone ID, 2000`), passhe=paste(name, collapse=", ")) %>% arrange(county)
## Find the list of Commuting zones with the universities and counties attached.
temp %>% group_by(CZ=`Commuting Zone ID, 2000`) %>% summarize(passhe=paste(unique(name), collapse=", "), county=paste(unique(county), collapse=", "))



## Employee counts from the PASSHE Data Workbook
wsMap  <- read_csv("wsMap-density/wsMap-density.csv")
## wsMap  <- read_csv("wsMap-density.csv")
wsMap %>% filter(`Measure for Color` > 0.8) %>% 
    select(`Geo Name2`,`TT Selected Measure, Mode, Univ`, `TT Measure Count when Density Selected`,`Measure for Color`)
test  <- wsMap %>% filter(!(`TT Selected Measure, Mode, Univ` %in% c("State System Employee Density","Office of the Chancellor Employee Density") )) %>%
    group_by(`TT Selected Measure, Mode, Univ`) %>%
    transmute(
        County = `Geo Name2`,
        university = gsub(" Employee Density", "",`TT Selected Measure, Mode, Univ`),
        count = `TT Measure Count when Density Selected`,
        density = `Measure for Color`/100,
        share = count / sum(count)
    )
test  <- test %>% arrange(university, desc(share)) %>% mutate(runshare = cumsum(share))
pt1  <- test %>% filter(runshare<0.8)
pt2  <- test %>% filter(runshare>=0.8) %>% filter(row_number()==1)
eightypercent  <- bind_rows(pt1,pt2) %>% arrange(university, desc(share))
twentyemployees  <- test %>% filter(count>=20)
criterion  <- eightypercent

(university.counties <- criterion %>% group_by(university) %>% summarize(`Counties accounting for 80% of Employment` = paste(County,collapse=", ") ))
(county.universities <- criterion %>% group_by(County) %>% summarize(`Universities associated with County` = paste(university,collapse=", ") ))




## create a layer name for the shapefile (filename before .shp/.shx/.dbf extension)
layerName <- "co42_d00"

## read data into a SpatialPolygonsDataFrame object
paCounties.spdf <- readOGR(dsn="/home/mash/Desktop/pa-apscuf-krc/geog/co42_d00_shp/", layer=layerName)

## add to the database portion of the spdf object
## a new column termed "id" composed of the rownames of data
## two new columns for latitude and longitude of centroid ("internal point")
paCounties.spdf@data$id <- rownames(paCounties.spdf@data)
paCounties.spdf@data$intptlon <- coordinates(paCounties.spdf)[,1]
paCounties.spdf@data$intptlat <- coordinates(paCounties.spdf)[,2]

paCounties.spdf@data <- left_join(paCounties.spdf@data, cz, by=c("STATE"="STATE","COUNTY"="COUNTY"))

paCounties.spdf@data <- left_join(paCounties.spdf@data, county.universities, by=c("County Name"="County"))

paCounties.spdf@data <- mutate(paCounties.spdf@data,
                                      `Bloomsburg University` = ifelse(grepl("Bloomsburg University", `Universities associated with County`), "maroon", NA),
                                      `California University` = ifelse(grepl("California University", `Universities associated with County`), "red", NA),
                                         `Cheyney University` = ifelse(grepl("Cheyney University", `Universities associated with County`), "royalblue", NA),
                                         `Clarion University` = ifelse(grepl("Clarion University", `Universities associated with County`), "blue", NA),
                                `East Stroudsburg University` = ifelse(grepl("East Stroudsburg University", `Universities associated with County`), "red", NA),
                                        `Edinboro University` = ifelse(grepl("Edinboro University", `Universities associated with County`), "red", NA),
                                         `Indiana University` = ifelse(grepl("Indiana University", `Universities associated with County`), "darkgray", NA),
                                        `Kutztown University` = ifelse(grepl("Kutztown University", `Universities associated with County`), "maroon", NA),
                                      `Lock Haven University` = ifelse(grepl("Lock Haven University", `Universities associated with County`), "red", NA),
                                       `Mansfield University` = ifelse(grepl("Mansfield University", `Universities associated with County`), "darkgray", NA),
                                    `Millersville University` = ifelse(grepl("Millersville University", `Universities associated with County`), "gold", NA),
                                    `Shippensburg University` = ifelse(grepl("Shippensburg University", `Universities associated with County`), "blue", NA),
                                   `Slippery Rock University` = ifelse(grepl("Slippery Rock University", `Universities associated with County`), "green", NA),
                                    `West Chester University` = ifelse(grepl("West Chester University", `Universities associated with County`), "purple", NA),
                               )

## create a data.frame from the spatial object
paCounties.df <- fortify(paCounties.spdf, region = "id")

## merge the "fortified" geographic data with the database portion of the spatial object
paCounties.df <- merge(paCounties.df, paCounties.spdf@data, by = "id")

paCounties.df  <- paCounties.df[order(paCounties.df$order),]



## ggplot Counties and Commuting Zones and place the universities
paCounties.gg <- ggplot(data = paCounties.df, aes(x=long, y=lat)) +
    geom_polygon(aes(group = COUNTY, fill=`Commuting Zone ID, 2000`),color="gray") +
    geom_text(data=paCounties.spdf@data, aes(x=intptlon, y=intptlat,label=NAME), color="gray") +
    geom_point(data=passhe, aes(x=LON,y=LAT), show.legend=FALSE) +
    geom_text(data=passhe, aes(x=LON,y=LAT,label=name), hjust="left", nudge_x=0.02) +
    theme(legend.position="none")

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
print(paCounties.gg + coord_map("conic", lat0 = 30))


## ggplot Counties and University Clusters and place the universities
paCounties.gg <- ggplot(data = paCounties.df, aes(x=long, y=lat)) +
    geom_polygon(aes(group = COUNTY, fill=NA),color="gray") +
    geom_polygon(aes(group = COUNTY, fill=`Bloomsburg University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`California University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Cheyney University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Clarion University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`East Stroudsburg University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Edinboro University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Indiana University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Kutztown University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Lock Haven University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Mansfield University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Millersville University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Shippensburg University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`Slippery Rock University`),color="gray", alpha=0.5) +
    geom_polygon(aes(group = COUNTY, fill=`West Chester University`),color="gray", alpha=0.5) +
    scale_fill_identity() +
    geom_text(data=paCounties.spdf@data, aes(x=intptlon, y=intptlat,label=NAME), color="gray") +
    geom_point(data=passhe, aes(x=LON,y=LAT), show.legend=FALSE) +
    geom_text(data=passhe, aes(x=LON,y=LAT,label=name), hjust="left", nudge_x=0.02) +
    theme(legend.position="none")
print(paCounties.gg + coord_map("conic", lat0 = 30))


## ggplot Counties and One University Cluster and place the university
for (univ in c("`Bloomsburg University`","`California University`","`Cheyney University`","`Clarion University`","`East Stroudsburg University`","`Edinboro University`","`Indiana University`","`Kutztown University`","`Lock Haven University`","`Mansfield University`","`Millersville University`","`Shippensburg University`","`Slippery Rock University`","`West Chester University`")) {
    univ_name  <- gsub("`","",univ)
paCounties.gg <- ggplot(data = paCounties.df, aes(x=long, y=lat)) +
    geom_polygon(aes(group = COUNTY, fill=eval(parse(text=univ))),color="gray") +
    scale_fill_identity() +
    geom_text(data=paCounties.spdf@data, aes(x=intptlon, y=intptlat,label=NAME), color="gray") +
    geom_point(data=filter(passhe,name==univ_name), aes(x=LON,y=LAT), show.legend=FALSE) +
    geom_text(data=filter(passhe,name==univ_name), aes(x=LON,y=LAT,label=name), hjust="left", nudge_x=0.02) +
    theme(legend.position="none")
print(paCounties.gg + coord_map("conic", lat0 = 30))
}







## State Legislative Districts Upper (Senate)
## create a layer name for the shapefile (filename before .shp/.shx/.dbf extension)
layerName <- "cb_2018_42_sldu_500k"

## read data into a SpatialPolygonsDataFrame object
paSLDU.spdf <- readOGR(dsn="/home/mash/Desktop/pa-apscuf-krc/geog/cb_2018_42_sldu_500k/", layer=layerName)

## add to the database portion of the spdf object
## a new column termed "id" composed of the rownames of data
## two new columns for latitude and longitude of centroid ("internal point")
paSLDU.spdf@data$id <- rownames(paSLDU.spdf@data)
paSLDU.spdf@data$intptlon <- coordinates(paSLDU.spdf)[,1]
paSLDU.spdf@data$intptlat <- coordinates(paSLDU.spdf)[,2]

## create a data.frame from the spatial object
paSLDU.df <- fortify(paSLDU.spdf, region = "id")

## merge the "fortified" geographic data with the database portion of the spatial object
paSLDU.df <- merge(paSLDU.df, paSLDU.spdf@data, by = "id")

paSLDU.df  <- paSLDU.df[order(paSLDU.df$order),]


## ggplot and place the universities
paSLDU.gg <- ggplot(data = paSLDU.df, aes(x=long, y=lat)) +
    geom_polygon(aes(group = NAME, fill=`NAME`),color="gray") +
    geom_text(data=paSLDU.spdf@data, aes(x=intptlon, y=intptlat, label=NAME), color="gray") +
    geom_point(data=passhe, aes(x=LON,y=LAT), show.legend=FALSE) +
    geom_text(data=passhe, aes(x=LON,y=LAT,label=name), hjust="left", nudge_x=0.02) +
##    scale_fill_viridis_d() +
    theme(legend.position="none")

print(paSLDU.gg + coord_map("conic", lat0 = 30))









## ## US Congressional Districts
## ## create a layer name for the shapefile (filename before .shp/.shx/.dbf extension)
## layerName <- "cb_2018_us_cd116_20m"

## ## read data into a SpatialPolygonsDataFrame object
## paCD.spdf <- readOGR(dsn="/home/mash/Desktop/pa-apscuf-krc/geog/cb_2018_us_cd116_20m/", layer=layerName)

## ## add to the database portion of the spdf object
## ## a new column termed "id" composed of the rownames of data
## ## two new columns for latitude and longitude of centroid ("internal point")
## paCD.spdf@data$id <- rownames(paCD.spdf@data)
## paCD.spdf@data$intptlon <- coordinates(paCD.spdf)[,1]
## paCD.spdf@data$intptlat <- coordinates(paCD.spdf)[,2]

## ## create a data.frame from the spatial object
## paCD.df <- fortify(paCD.spdf, region = "id")

## ## merge the "fortified" geographic data with the database portion of the spatial object
## paCD.df <- merge(paCD.df, paCD.spdf@data, by = "id")

## paCD.df <- paCD.df %>% filter(STATEFP==42)

## paCD.df  <- paCD.df[order(paCD.df$order),]



## ## ggplot and place the universities
## paCD.gg <- ggplot(data = paCD.df, aes(x=long, y=lat)) +
##     geom_polygon(aes(group=CD116FP, fill=CD116FP),color="gray") +
##     geom_text(data=filter(paCD.spdf@data,STATEFP=="42"), aes(x=intptlon, y=intptlat, label=CD116FP), color="gray") +
##     geom_point(data=passhe, aes(x=LON,y=LAT)) +
##     geom_text(data=passhe, aes(x=LON,y=LAT,label=name), hjust="left", nudge_x=0.02) +
## ##    scale_fill_viridis_d() +
##     theme(legend.position="none")

## print(paCD.gg + coord_map("conic", lat0 = 30))
