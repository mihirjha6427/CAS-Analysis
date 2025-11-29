
### Loading packages
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(sf)
library(leaflet)

options(scipen = 999)

## loading the plot function
source("plot_function.R")


# loading data set
cas_data <- read.csv("CAS_data.csv",stringsAsFactors = F)


## Reading shape files
region_shape = st_read("./statsnz-regional-council-2025-SHP/regional-council-2025.shp")


# loading GeoJSON file
#nz_regions <- st_read("CAS.geojson")


## Sanity checks

## checking if OBJECTIDs are unique for each observation 
nrow(cas_data) == n_distinct(cas_data$OBJECTID)


# checking counts by year and if sum is same as total
crash_by_year<-cas_data%>%group_by(crashYear)%>%summarise(count= n_distinct(OBJECTID))

nrow(cas_data) == sum(crash_by_year$count)

## Are there any missing values in the cas data specially for regional analysis
cas_data%>%select(X,Y,region,tlaName)%>%summarise(across(everything(), ~sum(is.na(.) | str_trim(.) =="")))
region_lookup<-cas_data%>%filter(region!="", tlaName!="")%>%distinct(region,tlaName)





###using region_shape file to clean the region data

cas_data_sf = cas_data%>%st_as_sf(coords = c("X","Y"),crs = 2193,remove = FALSE)%>%
  st_transform(4326)

#Joining it with nz_region
cas_data_sf<- st_join(cas_data_sf,region_shape%>%select(REGC2025_1))

cas_data_sf%>%st_drop_geometry()%>%
  group_by(region,REGC2025_1)%>%
  summarise(
    count = n_distinct(OBJECTID)
    )%>%arrange(desc(count))


cas_data <- cas_data_sf%>%st_drop_geometry()%>%select(-region)%>%rename("region"="REGC2025_1")




### remove df
rm(cas_data_sf)
