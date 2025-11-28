
source("main.R")

### let's look at the eastings and northings
cas_data %>% group_by(X, Y) %>% summarise(count = n_distinct(OBJECTID)) %>%
  ungroup() %>% filter(count > 0) %>% ggplot(aes(x = X, y = Y)) + geom_point(aes(size = count), color =
                                                                               "darkred", alpha = 0.2) +
  scale_size(range = c(2, 15)) + theme_minimal() + labs(title = "Bubble plot by crash count",
                                                        x = "X Coordinate",
                                                        y = "Y Coordinate",
                                                        size = "Crash Count")



#### let's group the eastings and northings to 10km grid to see a picture
# let's create a plot_function

data <- cas_data %>% mutate(X_round = round(X, -4), Y_round = round(Y, -4))

data_summarized <- data %>% group_by(X_round, Y_round) %>%
  summarise(count = n_distinct(OBJECTID)) %>%
  ungroup()


###### plotting the top 9 hotspots
coordinates_plot(
    data_summarized,
    title = "crash count - 10 km grid",
    top_n = 9
  )



## let's break it down by crash severity
data_by_severity<- data %>%mutate(crashSeverity = ifelse(crashSeverity!="Non-Injury Crash","Injury Crash",crashSeverity)) %>%
  group_by(X_round,Y_round,crashSeverity)%>%summarise(count = n_distinct(OBJECTID))%>%
  ungroup()

p1<- coordinates_plot(data=data_by_severity%>%filter(crashSeverity=="Non-Injury Crash"),title = "Non-Injury crash count - 10 km grid",top_n = 9)
p2<- coordinates_plot(data=data_by_severity%>%filter(crashSeverity!="Non-Injury Crash"),title = "Crash resulting in injury - 10 km grid",top_n = 9)

p1 + p2



## getting coordinates
top_9_hotspots <- data_summarized %>% mutate(perc_total = count * 100 /
                                               sum(count))%>%top_n(9,count)%>%arrange(desc(count))

top_9_hotspots$hotspots<-1:9

## inner joining with data
hotspots_data<- data%>%inner_join(top_9_hotspots,by=c("X_round"="X_round","Y_round"="Y_round"))

## cleaning and replacing the data
hotspots_data<- hotspots_data%>%mutate(tlaName = ifelse(tlaName=="","Auckland",tlaName))

## checking
hotspots_data%>%group_by(hotspots,tlaName)%>%summarise(count = n_distinct(OBJECTID))

## replacing waikato district with Hamilton city
hotspots_data<- hotspots_data%>%mutate(tlaName = ifelse(tlaName=="Waikato District","Hamilton City",tlaName))

hotspots_data<-hotspots_data%>%unite("hotspot_city",hotspots,tlaName)

## plotting time series for checking
hotspots_data %>% filter(crashYear<=2024,crashSeverity!='Non-Injury Crash')%>% group_by(hotspot_city,crashYear) %>% summarise(count = n_distinct(OBJECTID)) %>%
  mutate(smoothed_count = round((lag(count,1)+count + lead(count,1))/3,0),
         smoothed_count = if_else(is.na(smoothed_count),count,smoothed_count))%>%
  ggplot(aes(x = crashYear, y = smoothed_count )) +
  geom_line(aes(color = hotspot_city),size = 1.2) + geom_point(color='darkred',size = 2) + geom_text(
    aes(label = smoothed_count),
    size = 2,
    vjust = -1,
    nudge_y = 5
  ) + theme_minimal()+ labs(title = "Hotspot time-series analysis (Smoothened)",
                            x = "year") + ylab("Smoothened Count")






### Plotting recent years hotspot

data_by_severity<- data%>%filter(crashYear >2022) %>%mutate(crashSeverity = ifelse(crashSeverity!="Non-Injury Crash","Injury Crash",crashSeverity)) %>%
  group_by(X_round,Y_round,crashSeverity)%>%summarise(count = n_distinct(OBJECTID))%>%
  ungroup()

p1<- coordinates_plot(data=data_by_severity%>%filter(crashSeverity=="Non-Injury Crash"),title = "Crash with no injury,Year 2023-2025,10 km grid",top_n = 9)
p2<- coordinates_plot(data=data_by_severity%>%filter(crashSeverity!="Non-Injury Crash"),title = "Crash resulting in injury,Year 2023-2025,10 km grid",top_n = 9)

p1 + p2




## converting it into simple features (sf) for further analysis

data_sf = cas_data%>%mutate(X=round(X,-1),Y=round(Y,-1))%>%st_as_sf(coords = c("X","Y"),crs = 2193,remove = FALSE)%>%
  st_transform(4326)

##Looking at the recurring crashes which resulted in injury
map_data <- data_sf %>% filter(crashSeverity != "Non-Injury Crash") %>% group_by(geometry) %>%
  summarise(count = n_distinct(OBJECTID)) %>% filter(count > 5)

leaflet() %>% addTiles() %>% addCircleMarkers(data = map_data,
                                              radius = ~ rescale(count, to = c(3, 15)),
                                              label = ~ count)


## Looking at where fatal crahses occured
map_data <- data_sf %>% filter(crashSeverity == "Fatal Crash") %>% group_by(geometry) %>%
  summarise(count = n_distinct(OBJECTID))%>%filter(count>1) 

leaflet() %>% addTiles() %>% addCircleMarkers(data = map_data,
                                              radius = ~ rescale(count, to = c(3, 15)),
                                              label = ~ count,
                                              
)
