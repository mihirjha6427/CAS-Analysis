
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
data_by_severity<- data %>%
  mutate(crashSeverity = ifelse(crashSeverity!="Non-Injury Crash","Injury Crash",crashSeverity)) %>%
  group_by(X_round,Y_round,crashSeverity)%>%summarise(count = n_distinct(OBJECTID))%>%
  ungroup()

p1<- coordinates_plot(
  data=data_by_severity%>%filter(crashSeverity=="Non-Injury Crash"),
  title = "Non-Injury crash count - 10 km grid",
  top_n = 9
  )


p2<- coordinates_plot(
  data=data_by_severity%>%filter(crashSeverity!="Non-Injury Crash"),
  title = "Crash resulting in injury - 10 km grid",
  top_n = 9
  )

p1 + p2



## getting coordinates
top_9_hotspots <- data_summarized %>% 
  mutate(perc_total = count * 100 /sum(count))%>%
  top_n(9,count)%>%arrange(desc(count))

top_9_hotspots$hotspots<-1:9

## inner joining with data
hotspots_data<- data%>%
  inner_join(
    top_9_hotspots,
    by=c(
      "X_round"="X_round",
      "Y_round"="Y_round"
      )
    )

## cleaning and replacing the data
hotspots_data<-hotspots_data%>%
  mutate(
    crashSeverity= ifelse(crashSeverity!="Non-Injury Crash","Injury-related","Non-Injury"),
    X=round(X,-1),
    Y=round(Y,-1)
    )%>%
  group_by(X,Y,crashSeverity)%>%
  summarise(count = n_distinct(OBJECTID))%>%ungroup()

hotspots_data_sf = hotspots_data%>%st_as_sf(coords = c("X","Y"),crs=2193)%>%
  st_transform(4326)

pal<-colorFactor(
  palette = c("darkgreen","darkred"),
  domain = c("Non-Injury","Injury-related")
)

leaflet()%>%
  addTiles()%>% 
  addCircleMarkers(
  data=hotspots_data_sf,
  radius = ~ rescale(count, to = c(3,20)),
  label = ~count,
  color = ~pal(crashSeverity),
  fillOpacity = 0.6,
  stroke = FALSE
  )





## converting it into simple features (sf) for further analysis
##  creating a leaflet map of all the injury related crashed where the aggegated crash
## counts within 10m was greater than 5
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
