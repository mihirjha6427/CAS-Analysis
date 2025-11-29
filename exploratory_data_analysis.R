


source("main.R")

## plotting crash by year
plot_line_chart(
  data =crash_by_year%>%filter(crashYear<2025),
  x=crashYear,
  y=count,
  title="Annual Trend in Reported Crashes",
  xlab = "Year"
  )


## plotting crash by year and severity
plot_data<- cas_data %>% filter(crashYear < 2025,crashYear>=2014,region=="Auckland") %>%
  mutate(crashSeverity= ifelse(crashSeverity!= 'Non-Injury Crash','Crash resulting in injury',crashSeverity))%>%
  group_by(crashYear, crashSeverity) %>% 
  summarise(count = n_distinct(OBJECTID))

plot_data$crashSeverity <- factor(
  plot_data$crashSeverity,
  levels = c("Non-Injury Crash","Crash resulting in injury")
)



p<-plot_line_chart(
  data = plot_data,
  x=crashYear,
  y=count,
  title = "Annual Trend in Reported Crashes by Severity - Auckland",
  xlab = "Year"
  
)

p +
scale_x_continuous(breaks = unique(plot_data$crashYear))+
facet_wrap(~crashSeverity,ncol =1,scales ="free_y") 





### exploring the crash by holidays
cas_data %>% group_by(holiday) %>% summarise(count = n_distinct(OBJECTID))


######### Plotting by speed limits
cas_data %>%
  ggplot(aes(x = speedLimit, color = crashSeverity)) +
  geom_freqpoly(binwidth = 5, size = 1.2) +
  theme_minimal() +
  labs(title = "Distribution of Crash Speed Limits", x = "Speed Limit", y = "Count")

## roadways/crash location and injuries count
crash_location <- cas_data %>% group_by(crashLocation1) %>% 
  summarise(count =n_distinct(OBJECTID)) %>% 
  ungroup()

top_n_points <- crash_location %>% 
  mutate(perc_total = round(count * 100 / sum(count),0)) %>%
  top_n(10, count)


crash_location %>% 
  ggplot(
    aes(x = crashLocation1, y = count)) + 
  geom_point(color = "darkblue", size = 3) + 
  geom_text(
    data = top_n_points,
    aes(label = paste0(crashLocation1,"(",perc_total,"%)")),
    nudge_x = 1000,
    nudge_y = 1000
  ) +
  labs(title = "Scatter Plot by Crash Location (Location1)", x = "Crash Location", y = "Count") +
  theme_minimal() +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_blank())





############# Are there any particular meshblock or area where the crash stands out over the years
crash_by_meshblock <- cas_data %>% group_by(meshblockId, crashYear) %>% summarise(count = n_distinct(OBJECTID)) %>%
  arrange(desc(count)) %>% pivot_wider(names_from = crashYear, values_from = count) %>%
  replace(is.na(.), 0)
##Meshblock ID 423600 stands out and has the highest crash across all years

## we should also look at certain streets or areas where the crashes are happening the most?
crash_by_location <- cas_data %>% group_by(crashLocation1, crashYear) %>%
  summarise(count = n_distinct(OBJECTID)) %>% pivot_wider(names_from = crashYear, values_from = count) %>%
  replace(is.na(.), 0)

## scatter plot of meshblocks and count of injuries
top_n_points <- cas_data %>% group_by(meshblockId) %>% summarise(count =
                                                                   n_distinct(OBJECTID)) %>% top_n(20, count)

cas_data %>% group_by(meshblockId) %>% summarise(count = n_distinct(OBJECTID)) %>%
  ggplot(aes(x = meshblockId, y = count)) + geom_point(color = "darkblue", size = 3) +
  geom_text(
    data = top_n_points,
    aes(label = meshblockId),
    nudge_x = 1000,
    nudge_y = 20
  ) +
  labs(title = "Scatter Plot Example", x = "X Axis Label", y = "Y Axis Label") +
  theme_minimal()





######## Remove files
rm(crash_by_location,crash_by_meshblock,crash_by_year,crash_location,top_n_points)