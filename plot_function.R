

plot_line_chart<- function(data,x,y,grouping=NULL, title,xlab,ylab="Count"){
  
  p<-ggplot(data, aes(x={{x}},y={{y}})) 
  
  
  if(missing(grouping)){
   p<-p + geom_line(color = "steelblue",size=1.2) 
  } else{
    p<-p + geom_line(aes(color = {{grouping}}),size=1.2)
  }
  
  p<- p + geom_point(color="darkred",size=2) +
    geom_text(
      aes(label = comma({{y}})),
      size= 3,
      vjust = -0.5,
      nudge_y = 9
    )+
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0.05, 0.20))) +
    theme_minimal() +
    labs(title = title,
         x = xlab,
         y ="Count")
  
  p
  
}

# Scatter plot function 
#EDA - just using eastings and northings to see the distribution
coordinates_plot <-function(data, title="crash count",top_n = Null){
  


plot<-ggplot(data, aes(x = X_round, y = Y_round)) +
  geom_point(
    aes(size = count,
        alpha = count,
        color = count)
  )+
  scale_size(range = c(2, 15)) +
  scale_alpha(range = c(0.5, 1.0)) +
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    
  ) + 
  labs(
    title = title,
    x = "Eastings",
    y = "Northings",
    size = "Crash Count",
    alpha = "Crash Count",
    color = "Crash Count"
  )

if(is.null(top_n)){
  plot
} else {
  top_points <- data%>%
    top_n(9,count)
  plot +
    geom_label_repel(
      data = top_points,
      aes(label = count),
      force = 2,
      max.overlaps = Inf,
      box.padding = 0.5,
      point.padding = 0.3,
      segment.color = "black",
      segment.size = 0.3,
      color = "black",
      size = 3
    ) 
}

}


## leaflet plot function
plot_leaflet<- function(data){
  
  leaflet() %>% addTiles() %>% addCircleMarkers(data = map_data,
                                              radius = ~ rescale(count, to = c(3, 15)),
                                              label = ~ count,
                                              
)

}  





