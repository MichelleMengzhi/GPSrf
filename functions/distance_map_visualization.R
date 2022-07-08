distance_map_visualization <- function(MetasubDataPreds, filter_level='all', png_path){
  # In this function:
  # 1. get the corresponding country and continent 
  # 2. determine if the predicted geographic coordinate is in the same ocontinent of the original geographic corrdinate 
  # 3. calculate the distance from the origin
  # 4. extract samples based on the filter_level
  # 5. calculate the portion of samples predicted to be in the same continent in continent domain, and assign the cooridnate to draw pie for each continent
  # 6. map visualization with pie chart
  
  # @MetasubDataPreds: a data frame containing origin latitud and longitude, origin country, origin continent, and predicted latitude and longitude columns
  # @filter_level: a string, 'all' or '500' or '1000'. 
  #               Default is 'all'. 
  #               'all' represents use all samples without filtration; 
  #               'notSameContinent500' means to remain samples having distance from the origin > 500km, and not in the same continent; 
  #               'notSameContinent1000' means to reamin samples having distance from the origin > 1000km, and not in the same continent；
  #               'notSameContinent' means to reamin samples having distance from the origin > 1000km, and not in the same continent
  # @png_path: a string of the path to store the visualization
  
  ##### 1. get the corresponding country and continent  #####
  MetasubDataPreds$countryPred <- MazamaSpatialUtils::getCountry(MetasubDataPreds$longPred, MetasubDataPreds$latPred)
  MetasubDataPreds$continentPred <- countrycode(sourcevar = MetasubDataPreds$countryPred,
                                                origin = "country.name",
                                                destination = "continent")
  
  # manually change the incorrect country assignment
  for(i in 1:nrow(MetasubDataPreds)){
    if((MetasubDataPreds$longPred[i]< -33 & MetasubDataPreds$longPred[i] > -76) &
       (MetasubDataPreds$latPred[i]< 11 & MetasubDataPreds$latPred[i] > -56)){
      MetasubDataPreds$continentPred[i] <- 'Americas'
    }
  }
  
  ##### 2. determine if the predicted geographic coordinate is in the same ocontinent of the original geographic corrdinate  #####
  MetasubDataPreds$sameContinent <- NA
  for(i in 1:nrow(MetasubDataPreds)){
    if(MetasubDataPreds$continent[i] %in% MetasubDataPreds$continentPred[i]){
      MetasubDataPreds$sameContinent[i] <- TRUE
    }else{
      MetasubDataPreds$sameContinent[i] <- FALSE
    }
  }
  
  
  ##### 3. calculate the distance from the origin #####
  for (i in 1:nrow(MetasubDataPreds)){
    MetasubDataPreds[i,"distance_from_origin"] <- geosphere::distm(c(MetasubDataPreds[i,"longPred"],MetasubDataPreds[i,"latPred"]), 
                                                                   c(MetasubDataPreds[i,"longitude"],MetasubDataPreds[i,"latitude"]), fun = geosphere::distHaversine)/1000
  }
  
  
  ##### 4. extract samples based on the filter_level#####
  unique(MetasubDataPreds$continent)
  if(filter_level == 'all'){
    df <- MetasubDataPreds %>%
      # filter(sameContinent == FALSE) %>%
      # filter(distance_from_origin > 500) %>%
      select(longitude,latitude,longPred, latPred,sameContinent, country)
  }else if(filter_level == 'notSameContinent500'){
    df <- MetasubDataPreds %>%
      filter(sameContinent == FALSE) %>%
      filter(distance_from_origin > 500) %>%
      select(longitude,latitude,longPred, latPred,sameContinent, country)
    
  }else if(filter_level == 'notSameContinent1000'){
    df <- MetasubDataPreds %>%
      filter(sameContinent == FALSE) %>%
      filter(distance_from_origin > 1000) %>%
      select(longitude,latitude,longPred, latPred,sameContinent, country)
    
  }else if(filter_level == 'notSameContinent'){
    df <- MetasubDataPreds %>%
      filter(sameContinent == FALSE) %>%
      select(longitude,latitude,longPred, latPred,sameContinent, country)
    
  }else{stop('Invalid filter_level input')}
  
  df$color <- NA
  for(i in 1:nrow(df)){
    if(df$sameContinent[i] == T){
      df$color[i] <- "Same continent"
    }else{
      df$color[i] <- "Different continent"
      
    }
  }
  # print(head(df))
  
  ##### 5. calculate the portion of samples predicted to be in the same continent in continent domain, and assign the cooridnate to draw pie for each continent #####
  # print(pie_continent)
  if(filter_level != 'all'){
    pie_continent <- data.frame(continent=c(unique(MetasubDataPreds$continent), 'Overall'),
                                latitude= NA, longitude = NA,
                                in_continent = NA, not_in_continent = NA)
    
    for(i in c(unique(MetasubDataPreds$continent), 'Overall')){
      if(i == 'Overall'){
        pie_continent$in_continent[which(pie_continent$continent == 'Overall')] <- nrow(MetasubDataPreds[which(MetasubDataPreds$sameContinent == TRUE),])/nrow(MetasubDataPreds)*100
        pie_continent$not_in_continent[which(pie_continent$continent == 'Overall')] <- 100-pie_continent$in_continent[which(pie_continent$continent == 'Overall')]
        
      }else{
        the_sample <- MetasubDataPreds[which(MetasubDataPreds$continent == i),]
        pie_continent$in_continent[which(pie_continent$continent == i)] <- nrow(the_sample[which(the_sample$sameContinent == TRUE),])/nrow(the_sample)*100
        pie_continent$not_in_continent[which(pie_continent$continent == i)] <- 100-pie_continent$in_continent[which(pie_continent$continent == i)]
        
      }
    }
    pie_continent$latitude[which(pie_continent$continent == "Overall")] <- -62
    pie_continent$longitude[which(pie_continent$continent == "Overall")] <- -155
    
  }else{
    pie_continent <- data.frame(continent=unique(MetasubDataPreds$continent),
                                latitude= NA, longitude = NA,
                                in_continent = NA, not_in_continent = NA)
    
    for(i in unique(MetasubDataPreds$continent)){
      
      the_sample <- MetasubDataPreds[which(MetasubDataPreds$continent == i),]
      pie_continent$in_continent[which(pie_continent$continent == i)] <- nrow(the_sample[which(the_sample$sameContinent == TRUE),])/nrow(the_sample)*100
      pie_continent$not_in_continent[which(pie_continent$continent == i)] <- 100-pie_continent$in_continent[which(pie_continent$continent == i)]
      
    }
    
  }
  pie_continent$latitude[which(pie_continent$continent == "Africa")] <- -12
  pie_continent$longitude[which(pie_continent$continent == "Africa")] <- -4
  pie_continent$latitude[which(pie_continent$continent == "Europe")] <- 73
  pie_continent$longitude[which(pie_continent$continent == "Europe")] <- 40
  pie_continent$latitude[which(pie_continent$continent == "Asia")] <- 44
  pie_continent$longitude[which(pie_continent$continent == "Asia")] <- 156
  pie_continent$latitude[which(pie_continent$continent == "Oceania")] <- -21
  pie_continent$longitude[which(pie_continent$continent == "Oceania")] <- 97
  pie_continent$latitude[which(pie_continent$continent == "Americas")] <- 26
  pie_continent$longitude[which(pie_continent$continent == "Americas")] <- -56
  
  colnames(pie_continent)[4:5] <- c("Same continent", "Different continent")
  
  print(pie_continent)
  
  ##### 6. map visualization with pie chart #####
  cols <- c("Same continent"="steelblue1","Different continent"="violetred1",
            "original geographic coordinate"="blue4", 'predicted geographic coordinate'="firebrick3")
  
  mapWorld <- borders("world", colour="gray50", fill="white")
  p <- df %>% ggplot() + mapWorld+
    geom_point(aes(x = longitude, y = latitude, color = "original geographic coordinate"))+
    geom_point(aes(x = longPred, y = latPred, color = 'predicted geographic coordinate'))+
    geom_segment(aes(x = longitude, y = latitude,
                     xend = longPred, yend = latPred, group = sameContinent, color = color ),
                 arrow = arrow(length = unit(0.2, 'cm')), size = 0.25, alpha = 0.5)+
    geom_scatterpie(aes(x=longitude, y=latitude,  r = 6),
                    data = pie_continent, cols = c("Same continent", "Different continent") )+
    labs(x = "longitude",
         y = "latitude")+
    scale_colour_manual(name="Prediciton",values=cols, 
                        guide = guide_legend(override.aes=aes(fill=NA))) + 
    scale_fill_manual(name="Prediciton",values=cols, guide="none") +
    theme(legend.position = "bottom") 
  
  ggsave(p, file=paste0(png_path,'_',filter_level,'.png'), dpi = 600, width = 14, height=8)
  
}
