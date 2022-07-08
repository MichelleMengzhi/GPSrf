distance_diff_plot_ctry <- function(MetasubDataPreds, png_path){
  # Generate bar plot for predicted distance from the origin in levels: <100, 100-500, 500-1000, 1000-2000, 2000-3000, >3000, in the domain of country
  
  # @MetasubDataPreds: the data frame having population, admixture portion, geographic coordinate, predicted geographic coordinate, country, distance from the origin for all samples
  # @png_path: a string of the plot path and name
  
  #Calculate distance from origin
  population_names <- unique(MetasubDataPreds$country)
  
  bar_df1 <- data.frame(row.names = c(population_names, "Overall"))
  
  # <100
  for (i in 1: length(population_names)){
    this_city <- population_names[i]
    prop <- mean(MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] < 100)
    bar_df1[rownames(bar_df1) == this_city,"0 - 100km"] <- prop
    if(prop > 0){print(this_city)
      print(prop)}
  }
  overall_prop <- mean(MetasubDataPreds[,"Distance_from_origin"] < 100)
  bar_df1[ nrow(bar_df1),"0 - 100km"] <- overall_prop
  
  # >= 100 & < 500
  for (i in 1: length(population_names)){
    this_city <- population_names[i]
    prop <- mean(MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] >= 100 & MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] < 500)
    bar_df1[rownames(bar_df1) == this_city,"100 - 500km"] <- prop
  }
  overall_prop <-mean(MetasubDataPreds[,"Distance_from_origin"] >= 100 & MetasubDataPreds[,"Distance_from_origin"] < 500)
  bar_df1[ nrow(bar_df1),"100 - 500km"] <- overall_prop
  
  # >= 500 & < 1000
  for (i in 1: length(population_names)){
    this_city <- population_names[i]
    prop <- mean(MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] >= 500 & MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] < 1000)
    bar_df1[rownames(bar_df1) == this_city,"500 - 1000km"] <- prop
  }
  overall_prop <- mean(MetasubDataPreds[,"Distance_from_origin"] >= 500 & MetasubDataPreds[,"Distance_from_origin"] < 1000)
  bar_df1[ nrow(bar_df1),"500 - 1000km"] <- overall_prop
  
  # >= 1000 & < 2000
  for (i in 1: length(population_names)){
    this_city <- population_names[i]
    prop <- mean(MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"]>= 1000 & MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] < 2000)
    bar_df1[rownames(bar_df1) == this_city,"1000 - 2000km"] <- prop
  }
  overall_prop <- mean(MetasubDataPreds[,"Distance_from_origin"] >= 1000 & MetasubDataPreds[,"Distance_from_origin"] < 2000)
  bar_df1[ nrow(bar_df1),"1000 - 2000km"] <- overall_prop
  
  # >= 2000 & < 3000
  for (i in 1: length(population_names)){
    this_city <- population_names[i]
    prop <- mean(MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] >= 2000 & MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] < 3000)
    bar_df1[rownames(bar_df1) == this_city,"2000 - 3000km"] <- prop
  }
  overall_prop <- mean(MetasubDataPreds[,"Distance_from_origin"] >= 2000 & MetasubDataPreds[,"Distance_from_origin"] < 3000)
  bar_df1[nrow(bar_df1),"2000 - 3000km"] <- overall_prop
  
  # >= 3000
  for (i in 1: length(population_names)){
    this_city <- population_names[i]
    prop <- mean(MetasubDataPreds[MetasubDataPreds$country == this_city,][,"Distance_from_origin"] > 3000 )
    bar_df1[rownames(bar_df1) == this_city,"> 3000km"] <- prop
  }
  overall_prop <- mean(MetasubDataPreds[,"Distance_from_origin"] > 3000)
  bar_df1[ nrow(bar_df1),"> 3000km"] <- overall_prop
  
  bar_df1$country <- rownames(bar_df1)
  
  
  population_counts <- as.data.frame(table(MetasubDataPreds$country))
  population_counts$Var1 <- as.character(population_counts$Var1)
  population_counts <- rbind(population_counts, c('Overall', 0))
  colnames(population_counts)[1] <- 'country'
  bar_df1 <- merge(x = bar_df1, y = population_counts[ , c("country", "Freq")], by = "country", all.x=TRUE)
  bar_df1 <- rbind(bar_df1[which(bar_df1$country == 'Overall'),], bar_df1[-which(bar_df1$country == 'Overall'),])
  
  bar_df1$title <- NA
  for(i in 2:nrow(bar_df1)){
    bar_df1$title[i] <- paste0(bar_df1$country[i],'(',bar_df1$Freq[i],')' )
  }
  
  png(png_path, width = 20, height = 8, units = 'in', res = 600)
  par(xpd = T, mar = par()$mar + c(10,1,0,2),  las=2, mgp = c(3,1,0))
  
  layout(mat = matrix(c(1, 2), 
                      nrow = 1, 
                      ncol = 2),
         widths = c(1, 8))     # Widths of the two columns
  
  bp1 <- barplot(t(bar_df1[1,c(2:7)]*100), space = 0,col=c("lightyellow","slategray1","lightblue", "skyblue", "royalblue3", "darkblue"), 
                 names.arg=c("Overall",axes = FALSE) ,
                 cex.names=.6, ylab = "", axisnames = F, axes = F) 
  axis(side =2, pos = 0)
  mtext(text = c("Overall"), 
        side = 1, at = bp1, line = 0, padj = 1, cex=0.7) 
  title(ylab="Proportion of sample predictions %", mgp=c(3,1,0),cex.lab=1, cex = 0.7)
  
  
  bp2 <- barplot(t(bar_df1[-1,c(2:7)]*100), space = 0,col=c("lightyellow","slategray1","lightblue", "skyblue", "royalblue3", "darkblue"), 
                 names.arg=c(bar_df1$title[-1],
                             axes = FALSE) ,
                 las =2, cex.names=.6, ylab = "", axisnames = F, axes = F) 
  axis(side =2, pos = 0)
  mtext(text = c(bar_df1$title[-1]), 
        side = 1, at = bp2, line = 0, padj = 1, cex=0.7) 
  
  legend("right",inset = c(-0.02,0), rev(c(colnames(bar_df1[,c(2:7)]))), 
         fill = rev(c("lightyellow","slategray1","lightblue", "skyblue", "royalblue3", "darkblue")) ,
         bty = 1, cex = 0.5)  
  
  
  
  dev.off()
  
}
