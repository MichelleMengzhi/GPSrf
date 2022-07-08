barplot_admixture <- function(meta, pic_file_path){
  # Generate a PDF file having bar plot showing admixture portion for all sample, grouped by country, ordered by continent
  
  # @meta: the data frame having population, admixture portion, latitude, longitude, country for all samples
  # @pic_file_path: a string of pdf output file path and name
  
  library(dplyr)
  
  genepools <- c('NorthEastAsian', 'Mediterranean', 'SouthAfrican',
                 'SouthWestAsian', 'NativeAmerican', 'Oceanian',
                 'SouthEastAsian', 'NorthernEuropean', 'SubsaharanAfrican')
  
  barNaming <- function(col){
    retCol <- col
    for(i in 2:length(col)){
      if(stringr::str_trim(col[i-1]) == stringr::str_trim(col[i]) ){
        retCol[i] <- ''
      }
      
    }
    return(retCol)
  }
  
  # Convert lat and long to numeric
  meta$latitude <- sapply(meta$latitude, as.numeric)
  meta$longitude <- sapply(meta$longitude, as.numeric) 
  
  # Get the mean latitude and longitude for each country
  ave_ctry <- data.frame(country=NA, latitude=NA, longitude=NA)
  ctry_lst <- list()
  for(i in unique(meta$country)){
    ctry_lst[[i]] <- meta[which(meta$country == i),]
    
    ave_ctry <- rbind(ave_ctry, c(i, 
                                  mean(ctry_lst[[i]]$latitude), 
                                  mean(ctry_lst[[i]]$longitude)) )
    
  }
  ave_ctry <- ave_ctry[-1,]
  ave_ctry$latitude <- sapply(ave_ctry$latitude, as.numeric)
  ave_ctry$longitude <- sapply(ave_ctry$longitude, as.numeric) 
  str(ave_ctry)
  
  # Split countries to corresponding continent based on their mean lat and long
  africa_med <- data.frame()
  euro <- data.frame()
  asia <- data.frame()
  oceanian <- data.frame()
  america <- data.frame()
  for(j in 1:nrow(ave_ctry)){
    if((-34.9 < ave_ctry$latitude[j] & ave_ctry$latitude[j] < 43.8) & 
       (-28 < ave_ctry$longitude[j] & ave_ctry$longitude[j] < 58)){
      africa_med <- rbind(africa_med, ave_ctry[j,])
    }else if((36 < ave_ctry$latitude[j] & ave_ctry$latitude[j] < 72) & 
             (-28 < ave_ctry$longitude[j] & ave_ctry$longitude[j] < 59)){
      euro <- rbind(euro, ave_ctry[j,])
    }else if((-10 < ave_ctry$latitude[j] & ave_ctry$latitude[j] < 78) & 
             (59 < ave_ctry$longitude[j] & ave_ctry$longitude[j] < 180)){
      asia <- rbind(asia, ave_ctry[j,])
    }else if((-50 < ave_ctry$latitude[j] & ave_ctry$latitude[j] < -10) &
             (110 < ave_ctry$longitude[j] & ave_ctry$longitude[j] < 180)){
      oceanian <- rbind(oceanian, ave_ctry[j,])
    }else{
      america <- rbind(america, ave_ctry[j,])
    }
    
  }
  nrow(africa_med) + nrow(euro) + nrow(asia) + nrow(oceanian) + nrow(america) == length(unique(meta$country))
  
  # Sort countries based on:
  # descending for abs. lat
  # from left to right based on long
  africa_med <- africa_med %>% arrange(latitude, desc(longitude) )
  euro <- euro %>% arrange(latitude, desc(longitude))
  asia <- asia %>% arrange(longitude, desc(latitude))
  if(nrow(oceanian) != 0){
    oceanian <-  oceanian %>% arrange(desc(latitude), longitude)
  }
  america <- america %>% arrange(longitude, desc(latitude))
  
  add_inds_to_dt <- function(ctry_lst, continent){
    result_sub_dt <- data.frame()
    for(i in continent$country){
      result_sub_dt <- rbind(result_sub_dt, ctry_lst[[i]])
    }
    return(result_sub_dt)
  }
  tbl.ordered <- add_inds_to_dt(ctry_lst, africa_med)
  tbl.ordered <- rbind(tbl.ordered, add_inds_to_dt(ctry_lst, euro))
  tbl.ordered <- rbind(tbl.ordered, add_inds_to_dt(ctry_lst, asia))
  tbl.ordered <- rbind(tbl.ordered, add_inds_to_dt(ctry_lst, oceanian))
  tbl.ordered <- rbind(tbl.ordered, add_inds_to_dt(ctry_lst, america))
  nrow(tbl.ordered) == nrow(meta)
  
  if(nrow(tbl.ordered) >= 10000){
    # Add column for ind with country size < 20
    popsize <- as.data.frame(table(tbl.ordered[,12]))
    duplicate_line <- which(tbl.ordered$country %in% popsize$Var1[which(popsize$Freq < 20)])
    dup <- 19
    
    
  }else if(nrow(tbl.ordered) >= 1000){
    # Add column for ind with country size < 20
    popsize <- as.data.frame(table(tbl.ordered[,12]))
    duplicate_line <- which(tbl.ordered$country %in% popsize$Var1[which(popsize$Freq < 5)])
    # tbl.ordered_cp <- tbl.ordered
    # tbl.ordered <- tbl.ordered_cp
    dup <- 4
    
  }else{
    duplicate_line <- NA
  }
  
  if(length(duplicate_line)>1){
    ct <- 0
    
    for(i in duplicate_line){
      the_line <- as.data.frame(tbl.ordered[i+ct*dup,])
      tmp <- rbind(the_line, the_line[rep(1, dup),])
      half_up <-  tbl.ordered[c(1:(i+ct*dup-1)),]
      half_down <- tbl.ordered[c((i+ct*dup+1):nrow(tbl.ordered)),]
      if(i == 1){
        tbl.ordered <- rbind(tmp, half_down)
      }else if(i+ct*dup == nrow(tbl.ordered) ){
        tbl.ordered <- rbind(half_up, tmp)
        
      }else{
        tbl.ordered <- rbind(half_up, tmp, half_down)
      }
      ct <- ct+1
    }
    
  }
  
  sector <- function (x0=0, y0=0, angle1, angle2, radius1, radius2, col, angleinc = 0.03){
    if(isTRUE(angle1 > angle2)) {
      temp <- angle1
      angle1 <- angle2
      angle2 <- temp
    }
    if (isTRUE(radius1 > radius2)) {
      temp <- radius1
      radius1 <- radius2
      radius2 <- temp
    }
    
    # Use 4 points polygon to draw a sector
    angles <- seq(angle1, angle2, by = angleinc)
    angles[length(angles)] <- angle2
    angles <- angles*(pi/180)
    xpos <- c(cos(angles) * radius1, cos(rev(angles)) * radius2) + x0
    ypos <- c(sin(angles) * radius1, sin(rev(angles)) * radius2) + y0
    polygon(xpos, ypos, col = col, border = col)
  }
  
  colorPalette <- c('#984807', '#fcae91', '#632ea8',
                    '#cb181d', '#ffff00', '#ff00ff',
                    '#0000ff', '#00ff00', '#ff0000')
  
  # setting
  rmin <- 6
  rmax <- 1.85*rmin
  amin <- -251
  amax <- 91
  prgap <- 0.2
  col <- colorPalette
  indcol <- 1
  Kstart <- 3
  Knum <- 9
  pdf(pic_file_path, 45, 45)
  par(xpd=T,  mar=c(2, 2, 2, 2)) 
  plot(0, 0, xlim=c(-rmax, rmax), ylim=c(-rmax, rmax), axes=F, ann=F, type='n') # initialize the plot
  rstart <- rmin
  rlen <- (rmax-rmin)*(1-prgap) 
  data <- tbl.ordered
  angelperInd <- (amax - amin)/nrow(data) 
  angpre <- amin
  
  for ( i in 1:nrow(data) ){   # iteration for each individual
    ang1 <- angpre
    ang2 <- ang1 + angelperInd
    rpre <- rstart
    for ( j in Kstart:(Knum+Kstart-1) ){   # iteration for each K
      rpost <- rpre + rlen*data[i,j]
      sector(angle1=ang1, angle2=ang2, radius1=rpre, radius2=rpost, col=col[j-Kstart+1])   ###### draw sector for each K of each individual
      rpre <- rpost
    }
    angpre <- ang2
  }
  lend <- rmax - prgap*(rmax-rmin)
  
  #write all population name
  target <- unique(tbl.ordered$country)
  npop <- length(target)
  
  nameArgument <- barNaming(tbl.ordered$country)
  angelperpop <- (amax - amin)/nrow(data)
  angpre <- amin
  for ( i in 1:nrow(tbl.ordered) ){
    
    ang <- angpre + angelperpop
    xx <- lend*cos(ang*pi/180)
    yy <- lend*sin(ang*pi/180)
    text = nameArgument[i];
    cex_no = 220/length(nameArgument)
    if(cex_no>=0.1){cex_no=3}
    if(cex_no<0.1){cex_no=2}
    if ( ang < -90 ){text(xx, yy, text, srt=180+ang, adj=c(1.1, 0.5), cex=cex_no, font=1, col=colors()[490])}
    else {text(xx, yy, text, srt=ang, adj=c(-0.1, 0.5), cex=cex_no, font=1, col=colors()[490])
    }
    angpre <- angpre + angelperpop
  }
  dev.off()
  
}
