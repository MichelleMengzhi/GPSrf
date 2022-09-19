add_meta_ref <- function(metasub_data, meta){
  # Read population metasub information
  meta_latlong <- meta[which(sapply(X = meta$inCountry, FUN = isTRUE)),]
  metasub_data <- metasub_data[which(metasub_data$Populations %in% meta_latlong$nameArgument),] 
  meta_latlong$country <- make.names(meta_latlong$country)
  
  # Add country, latitude, longitude to the corresponding population
  metasub_data$country <- NA
  metasub_data$latitude <- NA
  metasub_data$longitude <- NA
  for(i in 1:nrow(metasub_data)){
    the_population <- metasub_data$Populations[i]
    metasub_data$country[i] <- meta_latlong$country[which(meta_latlong$nameArgument == the_population)]
    metasub_data$latitude[i] <- meta_latlong$latitidue[which(meta_latlong$nameArgument == the_population)]
    metasub_data$longitude[i] <- meta_latlong$longitude[which(meta_latlong$nameArgument == the_population)]
  }
  
  return(metasub_data)
}

add_meta_reich <- function(qfile_nogp, meta){
  # Read population metasub information
  # meta_latlong <- meta[which(sapply(X = meta$inCountry, FUN = isTRUE)),]
  # metasub_data <- metasub_data[which(metasub_data$Populations %in% meta_latlong$nameArgument),] 
  # meta_latlong$country <- make.names(meta_latlong$country)
  
  meta_pop <- meta[which(meta$Version.ID %in% qfile_nogp$GRC),]
  message(nrow(meta_pop))
  qfile_nogp_popFilter <- qfile_nogp[which(qfile_nogp$GRC %in% meta_pop$Version.ID),]
  message(nrow(qfile_nogp_popFilter))
  
  meta_pop$Country <- make.names(meta_pop$Country)
  meta_pop$Version.ID <- as.character(meta_pop$Version.ID)
  # Add country, latitude, longitude to the corresponding population
  qfile_nogp_popFilter$country <- NA
  qfile_nogp_popFilter$latitude <- NA
  qfile_nogp_popFilter$longitude <- NA
  for(i in 1:nrow(qfile_nogp_popFilter)){
    versionID <- qfile_nogp_popFilter$GRC[i]
    qfile_nogp_popFilter$Populations[i] <- as.character(meta_pop$Group_Label[which(meta_pop$Version.ID == versionID)])
    qfile_nogp_popFilter$country[i] <- as.character(meta_pop$Country[which(meta_pop$Version.ID == versionID)])
    qfile_nogp_popFilter$latitude[i] <- as.numeric(as.character(meta_pop$Lat.[which(meta_pop$Version.ID == versionID)]))
    qfile_nogp_popFilter$longitude[i] <- as.numeric(as.character(meta_pop$Long.[which(meta_pop$Version.ID == versionID)]))
  }
  
  return(qfile_nogp_popFilter)
}







