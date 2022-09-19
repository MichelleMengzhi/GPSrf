library(caret)
args <- commandArgs(trailingOnly=TRUE)
iteration <- c('baseline', 'bench', 'split300')
ind <- args[[1]]

for(ite in iteration){ # basleine; benchmark; split300
  message(paste('==========',ind,ite,'=========='))
  
  qfile_train <- read.table(paste0('inter_file/out_Q_',ind,'_training_',ite), header = T, sep = '\t') 
  qfile_test <- read.table(paste0('inter_file/out_Q_',ind,'_test_',ite), header = T, sep = '\t')
  
  # Add meta information
  meta <- read.csv('meta_table') #nrow(meta)=14008
  source('add_meta.R')
  
  qfile_train_nogp <- qfile_train[-which(qfile_train$Population %in% c('NorthEastAsian', 'Mediterranean',
                                                                       'SouthAfrican', 'SouthWestAsian',
                                                                       'NativeAmerican', 'Oceanian',
                                                                       'SouthEastAsian', 'NorthernEuropean',
                                                                       'SubsaharanAfrican')), ] 
  qfile_train_nogp$Populations<- as.character(qfile_train_nogp$Populations)
  qfile_train_nogp_popFilter <- add_meta_reich(qfile_train_nogp, meta)
  qfile_train_nogp_popFilter <- droplevels(qfile_train_nogp_popFilter)
  print('qfile_train_nogp_popFilter:')
  str(qfile_train_nogp_popFilter)
  
  qfile_test_nogp <- qfile_test[-which(qfile_test$Population %in% c('NorthEastAsian', 'Mediterranean',
                                                                    'SouthAfrican', 'SouthWestAsian',
                                                                    'NativeAmerican', 'Oceanian',
                                                                    'SouthEastAsian', 'NorthernEuropean',
                                                                    'SubsaharanAfrican')), ] 
  qfile_test_nogp$Populations<- as.character(qfile_test_nogp$Populations)
  qfile_test_nogp_popFilter <- add_meta_reich(qfile_test_nogp, meta)
  qfile_test_nogp_popFilter <- droplevels(qfile_test_nogp_popFilter)
  print('qfile_test_nogp_popFilter:')
  str(qfile_test_nogp_popFilter)
  
  qfile_train_nogp_popFilter$GRC <- as.character(qfile_train_nogp_popFilter$GRC)
  if(sum(is.na(qfile_train_nogp_popFilter$longitude)) > 0){
    qfile_train_nogp_popFilter <- qfile_train_nogp_popFilter[-which(is.na(qfile_train_nogp_popFilter$longitude)),]
  }
  qfile_test_nogp_popFilter$GRC <- as.character(qfile_test_nogp_popFilter$GRC)
  if(sum(is.na(qfile_test_nogp_popFilter$longitude)) > 0){
    qfile_test_nogp_popFilter <- qfile_test_nogp_popFilter[-which(is.na(qfile_test_nogp_popFilter$longitude)),]
  }
  if(nrow(qfile_test_nogp_popFilter) != 1){
    warning(paste0('Test sample ',ind,' does not have valid longitude, skip run RF'))
  }else{
    save(qfile_test_nogp_popFilter, file = paste0('prediction/',ind,'_',ite,'_rf.rdata'))
    
    ### Model training ###
    source('rf_model_training_LOOCV.R')
    rf_model_training(qfile_train_nogp_popFilter, qfile_test_nogp_popFilter, tag = c(ind, ite))
    
  }
  
}



