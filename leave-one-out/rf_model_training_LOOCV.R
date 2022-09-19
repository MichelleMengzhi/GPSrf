rf_model_training <- function(qfile_train_nogp_popFilter, qfile_test_nogp_popFilter, tag){
  
  # tag: a list, 
  #     element 1: ind i.e. test_name
  #     element 2: ite i.e. selected_feature_set (baseline/bench/split300)
  
  if(ncol(qfile_train_nogp_popFilter) != ncol(qfile_test_nogp_popFilter)){
    stop(paste('num of columns in train and test is not equal for case', tag[[1]]))
  }
  
  source('rf_latlong_LOOCV.R')
  load('sample_feature/coastlines.rdata')
  gp <- colnames(qfile_train_nogp_popFilter)[-c(1,2,12,13,14)]
  
  # random forest modle training and prediction
  start_time <- Sys.time()
  run_rlt <-rf_latlong(training = qfile_train_nogp_popFilter, testing = qfile_test_nogp_popFilter,
                       variables = gp, coast=coastlines)
  testPreds <- run_rlt[[1]]
  r2 <- run_rlt[[2]]
  
  end_time <- Sys.time()
  time_for_testPreds <- end_time- start_time
  message(time_for_testPreds)
  
  system(paste0("echo '",tag[[1]],"_",tag[[2]],",",r2,"' >> mean_R2"))
  
  add_preds <- cbind(qfile_test_nogp_popFilter,
                          "latPred" = testPreds[[1]],
                          "longPred" = testPreds[[2]] )
  
  save(add_preds, file = paste0('prediction/',tag[[1]],'_',tag[[2]],'_qfile.rdata'))
  MetasubDataPreds <- paste(add_preds, collapse = ',')
  system(paste0("echo '",MetasubDataPreds,"' >> ", tag[[2]], "_rf_rlt"))
  
}


