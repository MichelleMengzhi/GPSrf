rf_latlong <- function(training, testing, variables, coast=NULL, nthread=8){
  # Model training by random forest method and use the model to predict test set. Then adjust predicted geographic coordinates based on world point data and coastline data
  # @training: the data frame of training set, having sample names, corresponding ancestral population portion calculation, GRC, origin latitude and longitude, as well as country.
  # @testing: the data frame of test set, having sample names, corresponding ancestral population portion calculation, GRC, origin latitude and longitude, as well as country.
  # @variables: a list of column names used in model training
  # @coast: If coast = NULL, which is the feault, it means no need data adjustment after prediction; otherwise, it should be the coastline data generated in rf_model_training function. 
  # @nthread: number of threads to use when running this function. The default is 8.
  
  set.seed(1234)
  
  # create 5 subsets in training for cross validation
  folds <- createFolds(training[,'Populations'], k = 5, returnTrain = T)
  
  # ask cross validation when model training; the model training will be a regression
  trControl <-  trainControl( # regression
    method = "cv",
    number = 5,
    verboseIter = FALSE,
    returnData = FALSE,
    search = "grid",
    savePredictions = "final",
    allowParallel = T,
    index = folds)
  
  # use a range of value as the value of parameter so that all value will be tested and the model with smallest RMSE will be finally used
  tune_grid <- expand.grid(.mtry = c(1:15))
  
  # train for latitude
  training$rowIndex <- as.numeric(rownames(training))
  Xgb_latitude <- train(x = training[,variables],
                        y = training[,'latitude'],
                        method = "rf",
                        trControl = trControl,
                        tuneGrid = tune_grid ,
                        nthread = nthread
  )
  
  # train for longitude
  Xgb_longitude <- train(x = training[,variables],
                         y = training[,'longitude'],
                         method = "rf",
                         trControl = trControl,
                         tuneGrid = tune_grid ,
                         nthread = nthread
  )
  
  # predict the test set
  latPred <- predict(Xgb_latitude, newdata = testing[,variables])
  longPred <- predict(Xgb_longitude, newdata = testing[,variables])
  
  #adjust out of bounds predictions
  message('adjust out of bounds predictions')
  longPred[longPred > 180] <- 180
  longPred[longPred < -180] <- -180
  latPred[latPred > 90] <- 90
  latPred[latPred < -90] <- -90
  #Pull to nearest coastline if provided
  message('Pull to nearest coastline if provided')
  find_coast <- function(long, lat) { # find the closet point on the coast for the given long and lat
    distances_from_coastline <- sp::spDistsN1(coast, c(long, lat), longlat = TRUE)
    
    closest_point <-  which.min(distances_from_coastline)
    new_coords <- coast[closest_point,]
    
    return(new_coords)
    
  }
  
  if (!is.null(coast)) {
    message('toAdjust generated by function map.where')
    # find the longPred / latPred that are not on world latitude/longitude
    toAdjust <-
      which(is.na(maps::map.where(database = "world", longPred, latPred)))
    
    if(length(toAdjust) > 0){
      # apply find_coast function to adjust the latitude and longitude of given toAdjust index
      message('adjusted generated by mapply find_coast and longPred, latPred')
      adjusted <- mapply(find_coast, long = longPred[toAdjust], lat = latPred[toAdjust])
      
      # update the adjusted lat and long
      longPred[toAdjust] <- adjusted[1,]
      latPred[toAdjust] <- adjusted[2,]
    }
  }
  
  mean_r2 <- mean(c(Xgb_longitude$results$Rsquared[which(Xgb_longitude$results$mtry == Xgb_longitude$bestTune[[1]])],
                    Xgb_latitude$results$Rsquared[which(Xgb_latitude$results$mtry == Xgb_latitude$bestTune[[1]])]
  ))
  
  message('return')
  return(list(list(latPred, longPred),
              mean_r2) )
  
  
}
























