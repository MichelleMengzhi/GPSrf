# admixture profile visualization
source('functions/barplot_admixture.R')
load('data/baseline_qfile.rdata')
barplot_admixture(meta = baseline_qfile, pic_file_path = 'output/admixture_barplot.pdf')





# geographic origin prediction
source('functions/rf_model_training_train_test.R')
load('data/baseline_qfile.rdata')
load('data/trainig_data.rdata')
rf_model_training_train_test(qfile_train_nogp_popFilter = training_data, 
                             qfile_test_nogp_popFilter = baseline_qfile, 
                             output_path = 'baseline_prediction.rdata')





# distance away from the origin bar plot visualization
source('functions/distance_diff_plot_ctry.R')
load('data/baseline_prediction.rdata')
MetasubDataPreds<- add_preds
for (i in 1:nrow(MetasubDataPreds)){
  MetasubDataPreds[i,"Distance_from_origin"] <- geosphere::distm(c(MetasubDataPreds[i,"longPred"],MetasubDataPreds[i,"latPred"]), c(MetasubDataPreds[i,"longitude"],MetasubDataPreds[i,"latitude"]), fun = geosphere::distHaversine)/1000
}
# Print distance from origin results 
print(c(mean(MetasubDataPreds$Distance_from_origin ),
        median(MetasubDataPreds$Distance_from_origin ),
        median(MetasubDataPreds$Distance_from_origin[which(MetasubDataPreds$Distance_from_origin < 100)])))

png_path  <- paste0('output/accuracy_plot_ctry.png')
distance_diff_plot_ctry(MetasubDataPreds, png_path) 





# distance away from the origin on world map visualization
source('functions/distance_map_visualization.R')
load('data/baseline_prediction.rdata')
MetasubDataPreds<- add_preds

MetasubDataPreds$continent <- countrycode::countrycode(sourcevar = MetasubDataPreds$country,
                                                       origin = "country.name",
                                                       destination = "continent")

# Manually assign unknown continent
MetasubDataPreds$continent[which(MetasubDataPreds$country=='Abkhazia')] <- 'Asia'
MetasubDataPreds$continent[which(MetasubDataPreds$country=='Czechoslovakia')] <- 'Europe'

distance_map_visualization(MetasubDataPreds, filter_level='all', png_path = 'baseline_worldmap')
distance_map_visualization(MetasubDataPreds, filter_level='notSameContinent', png_path = 'baseline_worldmap')
distance_map_visualization(MetasubDataPreds, filter_level='notSameContinent1000', png_path = 'baseline_worldmap')
distance_map_visualization(MetasubDataPreds, filter_level='notSameContinent500', png_path = 'baseline_worldmap')



