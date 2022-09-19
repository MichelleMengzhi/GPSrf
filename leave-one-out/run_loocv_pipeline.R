reference_sample <- as.data.frame(read.table('sample_feature/reference_sample', header = T)[,c(1:2)])
test_sample <- as.data.frame(read.table('sample_feature/test_sample', header = T)[,c(1:2)])
head(reference_sample)

# add GRC and Populations 
reference_sample <- cbind(reference_sample, reference_sample[,1])
colnames(reference_sample) <- c('Populations', 'GRC', 'sampleID')
# str(reference_sample)
test_sample <- cbind(test_sample, test_sample[,1])
colnames(test_sample) <- c('Populations', 'GRC', 'sampleID')


# Add meta information
meta <- read.csv('meta_table') 
source('add_meta.R')
reference_sample_meta <- add_meta_reich(reference_sample, meta)
# save(reference_sample_meta, file = 'reference_sample_meta.rdata')
test_sample_meta <- add_meta_reich(test_sample, meta)
# save(test_sample_meta, file = 'test_sample_meta.rdata')


##### for test sample from the best result in 100 shuffle datatset #####
index_lst_test <- c()
ct <-1
for(i in 1:nrow(test_sample_meta)){
  # use features selected from reference samples
  test_name <- paste0(test_sample_meta$Populations[i],test_sample_meta$sampleID[i])
  
  ### training sample ###
  the_training_samples_meta <- rbind(test_sample_meta[-i,], reference_sample_meta)
  training_sample_file_name <- paste0('inter_file/',test_name,'_training_sample')
  write.table(the_training_samples_meta[, c(3,2)], file = training_sample_file_name, 
              quote = F, row.names = F, col.names = F)
  
  # extract training samples
  system(paste0('plink --bfile ../reich_here_overlap_qc2 --keep ',training_sample_file_name,
                ' --make-bed --out inter_file/',test_name,'_training --noweb'))
  # merge genepool to training samples
  # for baseline
  system(paste0('plink --bfile inter_file/',test_name,
                '_training --bmerge ../genepool_overlap_qc.bed ../genepool_overlap_qc.bim ../genepool_overlap_qc.fam  --make-bed --out inter_file/',test_name,'_training_baseline_overlap --noweb --allow-no-sex'))
  system(paste0("cut -f1-2 -d ' ' inter_file/",test_name,"_training_baseline_overlap.fam > inter_file/",test_name,"_training_baseline_overlap.pop.txt"))
  system(paste0("printf '%.0s\n' {1..",nrow(the_training_samples_meta),"}  > inter_file/",test_name,"_training_baseline_overlap.pop"))
  
  # for benchmark
  system(paste0('plink --bfile inter_file/',test_name,
                '_training_baseline_overlap --extract sample_feature/benchmark.snp  --make-bed --out inter_file/',test_name,'_training_selected_bench --noweb'))
  system(paste0("cut -f1-2 -d ' ' inter_file/",test_name,"_training_selected_bench.fam > inter_file/",test_name,"_training_selected_bench.pop.txt"))
  system(paste0("printf '%.0s\n' {1..",nrow(the_training_samples_meta),"}  > inter_file/",test_name,"_training_selected_bench.pop"))
  
  # for split300
  system(paste0('plink --bfile inter_file/',test_name,
                '_training_baseline_overlap --extract sample_feature/socres_df.split.top300.snp  --make-bed --out inter_file/',test_name,'_training_selected_split300 --noweb'))
  system(paste0("cut -f1-2 -d ' ' inter_file/",test_name,"_training_selected_split300.fam > inter_file/",test_name,"_training_selected_split300.pop.txt"))
  system(paste0("printf '%.0s\n' {1..",nrow(the_training_samples_meta),"}  > inter_file/",test_name,"_training_selected_split300.pop"))
  

  ### test sample ###
  test_sample_file_name <- paste0('inter_file/',test_name,'_test_sample')
  write.table(test_sample_meta[i, c(3,2)], file = test_sample_file_name, 
              quote = F, row.names = F, col.names = F)
  # extract test samples
  system(paste0('plink --bfile ../reich_here_overlap_qc2 --keep ',test_sample_file_name,
                ' --make-bed --out inter_file/',test_name,'_test --noweb'))
  # merge genepool to test samples
  # for baseline
  system(paste0('plink --bfile inter_file/',test_name,
                '_test --bmerge ../genepool_overlap_qc.bed ../genepool_overlap_qc.bim ../genepool_overlap_qc.fam  --make-bed --out inter_file/',test_name,'_test_baseline_overlap --noweb --allow-no-sex'))
  system(paste0("cut -f1-2 -d ' ' inter_file/",test_name,"_test_baseline_overlap.fam > inter_file/",test_name,"_test_baseline_overlap.pop.txt"))
  system(paste0("printf '%.0s\n' {1}  > inter_file/",test_name,"_test_baseline_overlap.pop"))
  
  # for benchmark
  system(paste0('plink --bfile inter_file/',test_name,
                '_test_baseline_overlap --extract sample_feature/benchmark.snp  --make-bed --out inter_file/',test_name,'_test_selected_bench --noweb'))
  system(paste0("cut -f1-2 -d ' ' inter_file/",test_name,"_test_selected_bench.fam > inter_file/",test_name,"_test_selected_bench.pop.txt"))
  system(paste0("printf '%.0s\n' {1}  > inter_file/",test_name,"_test_selected_bench.pop"))
  
  # for split300
  system(paste0('plink --bfile inter_file/',test_name,
                '_test_baseline_overlap --extract sample_feature/socres_df.split.top300.snp  --make-bed --out inter_file/',test_name,'_test_selected_split300 --noweb'))
  system(paste0("cut -f1-2 -d ' ' inter_file/",test_name,"_test_selected_split300.fam > inter_file/",test_name,"_test_selected_split300.pop.txt"))
  system(paste0("printf '%.0s\n' {1}  > inter_file/",test_name,"_test_selected_split300.pop"))

  
  index_lst_test <- c(index_lst_test, test_name)
  
  if(length(index_lst_test) == 7 | i == nrow(test_sample_meta)){ # one job submission for 7 cases
    message(paste0('=======================',paste(index_lst_test, collapse = ', '),'======================='))
    system(paste0("echo '#!/bin/bash\n#SBATCH -t 5:00:00\n#SBATCH -J split\n#SBATCH -N 1\n#SBATCH --tasks-per-node=8\n#SBATCH -o sh_file/ct",ct,
                  "_pipeline_%j.out\n#SBATCH -e sh_file/ct",ct,
                  "_pipeline_%j.err\n\n# write this script to stdout-file - useful for scripting errors\ncat $0\n\n# load the modules required for running R\n\nmodule load GCC/11.2.0\nmodule load OpenMPI/4.1.1\nmodule load R/4.1.2\n\n# run R script\nRscript --vanilla loocv_pipeline.R ",ct," ",paste(index_lst_test, collapse = ' '),
                  "' > sh_file/run_pipeline_",ct,".sh")) 
    system(paste0("sbatch sh_file/run_pipeline_",ct,".sh"))
#    Sys.sleep(600)
    index_lst_test <- c()
    if(ct %% 10 == 0){
        Sys.sleep(1200)
    }
    ct <- ct+1
     
 }
}
