args <- commandArgs(trailingOnly=TRUE)
ct <- args[[1]]
samples <- args[-1]
message(paste0('rf_',ct,'.sh is generated for ',paste(samples, collapse = ', ')))

system(paste0("echo '#!/bin/bash\n#SBATCH -t 15:00:00\n#SBATCH -J RF\n#SBATCH -N 1\n#SBATCH --tasks-per-node=8\n#SBATCH -o sh_file/ct",ct,"_%j.out\n#SBATCH -e sh_file/ct",ct,"_%j.err\n\n# write this script to stdout-file - useful for scripting errors\ncat $0\n\n# load the modules required for running R\n\nmodule load GCC/11.2.0\nmodule load OpenMPI/4.1.1\nmodule load R/4.1.2\n\n# run R script\n' > sh_file/rf_",ct,".sh"))    
for(ind in samples){ # 7 samples
  test_name <- ind
  
  ### training sample ###
  system(paste0('sh feature_selection.sh inter_file/ ',test_name,'_training '))
  
  ### test sample ###
  system(paste0('sh feature_selection.sh inter_file/ ',test_name,'_test'))
  
  ### write into sh file and remove intermediate files
  system(paste0("echo 'Rscript --vanilla run_rf_LOOCV.R ",test_name,"' >> sh_file/rf_",ct,".sh"))
  system(paste0('rm inter_file/',test_name,'*'))
  
}
system(paste0("cat sh_file/rf_",ct,".sh"))
system(paste0("sbatch sh_file/rf_",ct,".sh"))    


