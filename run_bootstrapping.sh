#!/usr/bin/bash

#Azimuth
nohup Rscript -e "rmarkdown::render('Azimuth_MMC_Bootstrapping.Rmd')" > Azimuth_MMC_Bootstrapping.log 2>&1 &
#PBMC
nohup Rscript -e "rmarkdown::render('PBMC_Bootstrapping.Rmd')" > PBMC_Bootstrapping.log 2>&1 &
#Pollen
nohup Rscript -e "rmarkdown::render('Pollen_Bootstrapping.Rmd')" > Pollen_Bootstrapping.log 2>&1 &
#BrainData
nohup Rscript -e "rmarkdown::render('Human_Primary_Motor_Cortex_Bootstrapping.Rmd')" > Human_Primary_Motor_Cortex_Bootstrapping.log 2>&1 &
#TabulaMurisLung
nohup Rscript -e "rmarkdown::render('TabulaMuris_Lung_Bootstrapping.Rmd')" > TabulaMuris_Lung_Bootstrapping.log 2>&1 &
#TabulaMurisLiver
nohup Rscript -e "rmarkdown::render('TabulaMuris_Liver_Bootstrapping.Rmd')" > TabulaMuris_Liver_Bootstrapping.log 2>&1 &
