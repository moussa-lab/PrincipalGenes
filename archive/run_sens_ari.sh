#!/bin/bash

nohup Rscript -e "rmarkdown::render('Azimuth_MMC_Sensitivity_ARI.Rmd')" > Sens_ARI_Azimuth.log 2>&1 &
nohup Rscript -e "rmarkdown::render('PBMC_Sensitivity_ARI.Rmd')" > Sens_ARI_PBMC.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Pollen_Sensitivity_ARI.Rmd')" > Sens_ARI_Pollen.log 2>&1 &
nohup Rscript -e "rmarkdown::render('TabulaMuris_Lung_Sensitivity_ARI.Rmd')" > Sens_ARI_TabulaMuris_Lung.log 2>&1 &
nohup Rscript -e "rmarkdown::render('TabulaMuris_Liver_Sensitivity_ARI.Rmd')" > Sens_ARI_TabulaMuris_Liver.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Human_Primary_Motor_Cortex_Sensitivity_ARI.Rmd')" > Sens_ARI_Human_Primary_Motor_Cortex.log 2>&1 &