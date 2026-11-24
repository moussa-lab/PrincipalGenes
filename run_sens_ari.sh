#!/usr/bin/bash

nohup Rscript -e "rmarkdown::render('Azimuth_MMC_Sensitivity_ARI.Rmd')" > Azimuth_MMC_Sensitivity_ARI.log 2>&1 &
nohup Rscript -e "rmarkdown::render('PBMC_Sensitivity_ARI.Rmd')" > PBMC_Sensitivity_ARI.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Pollen_Sensitivity_ARI.Rmd')" > Pollen_Sensitivity_ARI.log 2>&1 &
nohup Rscript -e "rmarkdown::render('TabulaMuris_Lung_Sensitivity_ARI.Rmd')" > TabulaMuris_Lung_Sensitivity_ARI.log 2>&1 &
nohup Rscript -e "rmarkdown::render('TabulaMuris_Liver_Sensitivity_ARI.Rmd')" > TabulaMuris_Liver_Sensitivity_ARI.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Human_Primary_Motor_Cortex_Sensitivity_ARI.Rmd')" > Human_Primary_Motor_Cortex_Sensitivity_ARI.log 2>&1 &
