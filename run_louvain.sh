#!/usr/bin/bash

nohup Rscript -e "rmarkdown::render('Azimuth_MMC_Louvain_Run.Rmd')" > Azimuth_MMC_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Human_CRC_Louvain_Run.Rmd')" > Human_CRC_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Human_Intestines_Louvain_Run.Rmd')" > Human_Intestines_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Human_Primary_Motor_Cortex_Louvain_Run.Rmd')" > Human_Primary_Motor_Cortex_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Mouse_Heart_Louvain_Run.Rmd')" > Mouse_Heart_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('PBMC12K_Louvain_Run.Rmd')" > PBMC12K_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('PBMCs_Louvain_Run.Rmd')" > PBMCs_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Pollen_Louvain_Run.Rmd')" > Pollen_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Tabula_MurisLiver_Louvain_Run.Rmd')" > TabulaMuris_Liver_Louvain_Run.log 2>&1 &
nohup Rscript -e "rmarkdown::render('Tabula_MurisLung_Louvain_Run.Rmd')" > TabulaMuris_Lung_Louvain_Run.log 2>&1 &
