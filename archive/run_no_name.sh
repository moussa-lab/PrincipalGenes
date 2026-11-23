#!/bin/bash

#PBMC
nohup Rscript -e "rmarkdown::render('No_Name_Filtering_PBMC.Rmd')" > No_Name_PBMC.log 2>&1 &
#Pollen
nohup Rscript -e "rmarkdown::render('No_Name_Filtering_Pollen.Rmd')" > No_Name_Pollen.log 2>&1 &
