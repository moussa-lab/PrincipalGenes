#!/usr/bin/bash

nohup Rscript -e "rmarkdown::render('No_Name_Filtering_Sensitivity_ARI.Rmd')" > No_Name_Filtering_Sensitivity_ARI.log 2>&1 &
