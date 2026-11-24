#!/usr/bin/bash

nohup Rscript -e "rmarkdown::render('Vary_Num_PCS.Rmd')" > Vary_Num_PCS.log 2>&1 &
