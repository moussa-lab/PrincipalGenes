#!/usr/bin/bash

nohup Rscript -e "rmarkdown::render('Distortion_Free_PCA.Rmd')" > Distortion_Free_PCA.log 2>&1 &
