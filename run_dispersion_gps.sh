#!/usr/bin/bash

nohup Rscript -e "rmarkdown::render('Dispersion_vs_GPS.Rmd')" > Dispersion_vs_GPS.log 2>&1 &
