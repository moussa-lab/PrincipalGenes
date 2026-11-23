#!/bin/bash

nohup Rscript -e "rmarkdown::render('Vst_TabulaMuris_Liver.Rmd')" > Vst_TabulaMuris_Liver.log 2>&1 &
