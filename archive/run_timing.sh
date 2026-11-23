#!/bin/bash

nohup Rscript -e "rmarkdown::render('Principal_Genes_Timing.Rmd')" > Principal_Genes_Timing.log 2>&1 &
