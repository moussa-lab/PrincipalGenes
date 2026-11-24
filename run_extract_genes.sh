#!/usr/bin/bash

nohup Rscript -e "rmarkdown::render('Extract_Principal_Genes_and_HVGs.Rmd')" > Extract_Principal_Genes_and_HVGs.log 2>&1 &
