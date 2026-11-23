#!/bin/bash

nohup Rscript -e "rmarkdown::render('Louvain_Run.Rmd')" > Louvain_Run.log 2>&1 &
