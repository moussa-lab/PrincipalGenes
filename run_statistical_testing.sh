#!/usr/bin/bash

nohup Rscript -e "rmarkdown::render('Statistical_Testing.Rmd')" > Statistical_Testing.log 2>&1 &
