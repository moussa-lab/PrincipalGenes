# PrincipalGenes
Repository of Data and Code for the development and evaluation of the Principal Genes Method

## Aquiring Datasets
- Input count matrices are available [here](https://drive.google.com/drive/folders/1ltFHX5rjB8K2SGMiuhtHgTSbZug4zVzb?usp=sharing)
- Uncompress input count matrices in `Filtered_Input.tar.gz` using the commands below. Ensure the input data is in the parent directory (PrincipalGenes).

```
tar -zxvf Filtered_Input.tar.gz .
```

## Running Code

All code was written and tested in R version 4.5.2, it takes several hours of runtime to generate all figures and tables so it's recommended to run everything as background processes. Intensive memory and CPU utilization is expected since all code was written and tested on a virtual machine with 120 GB of memory and 32 CPU cores.

### Execution Order
- Run all Sensitivity_ARI.Rmd files first then Statistical_Testing.Rmd then Boostrapping.Rmd files last.
- Data_Hold directories will contain several intermediary tables that can be used for downstream analysis and exploration.
- Figures are primarily stored in the results directories.
- Statistical_Tests will contain composite results for all datasets obtained when running Statistical_testing.Rmd

