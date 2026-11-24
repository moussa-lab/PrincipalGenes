# PrincipalGenes
Repository of Data and Code for the development and evaluation of the Principal Genes Method

## Aquiring Datasets
- Input count matrices are available [here](https://drive.google.com/drive/folders/1ltFHX5rjB8K2SGMiuhtHgTSbZug4zVzb?usp=sharing)
- Uncompress input count matrices in `Filtered_Input.tar.gz` using the commands below. Ensure the input data is in the parent directory (PrincipalGenes).

```
tar -zxvf Filtered_Input.tar.gz .
```

## Directory Structure for code execution
Create the expected directory structure by executing the `create_directories.sh` script:

```
./create_directories.sh
```

## Running Code
All code was written and tested in R version 4.5.2, it takes several hours of runtime per script to generate all figures and tables so it's recommended to run each script one at a time and check on the progress before running the next one. Intensive memory and CPU utilization is expected since all code was written and tested on a virtual machine with 170 GB of memory and 32 CPU cores.

### Execution Order and Output Figures
All scripts should be executed in the order they are listed below. Output figures for each run can be found in the associated directories:
- `run_sens_ari.sh`: PCA_Plots, Clustering_Sensitivity, Adjusted_Rand_Index
- `run_dispersion_gps.sh` : Dispersion_GPS_Plots
- `run_distortion_free.sh` : Distortion_Free_PCA
- `run_no_name.sh` : No_Name_Filtering
- `run_louvain_unsupervised.sh`: Unsupervised_Results
- `run_statistical_testing.sh`: Statistical_Tests
- `run_vary_pcs.sh`: Properties_and_Performance
- `run_timing.sh`: Properties_and_Performance
- `run_Bootstrapping.sh`: Bootstrapping_Results
- `run_vst.sh`: VST_Comparison
- Data_Hold directories will contain several intermediary tables that can be used for downstream analysis and exploration.
- Figures are primarily stored in the results directories.
- Statistical_Tests will contain composite results for all datasets obtained when running Statistical_testing.Rmd
