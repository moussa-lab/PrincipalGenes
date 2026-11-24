# PrincipalGenes
Repository of Data and Code for the development and evaluation of the Principal Genes Method

## Aquiring Datasets
- Input count matrices are available [here](https://drive.google.com/drive/folders/1ltFHX5rjB8K2SGMiuhtHgTSbZug4zVzb?usp=sharing)
- Uncompress input count matrices in `Filtered_Input.tar.gz` using the commands below. Ensure the input data is in the parent directory (PrincipalGenes).

```
tar -zxvf Filtered_Input.tar.gz .
```

## Running Code
All code was written and tested in R version 4.5.2, it takes several hours of runtime per script to generate all figures and tables so it's recommended to run each script one at a time and check on the progress before running the next one. Intensive memory and CPU utilization is expected since all code was written and tested on an Ubuntu 22.04 virtual machine with 170 GB of memory and 32 CPU cores.

### Execution Order and Output Figures
All scripts should be executed in the order they are listed below. Output figures/csvs for each run can be found in the associated directories:
- `run_extract_genes.sh`: Data_Hold/Gene_Scores, Data_Hold/PCA_Coordinates
- `run_sens_ari.sh`: Data_Hold/Sensitivity_ARI, PCA_Plots, Sensitivity_ARI
- `run_dispersion_gps.sh` : Data_Hold/Dispersion_vs_GPS, Dispersion_GPS_Plots
- `run_distortion_free.sh` : Distortion_Free_PCA
- `run_no_name.sh` : Data_Hold/No_Name_Filtering_Sensitivity_ARI, No_Name_Filtering_Sensitivity_ARI
- `run_louvain.sh`: Data_Hold/Calinski_Harabasz, Data_Hold/Subset_Counts, Calinski_Harabasz
- `run_statistical_testing.sh`: Data_Hold/Statistical_Tests, Statistical_Tests
- `run_vary_pcs.sh`: Properties_and_Performance
- `run_timing.sh`: Properties_and_Performance
- `run_Bootstrapping.sh`: Bootstrapping_Results
- `run_vst.sh`: VST_Comparison
- Data_Hold directories will contain several intermediary tables that can be used for downstream analysis and exploration.
- Figures are primarily stored in the root directory in subdirectories specified above.
