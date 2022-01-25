
# Overview 
This repository contains the code that covers the "raw data" to "cooked data" phase of this [article](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8325917/) and [repository](https://github.com/earthlab/idtrees_earthlab). In step 2, the script extracts additional geometric and intensity lidar features from point clouds.

## The steps for implementing the code in this repository
|     Step      |    Script     | input            | output  |  
| ------------- | ------------- | -------------    | ------------- |
|1 |las_individual_tree_pseudo_wf.R|las files in "LAS", "train_MLBS.shp", "train_OSBS.shp"|las files of individual trees in "las", "idtrees_pwave_indvdID.csv"  |
|2 |geometric_intensity_lidar_features.R|las files of individual trees in "las" |"geometric_intensity_metrics_indvdID.csv" |
|3 |hyperspectral_part.R  |"train_MLBS.shp", "train_OSBS.shp", GeoTiff files in "HSI" , |"hyperspectral_train.csv"  |


## Repository tree

```bash
.
├───labdata_competition_data
│   └───IDTREES_competition_train
│       ├───Field
│       ├───ITC
│       └───RemoteSensing
│           ├───CHM
│           ├───HSI
│           ├───LAS
│           └───RGB
├───output
│   └───las
└───R_scripts

```
