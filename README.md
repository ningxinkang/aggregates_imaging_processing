# Imaging processing
## About
This set of script is used to take in a set of imaging result treated after ImageJ, formating and documenting individual datasheets into a large dataset, cleaning and extracting the features we want to look at, and generating the graphs.

## Preparing protocol
1.	Separate the GFP result and the DAPI result into two folders.
2.	For each folder, create a .xls file name `filepath.xls`
3.	For each .xls file, we want to create a table with only one column, with the column name `filepath`.
4.	Open your terminal. Go to the directory of your GFP result (or DAPI result) by typing `cd dir_of_your_choice`.
5.	Type `ls -d $PWD/*` to let the system print out the absolute path of files in the directory.
6.	Copy the absolute paths of the files into the filepath.xls we just created and save it.

## Directory Structure

    ./
    ├── README.md
    ├── input
    │   ├── DAPI                                <- original filepath of DAPI
    │   │   └── filepath.xls
    │   ├── DAPI.csv                            <- formatted DAPI dataframe
    │   ├── GFP                                 <- original filepath of GFP
    │   │   └──filepath.xls
    │   ├── GFP.csv                             <- formatted GFP dataframe
    │   ├── IntDen_vs_DAPI_per_mouse.csv        <- Features extracted
    │   ├── IntDen_vs_DAPI_per_pic.csv
    │   ├── agg_per_mouse.csv
    │   ├── agg_per_pic.csv
    │   ├── agg_vs_DAPI_per_mouse.csv
    │   ├── agg_vs_DAPI_per_pic.csv
    │   ├── area_vs_DAPI_per_mouse.csv
    │   ├── area_vs_DAPI_per_pic.csv
    │   ├── avg_IntDen_per_mouse.csv
    │   ├── avg_IntDen_per_pic.csv
    │   ├── avg_area_per_mouse.csv
    │   ├── avg_area_per_pic.csv
    │   ├── total_IntDen_per_mouse.csv
    │   ├── total_IntDen_per_pic.csv
    │   ├── total_area_per_mouse.csv
    │   └── total_area_per_pic.csv
    ├── report.pptx
    ├── result                                  <- the plots generated with diff features
    │   ├── IntDen_distribution.png
    │   ├── IntDen_per_DAPI.png
    │   ├── IntDen_per_aggregate.png
    │   ├── area_distribution.png
    │   ├── area_per_DAPI.png
    │   ├── area_per_aggregate.png
    │   ├── avg_IntDen.png
    │   ├── avg_area.png
    │   ├── num_agg.png
    │   ├── num_agg_per_DAPI.png
    │   ├── total_IntDen.png
    │   └── total_area.png
    └── script                                  <- dir for scripts
        ├── 1_data_cleaning.ipynb               <- Script that merge and clean the inidvidual data
        ├── 2_info_extraction.ipynb             <- Script that extract features
        ├── 3_plot.R                            <- Script for plotting features usng the functions below
        ├── feature_distribution.R              <- function for plotting the distribution
        ├── feature_plot.R                      <- function for plotting the features
        └── summarySE.R                         <- function for calculating standard error bar