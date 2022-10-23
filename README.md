# Imaging processing
## About
This set of script is used to take in a set of imaging result treated after ImageJ, formating and documenting individual datasheets into a large dataset, cleaning and extracting the features we want to look at, comparing features between treatment groups using normal t-test and LMM (considering individual mouse variance), and generating the graphs.

## Preparing protocol
1.	Separate the GFP result and the DAPI result into two folders.
2.	For each folder, create a .xls file name `filepath.xls`
3.	For each .xls file, we want to create a table with only one column, with the column name `filepath`.
4.	Open your terminal. Go to the directory of your GFP result (or DAPI result) by typing `cd dir_of_your_choice`.
5.	Type `ls -d $PWD/*` to let the system print out the absolute path of files in the directory.
6.	Copy the absolute paths of the files into the filepath.xls we just created and save it.

## Steps
1.  To clone this repository for your own aggregate analysis, you want to use the code `git clone https://github.com/ningxinkang/aggregates_imaging_processing.git` or click `Code -> Download Zip` to the local directory you want.
2.  After downloading, you will see a file directory with components listed below in **Directory Strcuture**. The coding scripts are in `script/`, while the sample output are in the `result/`, feel free to delete them.
3.  Parallel with `script/` and `result/`, you also want to create a directory called `input/`, in which you put the GFP result and DAPI result you handled from the previous section.
4.  You want to run the code one by one, following the number in the file name. After openning each file, make sure you change the corresponding variable based on my comment, so that you are making sure your input feature and directory, as well as output directory is right.

## Directory Structure

    ./
    ├── README.md
    ├── result                                  <- the plots generated with diff features
    │   ├── IntDen_distribution.png
    │   ├── IntDen_per_DAPI.png
    │   ├── area_distribution.png
    │   ├── area_distribution_nofilter.png
    │   ├── area_per_DAPI.png
    │   ├── avg_IntDen.png
    │   ├── avg_area.png
    │   ├── num_agg.png
    │   ├── num_agg_per_DAPI.png
    │   ├── total_IntDen.png
    │   └── total_area.png
    └── script                                  <- dir for scripts
        ├── 1_data_cleaning.ipynb               <- Step1, Script that merge and clean the inidvidual data
        ├── 2_info_extraction.ipynb             <- Step2, Script that extract features
        ├── 3_plot.R                            <- Step3, Script for plotting features usng the functions below
        ├── feature_distribution.R              <- function for plotting the distribution
        ├── feature_plot.R                      <- function for plotting the features
        └── summarySE.R                         <- function for calculating standard error bar

### Author
Ningxin Kang (nik010@ucsd.edu)