# Imaging processing
## About
This set of script is used to take in a set of imaging result treated after ImageJ, formating and documenting individual datasheets into a large dataset, cleaning and extracting the features we want to look at, and generating the graphs.

## Imaging processing protocol
1.	Separate the GFP result and the DAPI result into two folders.
2.	For each folder, create a .xls file name `filepath.xls`
3.	For each .xls file, we want to create a table with only one column, with the column name `filepath`.
4.	Open your terminal. Go to the directory of your GFP result (or DAPI result) by typing `cd dir_of_your_choice`.
5.	Type `ls -d $PWD/*` to let the system print out the absolute path of files in the directory.
6.	Copy the absolute paths of the files into the filepath.xls we just created.
7.	
