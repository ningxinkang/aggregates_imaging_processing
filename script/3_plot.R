##__________________________________________________________________
## Author: Ningxin Kang (nik010@ucsd.edu)       
## Last update: 2022-10-13    
## File: 3_plot.R          
## File Summary:
##  This file include the code for plotting the features extracted.
##__________________________________________________________________

# load the library
library(ggplot2)
library(ggpubr)
library(patchwork)
library(rlang)
source("script/summarySE.R")
source("script/feature_plot.R")
source("script/feature_distribution.R")

###1#######################################
## 1. Average number of single aggregates##
###1#######################################
dir_per_pic = "input/v2/agg_per_pic.csv"
dir_per_mouse = "input/v2/agg_per_mouse.csv"
ytitle = "Number of aggregates"
ylim = 350
feature="num_aggregates"
path_output = "result/v3/"
name_output = "avg_num_agg_anova.png"

plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)

###2#################################################
## 2. Total number per DAPI of aggregates on plate###
###2#################################################
dir_per_pic = "input/v2/agg_vs_DAPI_per_pic.csv"
dir_per_mouse = "input/v2/agg_vs_DAPI_per_mouse.csv"
ytitle = "Total number of aggregates/DAPI"
ylim = 0.17
feature="num_aggregates_vs_DAPI"
path_output = "result/v3/"
name_output = "num_agg_per_DAPI.png"

plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)

###3######################################
## 3. Average area of single aggregates##
###3######################################
dir_per_pic = "input/v2/avg_area_per_pic.csv"
dir_per_mouse = "input/v2/avg_area_per_mouse.csv"
ytitle = "Area (square microns)"
ylim = 55
feature="Area"
path_output = "result/v3/"
name_output = "avg_area.png"

plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)

###4###################################################
## 4. Average Integrated density of single aggregates##
###4###################################################
dir_per_pic = "input/v2/avg_IntDen_per_pic.csv"
dir_per_mouse = "input/v2/avg_IntDen_per_mouse.csv"
ytitle = "Integrated Density"
ylim = 1.5e-05
feature="IntDen"
path_output = "result/v3/"
name_output = "avg_IntDen.png"

plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)

###5######################################
## 5. Total area of aggregates one plate##
###5######################################
dir_per_pic = "input/v2/total_area_per_pic.csv"
dir_per_mouse = "input/v2/total_area_per_mouse.csv"
ytitle = "Area (square microns)"
ylim = 7000
feature="Area"
path_output = "result/v3/"
name_output = "total_area.png"

plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)

###6####################################################
## 6. Total Integrated Density of aggregates one plate##
###6####################################################
dir_per_pic = "input/v2/total_IntDen_per_pic.csv"
dir_per_mouse = "input/v2/total_IntDen_per_mouse.csv"
ytitle = "Integrated Density"
ylim = 0.00175
feature="IntDen"
path_output = "result/v3/"
name_output = "total_IntDen.png"

plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)
###7###############################################
## 7. Total Area per DAPI of aggregates one plate##
###7###############################################
dir_per_pic = "input/v2/area_vs_DAPI_per_pic.csv"
dir_per_mouse = "input/v2/area_vs_DAPI_per_mouse.csv"
ytitle = "Total area of aggregates/DAPI"
ylim = 4
feature="Area_vs_DAPI"
path_output = "result/v3/"
name_output = "area_per_DAPI.png"

plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)

###8#################################################
## 8. Total IntDen per DAPI of aggregates one plate##
###8#################################################
dir_per_pic = "input/v2/IntDen_vs_DAPI_per_pic.csv"
dir_per_mouse = "input/v2/IntDen_vs_DAPI_per_mouse.csv"
ytitle = "Total integrated density of aggregates/DAPI"
ylim = 9e-07
feature="IntDen_vs_DAPI"
path_output = "result/v3/"
name_output = "IntDen_per_DAPI.png"

plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)

###9#####################
## 9. Area of aggregates#
###9#####################
dir_input = "input/GFP.csv"
dir_output = "result/v3"
name_output = "area_distribution.png"
feature = "Area"

feature_distribution(feature,dir_input, dir_output, name_output)

###10#####################
## 10. IntDen of aggregates#
###10#####################
dir_input = "input/GFP.csv"
dir_output = "result/v3"
name_output = "IntDen_distribution.png"
feature = "IntDen"

feature_distribution(feature,dir_input, dir_output, name_output)







