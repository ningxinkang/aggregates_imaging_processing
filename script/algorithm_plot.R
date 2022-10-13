# load the library
library(ggplot2)
library(ggpubr)
library(patchwork)
library(rlang)
source("script/summarySE.R")


is_outlier <- function(x) {
  #print(x)
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


plot_data <- 
  function(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output) {
    # The errorbars overlapped, so use position_dodge to move them horizontally
    pd <- position_dodge(0.1)
    
    per_pic <- 
      read.csv(dir_per_pic, stringsAsFactors=FALSE)
    colnames(per_pic)[grep(feature, colnames(per_pic))] <- "feature"
    
    per_mouse <- 
      read.csv(dir_per_mouse, stringsAsFactors=FALSE)
    colnames(per_mouse)[grep(feature, colnames(per_mouse))] <- "feature"
    
    sample_summary <- 
      summarySE(per_pic, 
                measurevar="feature", groupvars=c("mouse_id","category"))
    colnames(sample_summary)[grep(feature, colnames(sample_summary))] <- "feature"
    
    category_summary <- 
      summarySE(per_pic, 
                measurevar="feature", groupvars=c("category"))
    colnames(category_summary)[grep(feature, colnames(category_summary))] <- "feature"

    # Draw number of aggregates per picture
    num_agg_1 <- 
      # select outliers
      per_pic %>% 
      mutate(outlier = ifelse(is_outlier(feature), picture_id, as.numeric(NA))) %>%
      # Add error bars and mean value to the graph
      ggplot(aes(x=mouse_id, y=feature, color = category)) +
      # Using standard error when drawing error bar
      geom_errorbar(data = sample_summary, color = "black",
                    aes(ymin=feature-se, ymax=feature+se), 
                    width=.1, position=pd)+
      geom_point(position=pd)+
      # add mean value
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
      # split channel for each categories
      facet_grid(~category,scales = 'free_x')+
      # label the outlier
      geom_text(aes(label = outlier), na.rm = TRUE, 
                vjust = 1.5, size = 2, color = "black")+
      
      # manege theme and y-axis
      ylim(0,ylim)+
      ylab(ytitle)+
      theme_bw()+
      scale_color_brewer(palette="Dark2")+
      theme(
        legend.position = "none",
        aspect.ratio = 2/1,
        axis.text.x = element_text(angle = 15, size = 7, vjust = 0.5, hjust = 0.75)
      )
    
    # draw the number of aggregates per mouse
    num_agg_2 <- 
      per_mouse %>%
      mutate(outlier = ifelse(is_outlier(feature), mouse_id, as.numeric(NA))) %>%
      # Add error bars and mean value to the graph
      ggplot(aes(x = category, y = feature, color = category)) +
      # Using standard error when drawing error bar
      geom_errorbar(data = category_summary, color = "black",
                    aes(ymin = feature - se, ymax = feature + se), 
                    width = .1, position = pd)+
      geom_point(position = pd) +
      # add mean value
      stat_summary(fun= mean, geom= "point", shape= 20, size= 2, color="black", fill="red") +
      # add t-test
      # diff bet t-test and wilcox
      stat_compare_means(method = "t.test") +
      # label the outlier
      geom_text(aes(label = outlier), na.rm = TRUE, 
                vjust = 1.5, size = 2, color = "black")+
      # edit on the features
      ylim(0,ylim)+
      theme_bw()+
      scale_color_brewer(palette="Dark2")+
      ylab(ytitle)+
      theme(legend.position = "none",
            aspect.ratio = 3/1
      )
    
    # draw the number of aggregates per mouse
    num_agg_3 <- 
      # select outliers
      per_pic %>%
      mutate(outlier = ifelse(is_outlier(feature), picture_id, as.numeric(NA))) %>%
      ggboxplot(x = "category",y = "feature",
                add = "jitter", color = "category")+
      
      # add t-test
      # diff bet t-test and wilcox
      stat_compare_means(method = "t.test")+
      # add mean value
      stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color="black", fill="black") +
      # label the outlier
      geom_text(aes(label = outlier), na.rm = TRUE, 
                vjust = 1.5, size = 2, color = "black")+
      
      # manege theme and y-axis
      ylim(0,ylim)+
      ylab(ytitle)+
      theme_bw()+
      scale_color_brewer(palette="Dark2")+
      theme(
        legend.position = "none",
        aspect.ratio = 3/1
      )
    
    # make two plots side-by-side
    num_agg_1 + num_agg_2 + num_agg_3
    ggsave(path = path_output, filename = name_output,width = 13, height = 8, device='png', dpi=700)
  }

###1#######################################
## 1. Average number of single aggregates##
###1#######################################
dir_per_pic = "input/v2/agg_per_pic.csv"
dir_per_mouse = "input/v2/agg_per_mouse.csv"
ytitle = "Number of aggregates"
ylim = 350
feature="num_aggregates"
path_output = "result/v3/"
name_output = "avg_num_agg.png"

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
ytitle = "Area (square micrometers)"
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
ytitle = "Area (square micrometer)"
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
source("script/feature_distribution.R")
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



