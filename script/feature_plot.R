##__________________________________________________________________
## Author: Ningxin Kang (nik010@ucsd.edu)       
## Last update: 2022-10-23    
## File: feature_plot.R          
## Functions: 
##  is_outlier(x)
##  plot_data(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output)
## File Summary:
##  This file include the code for plotting the features extracted.
##__________________________________________________________________

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
    
    ######################################
    ## dataset input and SEM calculation##
    ######################################
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
    
    ############################################################
    ## Using LMM to estimate the effect of the diet to feature##
    ############################################################
    library(lmerTest)
    library("report")
    per_pic$category <- as.factor(per_pic$category)
    per_pic$category <- relevel(per_pic$category, ref="CTRL")
    #Build LMM model
    model<-lmerTest::lmer(feature ~ 1 + category + (1|mouse_id), 
                data = per_pic, REML =TRUE)
    print("Result summary of LMM:")
    print(summary(model))
    print("Scentific intepretation of LMM result:")
    print(report::report(model))
    # Build nul LMM model for comparison
    model_null<-lmerTest::lmer(feature ~ 1 + (1|mouse_id), 
                data = per_pic, REML = TRUE)
    print("Anova result of comparison between two models:")
    print(anova(model,model_null))

    #############
    ## Plotting##
    #############
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

