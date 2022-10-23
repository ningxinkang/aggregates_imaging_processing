##__________________________________________________________________
## Author: Ningxin Kang (nik010@ucsd.edu)       
## Last update: 2022-10-23    
## File: feature_distribution.R          
## File Summary:
##  This file include the code for plotting distribution of features.
##__________________________________________________________________

# load the library
library(ggplot2)
library(ggpubr)
library(patchwork)
library(rlang)
library(tidyr)

feature_distribution <- function(feature,dir_input, dir_output, name_output){
  
  GFP <- read.csv(dir_input, stringsAsFactors=FALSE)
  colnames(GFP)[which(colnames(GFP) ==feature)] <- "feature"
  ############################################################
  ## Using LMM to estimate the effect of the diet to feature##
  ############################################################
  library(lmerTest)
  library("report")
  GFP$category <- as.factor(GFP$category)
  GFP$category <- relevel(GFP$category, ref="CTRL")
  #Build LMM model
  model<-lmerTest::lmer(feature ~ 1 + category + (1|mouse_id), 
                        data = GFP, REML =TRUE)
  print("Result summary of LMM:")
  print(summary(model))
  print("Scentific intepretation of LMM result:")
  print(report::report(model))
  # Build nul LMM model for comparison
  model_null<-lmerTest::lmer(feature ~ 1 + (1|mouse_id), 
                             data = GFP, REML = TRUE)
  print("Anova result of comparison between two models:")
  print(anova(model,model_null))
  
  #############
  ## Plotting##
  #############
  # select outliers
  output <- GFP %>%
    ggplot(aes(x = category, y = feature,color = category))+
    geom_violin(scale = "area",
                aes(fill = category), alpha = 0.5,color = "transparent")+
    geom_boxplot(outlier.shape = NA,fill = "transparent")+  # NO OUTLIERS
    # add t-test
    # diff bet t-test and wilcox
    stat_compare_means(method = "t.test")+
    # add mean value
    stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color="black", fill="black") +
    
    # manege theme and y-axis
    ylim(0,NA)+
    ylab("Area of single aggregates (square centimeters)")+
    theme_bw()+
    scale_color_brewer(palette="Dark2")+
    scale_fill_brewer(palette="Dark2")+
    theme(
      legend.position = "none",
      aspect.ratio = 3/1
    )
  output
  ggsave(path = dir_output, filename = name_output,width = 3, height = 8, device='png', dpi=700)
}


