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
library(dplyr)

feature_distribution <- function(feature,dir_input, dir_output, name_output,ytitle){
  
  GFP <- read.csv(dir_input, stringsAsFactors=FALSE) %>%
    #  dplyr::filter(category != "plate1" & category != "plate2" & category != "plate3") %>% 
    dplyr::filter(mouse_id != "N104" & mouse_id != "N106")
  GFP$category <- factor(GFP$category,                                    # Change ordering manually
                         levels = c("2mm","5mm","6.5mm"))
  colnames(GFP)[which(colnames(GFP) ==feature)] <- "feature"
  ############################################################
  ## Using LMM to estimate the effect of the diet to feature##
  ############################################################
  library(lmerTest)
  library("report")
  GFP$category <- as.factor(GFP$category)
  GFP$category <- relevel(GFP$category, ref="6.5mm")   # change reference accordingly
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
  
  ##############################
  ## one-way ANOVA per picture##
  ##############################
  
  # Compute the analysis of variance
  res.aov <- aov(feature ~ category+mouse_id, data = GFP)
  # Summary of the analysis
  summary(res.aov)
  
  # Tukey multiple pairwise-comparisons
  res.tukey = TukeyHSD(res.aov)
  library(gridExtra)
  res.tukey = as.data.frame(res.tukey$category) %>% 
    tibble::rownames_to_column("compare")%>%
    mutate(sig = dplyr::case_when(`p adj` < 0.001 ~ '***',
                                  `p adj` < 0.01 ~ '**',
                                  `p adj` < 0.05 ~ '*',
                                  `p adj` > 0.1 ~ 'NA')) %>%
    dplyr::filter(sig != "NA") %>%
    arrange(`p adj`)
  
  if (dim(res.tukey)[1]>0){
    table = tableGrob(res.tukey, rows=NULL,theme = ttheme_default(base_size = 8))
  }
  
  #############
  ## Plotting##
  #############
  # select outliers
  output <- GFP %>% mutate(x.axis = dplyr::case_when(GFP$category == "6.5mm" ~ "void",
                                                     GFP$category == "5mm" ~ "TTE_5mm",
                                                     GFP$category == "2mm" ~ "TTE_2mm"))%>%
    ggplot(aes(x = x.axis, y = feature,color = x.axis))+
    geom_violin(scale = "area",
                aes(fill = x.axis), alpha = 0.5,color = "transparent")+
    geom_boxplot(outlier.shape = NA,fill = "transparent")+  # NO OUTLIERS
    # add t-test
    # diff bet t-test and wilcox
    #stat_compare_means(method = "t.test")+
    # add mean value
    stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color="black", fill="black") +
    
    # manege theme and y-axis
    ylim(0,NA)+
    ylab(ytitle)+
    theme_bw()+
    xlab("Treatment")+
    scale_color_brewer(palette="Dark2")+
    scale_fill_brewer(palette="Dark2")+
    theme(
      legend.position = "none",
      aspect.ratio = 4/3,
      axis.text.x = element_text(angle = 35, size = 7, vjust = 0.5, hjust = 0.75)
    )
  if (dim(res.tukey)[1] >0){
    output + table
  } else{
    output
  }
  ggsave(path = dir_output, filename = name_output,width = 11, height = 8, device='png', dpi=700)
}


