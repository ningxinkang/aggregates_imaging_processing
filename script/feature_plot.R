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
library(tibble)
source("script/summarySE.R")


is_outlier <- function(x) {
  #print(x)
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


plot_data <- 
  function(dir_per_pic, dir_per_mouse, ytitle, ylim, feature, path_output, name_output) {
    # The errorbars overlapped, so use position_dodge to move them horizontally
    pd <- position_dodge(0.1)
    pic_meta <- read.csv("input/pic_metadata.csv")
    mouse_meta <- read.csv("input/mouse_metadata.csv")
    
    ######################################
    ## dataset input and SEM calculation##
    ######################################
    per_pic <- 
      read.csv(dir_per_pic, stringsAsFactors=FALSE) 
    per_pic <- merge(per_pic,pic_meta, 
                     by = c("coverslip_id","category","mouse_id","picture_id"), all.y = TRUE)
    per_pic[[feature]][is.na(per_pic[[feature]])] <- 0
    per_pic <- per_pic %>%
      #  dplyr::filter(category != "plate1" & category != "plate2" & category != "plate3") %>% 
      dplyr::filter(mouse_id != "N104" & mouse_id != "N106")
    
    colnames(per_pic)[grep(feature, colnames(per_pic))] <- "feature"
    per_pic$category <- factor(per_pic$category,                                    # Change ordering manually
                               levels = c("2mm","5mm","6.5mm"))
    
    per_mouse <- 
      read.csv(dir_per_mouse, stringsAsFactors=FALSE) 
    per_mouse <- merge(per_mouse,mouse_meta, 
                       by = c("coverslip_id","category","mouse_id"), all.y = TRUE)
    per_mouse[[feature]][is.na(per_mouse[[feature]])] <- 0
    per_mouse <- per_mouse%>%
      #  dplyr::filter(category != "plate1" & category != "plate2" & category != "plate3") %>% 
      dplyr::filter(mouse_id != "N104" & mouse_id != "N106")
    colnames(per_mouse)[grep(feature, colnames(per_mouse))] <- "feature"
    per_mouse$category <- factor(per_mouse$category,                                    # Change ordering manually
                                 levels = c("2mm","5mm","6.5mm"))
    
    per_pic <- per_pic %>%
      mutate(x.axis = dplyr::case_when(per_pic$category == "6.5mm" ~ "void",
                                       per_pic$category == "5mm" ~ "TTE_5mm",
                                       per_pic$category == "2mm" ~ "TTE_2mm"))
    per_mouse <- per_mouse %>%
      mutate(x.axis = dplyr::case_when(per_mouse$category == "6.5mm" ~ "void",
                                       per_mouse$category == "5mm" ~ "TTE_5mm",
                                       per_mouse$category == "2mm" ~ "TTE_2mm"))
    
    sample_summary <- 
      summarySE(per_pic, 
                measurevar="feature", groupvars=c("mouse_id","x.axis"))
    colnames(sample_summary)[grep(feature, colnames(sample_summary))] <- "feature"
    
    category_summary <- 
      summarySE(per_pic, 
                measurevar="feature", groupvars=c("x.axis"))
    colnames(category_summary)[grep(feature, colnames(category_summary))] <- "feature"
    
    ############################################################
    ## Using LMM to estimate the effect of the diet to feature##
    ############################################################
    library(lmerTest)
    library("report")
    per_pic$category <- as.factor(per_pic$category)
    per_pic$category <- relevel(per_pic$category, ref="6.5mm")
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
    ##############################
    ## one-way ANOVA per picture##
    ##############################
    
    # Compute the analysis of variance
    res.aov.pic <- aov(feature ~ category+mouse_id, data = per_pic)
    # Summary of the analysis
    summary(res.aov.pic)
    
    # Tukey multiple pairwise-comparisons
    res.tukey.pic = TukeyHSD(res.aov.pic)
    library(gridExtra)
    res.tukey.pic = as.data.frame(res.tukey.pic$category) %>% 
      tibble::rownames_to_column("compare")%>%
      mutate(Annotation = "per_pic") %>%
      mutate(sig = dplyr::case_when(`p adj` < 0.001 ~ '***',
                                    `p adj` < 0.01 ~ '**',
                                    `p adj` < 0.05 ~ '*',
                                    `p adj` > 0.1 ~ 'NA')) %>%
      dplyr::filter(sig != "NA") %>%
      arrange(`p adj`)
    
    #table.pic = tableGrob(res.tukey.pic, rows=NULL,theme = ttheme_default(base_size = 8))
    
    ############################
    ## one-way ANOVA per mouse##
    ############################
    
    # Compute the analysis of variance
    res.aov.mouse <- aov(feature ~ category+mouse_id, data = per_mouse)
    # Summary of the analysis
    summary(res.aov.mouse)
    
    # Tukey multiple pairwise-comparisons
    res.tukey.mouse = TukeyHSD(res.aov.mouse)
    library(gridExtra)
    res.tukey.mouse = as.data.frame(res.tukey.mouse$category) %>% 
      tibble::rownames_to_column("compare") %>%
      mutate(Annotation = "per_mouse") %>%
      mutate(sig = dplyr::case_when(`p adj` < 0.001 ~ '***',
                                    `p adj` < 0.01 ~ '**',
                                    `p adj` < 0.05 ~ '*',
                                    `p adj` > 0.1 ~ 'NA')) %>%
      dplyr::filter(sig != "NA") %>%
      arrange(`p adj`)
    
    res.tukey <- dplyr::bind_rows(res.tukey.pic,res.tukey.mouse)
    if (dim(res.tukey)[1]>0){
      table = tableGrob(res.tukey, rows=NULL,theme = ttheme_default(base_size = 8))
    }
    #############
    ## Plotting##
    #############
    
    # Draw number of aggregates per picture
    num_agg_1 <- 
      # select outliers
      per_pic %>% 
      mutate(outlier = ifelse(is_outlier(feature), picture_id, as.numeric(NA))) %>%
      # Add error bars and mean value to the graph
      ggplot(aes(x=mouse_id, y=feature, color = x.axis)) +
      # Using standard error when drawing error bar
      #geom_errorbar(data = sample_summary, color = "black",
      #              aes(ymin=feature-se, ymax=feature+se), 
      #              width=.1, position=pd)+
      geom_point(position=pd, fill = NA)+
      # add mean value
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
      # split channel for each categories
      facet_grid(~x.axis,scales = 'free_x')+
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
        aspect.ratio = 4/3,
        axis.text.x = element_text(angle = 35, size = 7, vjust = 0.5, hjust = 0.75)
      )
    
    # draw the number of aggregates per mouse
    num_agg_2 <- 
      per_mouse %>%
      mutate(outlier = ifelse(is_outlier(feature), mouse_id, as.numeric(NA))) %>%
      # Add error bars and mean value to the graph
      ggplot(aes(x = x.axis, y = feature, color = x.axis)) +
      # Using standard error when drawing error bar
      geom_errorbar(data = category_summary, color = "black",
                    aes(ymin = feature - se, ymax = feature + se), 
                    width = .1, position = pd)+
      geom_point(position = pd, fill = NA) +
      # add mean value
      stat_summary(fun= mean, geom= "point", shape= 20, size= 2, color="black", fill="red") +
      # add t-test
      # diff bet t-test and wilcox
      #stat_compare_means(method = "t.test") +
      # label the outlier
      geom_text(aes(label = outlier), na.rm = TRUE, 
                vjust = 1.5, size = 2, color = "black")+
      # edit on the features
      ylim(0,ylim)+
      theme_bw()+
      scale_color_brewer(palette="Dark2")+
      ylab(ytitle)+
      theme(legend.position = "none",
            aspect.ratio = 4/3,
            axis.text.x = element_text(angle = 35, size = 7, vjust = 0.5, hjust = 0.75)
      )
    
    # draw the number of aggregates per mouse
    num_agg_3 <- 
      # select outliers
      per_pic %>%
      mutate(outlier = ifelse(is_outlier(feature), picture_id, as.numeric(NA))) %>%
      ggboxplot(x = "x.axis",y = "feature",
                add = "jitter", color = "x.axis")+
      
      # add t-test
      # diff bet t-test and wilcox
      #stat_compare_means(method = "t.test")+
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
        aspect.ratio = 4/3,
        axis.text.x = element_text(angle = 35, size = 7, vjust = 0.5, hjust = 0.75)
      )
    
    # make two plots side-by-side
    if (dim(res.tukey)[1] >0){
      num_agg_1 / (num_agg_2 + num_agg_3 + table)
    } else{
      num_agg_1 / (num_agg_2 + num_agg_3)
    }
    ggsave(path = path_output, filename = name_output,width = 13, height = 8, device='png', dpi=700)
  }

