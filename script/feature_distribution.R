# load the library
library(ggplot2)
library(ggpubr)
library(patchwork)
library(rlang)
library(tidyr)
feature_distribution <- function(feature,dir_input, dir_output, name_output){
  
  GFP <- read.csv(dir_input, stringsAsFactors=FALSE)
  colnames(GFP)[which(colnames(GFP) ==feature)] <- "feature"
  
  is_outlier <- function(x) {
    #print(x)
    return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
  }
  
  GFP_BHB <- GFP %>% dplyr::filter(category == "BHB") %>%
    mutate(outlier = ifelse(is_outlier(feature), picture_id, as.numeric(NA)))
  
  GFP_CTRL <- GFP %>% dplyr::filter(category == "CTRL") %>%
    mutate(outlier = ifelse(is_outlier(feature), picture_id, as.numeric(NA)))
  
  GFP <- dplyr::bind_rows(GFP_BHB, GFP_CTRL)
  
  # select outliers
  output <- GFP %>%
    ggplot(aes(x = category, y = feature,color = category))+
    geom_violin(scale = "area",
                aes(fill = category), alpha = 0.5,color = "transparent")+
    geom_boxplot(outlier.shape = NA,fill = "transparent")+  # NO OUTLIERS
    # Outliers
    geom_point(data = GFP %>% drop_na(outlier), position = 'jitter', na.rm = TRUE, alpha = 0.5)+
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


