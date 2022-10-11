# load the library
library(ggplot2)
library(ggpubr)
library(patchwork)
source("script/summarySE.R")

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1)
###1#################################
## 1. Average number of aggregates##
###1#################################
avg_agg_per_pic <- 
  read.csv("input/avg_agg_per_pic.csv",stringsAsFactors=FALSE)
head(avg_agg_per_pic)

avg_agg_per_mouse <- 
  read.csv("input/avg_agg_per_mouse.csv",stringsAsFactors=FALSE)
head(avg_agg_per_mouse)

sample_summary <- 
  summarySE(avg_agg_per_pic, 
            measurevar="num_aggregates", groupvars=c("mouse_id","category"))
category_summary <- 
  summarySE(avg_agg_per_pic, 
            measurevar="num_aggregates", groupvars=c("category"))

# Draw number of aggregates per picture
num_agg_1 <- 
  # select outliers
  avg_agg_per_pic %>%
  mutate(outlier = ifelse(is_outlier(num_aggregates), picture_id, as.numeric(NA))) %>%
  # Add error bars and mean value to the graph
  ggplot(aes(x=mouse_id, y=num_aggregates, color = category)) +
  # Using standard error when drawing error bar
  geom_errorbar(data = sample_summary, color = "black",
                aes(ymin=num_aggregates-se, ymax=num_aggregates+se), 
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
  ylim(0,350)+
  ylab("Number of aggregates")+
  theme_bw()+
  scale_color_brewer(palette="Dark2")+
  theme(
    legend.position = "none",
    aspect.ratio = 2/1,
    axis.text.x = element_text(angle = 15,size = 7,vjust = 0.5,hjust = 0.75)
  )

# draw the number of aggregates per mouse
num_agg_2 <- 
  avg_agg_per_mouse %>%
  mutate(outlier = ifelse(is_outlier(num_aggregates), mouse_id, as.numeric(NA))) %>%
  # Add error bars and mean value to the graph
  ggplot(aes(x=category, y=num_aggregates, color = category)) +
  # Using standard error when drawing error bar
  geom_errorbar(data = category_summary, color = "black",
                aes(ymin=num_aggregates-se, ymax=num_aggregates+se), 
                width=.1, position=pd)+
  geom_point(position=pd)+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="red") +
  # add t-test
  # diff bet t-test and wilcox
  stat_compare_means(method = "t.test")+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2, color = "black")+
  # edit on the features
  ylim(0,350)+
  theme_bw()+
  scale_color_brewer(palette="Dark2")+
  ylab("Number of aggregates")+
  theme(legend.position = "none",
        aspect.ratio = 3/1
        )

# draw the number of aggregates per mouse
num_agg_3 <- 
  # select outliers
  avg_agg_per_pic %>%
  mutate(outlier = ifelse(is_outlier(num_aggregates), picture_id, as.numeric(NA))) %>%
  ggboxplot(x = "category",y = "num_aggregates",
            add = "jitter", color = "category")+
  
  # add t-test
  # diff bet t-test and wilcox
  stat_compare_means(method = "t.test")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2, color = "black")+
  
  # manege theme and y-axis
  ylim(0,350)+
  ylab("Number of aggregates")+
  theme_bw()+
  scale_color_brewer(palette="Dark2")+
  theme(
    legend.position = "none",
    aspect.ratio = 3/1
  )

# make two plots side-by-side
num_agg_1 + num_agg_2 + num_agg_3
ggsave(path = 'result/v2/', filename = 'avg_num_agg.png',width = 13, height = 8, device='png', dpi=700)

###2######################################
## 2. Average area of single aggregates##
###2######################################
avg_area_IntDen_per_mouse <- 
  read.csv("input/avg_area&IntDen_per_mouse.csv",stringsAsFactors=FALSE)
head(avg_area_IntDen_per_mouse)

avg_area_IntDen_per_pic <- 
  read.csv("input/avg_area&IntDen_per_pic.csv",stringsAsFactors=FALSE)
head(avg_area_IntDen_per_pic)

# Draw average area per picture
avg_single_area_1 <- 
  avg_area_IntDen_per_pic %>%
  mutate(outlier = ifelse(is_outlier(Area), picture_id, as.numeric(NA))) %>%
  ggboxplot(x = "mouse_id",y = "Area",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  # split channel for each categories
  facet_grid(~category,scales = 'free_x')+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  
  # manege theme and y-axis
  ylim(0,55)+
  ylab("Area (square micrometers)")+
  theme_bw()+
  theme(
    legend.position = "none",
    aspect.ratio = 2/1,
    axis.text.x = element_text(angle = 15,size = 7,vjust = 0.5,hjust = 0.75)
  )

# draw average area per mouse
avg_single_area_2 <- 
  avg_area_IntDen_per_mouse %>%
  mutate(outlier = ifelse(is_outlier(Area), mouse_id, as.numeric(NA))) %>%
  ggboxplot(x = "category",y = "Area",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="red") +
  # add t-test
  # diff bet t-test and wilcox
  stat_compare_means(method = "t.test")+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  # edit on the features
  ylim(0,55)+
  theme_bw()+
  ylab("Area (square micrometers)")+
  theme(legend.position = "none",
        aspect.ratio = 3/1
  )

# make two plots side-by-side
avg_single_area_1 + avg_single_area_2
ggsave(path = 'result/', filename = 'avg_single_area.png',width = 10, height = 8, device='png', dpi=700)
###3###################################################
## 3. Average Integrated density of single aggregates##
###3###################################################

# Draw average area per picture
avg_single_IntDen_1 <- 
  avg_area_IntDen_per_pic %>%
  mutate(outlier = ifelse(is_outlier(IntDen), picture_id, as.numeric(NA))) %>%
  ggboxplot(x = "mouse_id",y = "IntDen",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  # split channel for each categories
  facet_grid(~category,scales = 'free_x')+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  
  # manege theme and y-axis
  ylim(0,1.5e-05)+
  ylab("Integrated Density")+
  theme_bw()+
  theme(
    legend.position = "none",
    aspect.ratio = 2/1,
    axis.text.x = element_text(angle = 15,size = 7,vjust = 0.5,hjust = 0.75)
  )

# draw average area per mouse
avg_single_IntDen_2 <- 
  avg_area_IntDen_per_mouse %>%
  mutate(outlier = ifelse(is_outlier(IntDen), mouse_id, as.numeric(NA))) %>%
  ggboxplot(x = "category",y = "IntDen",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="red") +
  # add t-test
  # diff bet t-test and wilcox
  stat_compare_means(method = "t.test")+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  # edit on the features
  ylim(0,1.5e-05)+
  theme_bw()+
  ylab("Integrated Density")+
  theme(legend.position = "none",
        aspect.ratio = 3/1
  )

# make two plots side-by-side
avg_single_IntDen_1 + avg_single_IntDen_2
ggsave(path = 'result/', filename = 'avg_single_IntDen.png',width = 10, height = 8, device='png', dpi=700)
###4######################################
## 4. Total area of aggregates one plate##
###4######################################
total_area_IntDen_per_mouse <- 
  read.csv("input/total_area&IntDen_per_mouse.csv",stringsAsFactors=FALSE)
head(total_area_IntDen_per_mouse)

total_area_IntDen_per_pic <- 
  read.csv("input/total_area&IntDen_per_pic.csv",stringsAsFactors=FALSE)
head(total_area_IntDen_per_pic)

# Draw average area per picture
total_area_1 <- 
  total_area_IntDen_per_picture %>%
  mutate(outlier = ifelse(is_outlier(Area), picture_id, as.numeric(NA))) %>%
  ggboxplot(x = "mouse_id",y = "Area",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  # split channel for each categories
  facet_grid(~category,scales = 'free_x')+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  
  # manege theme and y-axis
  ylim(0,7000)+
  ylab("Area (square micrometers)")+
  theme_bw()+
  theme(
    legend.position = "none",
    aspect.ratio = 2/1,
    axis.text.x = element_text(angle = 15,size = 7,vjust = 0.5,hjust = 0.75)
  )

# draw average area per mouse
total_area_2 <- 
  total_area_IntDen_per_mouse %>%
  mutate(outlier = ifelse(is_outlier(Area), mouse_id, as.numeric(NA))) %>%
  ggboxplot(x = "category",y = "Area",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="red") +
  # add t-test
  # diff bet t-test and wilcox
  stat_compare_means(method = "t.test")+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  # edit on the features
  theme_bw()+
  ylim(0,7000)+
  ylab("Area (square micrometers)")+
  theme(legend.position = "none",
        aspect.ratio = 3/1
  )

# make two plots side-by-side
total_area_1 + total_area_2
ggsave(path = 'result/', filename = 'total_area.png',width = 10, height = 8, device='png', dpi=700)
###5####################################################
## 5. Total Integrated Density of aggregates one plate##
###5####################################################
# Draw average area per picture
total_IntDen_1 <- 
  total_area_IntDen_per_pic %>%
  mutate(outlier = ifelse(is_outlier(IntDen), picture_id, as.numeric(NA))) %>%
  ggboxplot(x = "mouse_id",y = "IntDen",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  # split channel for each categories
  facet_grid(~category,scales = 'free_x')+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  
  # manege theme and y-axis
  ylim(0,0.00175)+
  ylab("Area (square micrometers)")+
  theme_bw()+
  theme(
    legend.position = "none",
    aspect.ratio = 2/1,
    axis.text.x = element_text(angle = 15,size = 7,vjust = 0.5,hjust = 0.75)
  )

# draw average area per mouse
total_IntDen_2 <- 
  total_area_IntDen_per_mouse %>%
  mutate(outlier = ifelse(is_outlier(IntDen), mouse_id, as.numeric(NA))) %>%
  ggboxplot(x = "category",y = "IntDen",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="red") +
  # add t-test
  # diff bet t-test and wilcox
  stat_compare_means(method = "t.test")+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  # edit on the features
  theme_bw()+
  ylim(0,0.00175)+
  ylab("Area (square micrometers)")+
  theme(legend.position = "none",
        aspect.ratio = 3/1
  )

# make two plots side-by-side
total_IntDen_1 + total_IntDen_2
ggsave(path = 'result/', filename = 'total_IntDen.png',width = 10, height = 8, device='png', dpi=700)
###6###############################################
## 6. Total Area per DAPI of aggregates one plate##
###6###############################################
area_IntDen_per_DAPI_per_pic <- 
  read.csv("input/area&IntDen_vs_DAPI_per_pic.csv",stringsAsFactors=FALSE)
head(area_IntDen_per_DAPI_per_pic)

area_IntDen_per_DAPI_per_mouse <- 
  read.csv("input/area&IntDen_vs_DAPI_per_mouse.csv",stringsAsFactors=FALSE)
head(area_IntDen_per_DAPI_per_mouse)

# Draw average area per picture
area_per_DAPI_1 <- 
  area_IntDen_per_DAPI_per_pic %>%
  mutate(outlier = ifelse(is_outlier(area_vs_DAPI), picture_id, as.numeric(NA))) %>%
  ggboxplot(x = "mouse_id",y = "area_vs_DAPI",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
  # split channel for each categories
  facet_grid(~category,scales = 'free_x')+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  
  # manege theme and y-axis
  #ylim(0,0.00175)+
  ylab("Total area of aggregates/DAPI")+
  theme_bw()+
  theme(
    legend.position = "none",
    aspect.ratio = 2/1,
    axis.text.x = element_text(angle = 15,size = 7,vjust = 0.5,hjust = 0.75)
  )

# draw average area per mouse
area_per_DAPI_2 <- 
  area_IntDen_per_DAPI_per_mouse %>%
  mutate(outlier = ifelse(is_outlier(area_vs_DAPI), mouse_id, as.numeric(NA))) %>%
  ggboxplot(x = "category",y = "area_vs_DAPI",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="red") +
  # add t-test
  # diff bet t-test and wilcox
  stat_compare_means(method = "t.test")+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  # edit on the features
  theme_bw()+
  #ylim(0,0.00175)+
  ylab("Total area of aggregates/DAPI")+
  theme(legend.position = "none",
        aspect.ratio = 3/1
  )

# make two plots side-by-side
area_per_DAPI_1 + area_per_DAPI_2
ggsave(path = 'result/', filename = 'area_per_DAPI.png',width = 10, height = 8, device='png', dpi=700)
###7#################################################
## 7. Total IntDen per DAPI of aggregates one plate##
###7#################################################
IntDen_per_DAPI_1 <- 
  area_IntDen_per_DAPI_per_pic %>%
  mutate(outlier = ifelse(is_outlier(IntDen_vs_DAPI), picture_id, as.numeric(NA))) %>%
  ggboxplot(x = "mouse_id",y = "IntDen_vs_DAPI",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, 
               color="black", fill="black") +
  # Label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  # split channel for each categories
  facet_grid(~category,scales = 'free_x')+
  
  # manege theme and y-axis
  #ylim(0,0.00175)+
  ylab("Total integrated density of aggregates/DAPI")+
  theme_bw()+
  theme(
    legend.position = "none",
    aspect.ratio = 2/1,
    axis.text.x = element_text(angle = 15,size = 7,vjust = 0.5,hjust = 0.75)
  )

# draw average area per mouse
IntDen_per_DAPI_2 <- 
  area_IntDen_per_DAPI_per_mouse %>%
  mutate(outlier = ifelse(is_outlier(IntDen_vs_DAPI), mouse_id, as.numeric(NA))) %>%
  ggboxplot(x = "category",y = "IntDen_vs_DAPI",
            add = "jitter", color = "category", palette = "jco")+
  # add mean value
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="red") +
  # add t-test
  # diff bet t-test and wilcox
  stat_compare_means(method = "t.test")+
  # label the outlier
  geom_text(aes(label = outlier), na.rm = TRUE, 
            vjust = 1.5, size = 2)+
  
  # edit on the features
  theme_bw()+
  #ylim(0,0.00175)+
  ylab("Total integrated density of aggregates/DAPI")+
  theme(legend.position = "none",
        aspect.ratio = 3/1
  )

# make two plots side-by-side
IntDen_per_DAPI_1 + IntDen_per_DAPI_2
ggsave(path = 'result/', filename = 'IntDen_per_DAPI.png',width = 10, height = 8, device='png', dpi=700)
