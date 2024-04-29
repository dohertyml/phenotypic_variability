#### Montastrea Cavernosa Boxplot and analysis

######### MONTASTREA CAVERNOSA

################ BOXPLOT AND ANOVAS

library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)

# import data set 

mightyscope_data <- read_csv("data/mightyscope_data.csv")

#filter for mcav and past 

mcav_df <- mightyscope_data %>% filter(species %in% "montastrea_cavernosa")

###### mcav septa width boxplot 

mcav_sem_width <- read_csv("data/mcav_sem_width.csv")

mcav_mightyscope_sep <- read_csv("data/mcav_mightyscope_new.csv")

##### major septa
mcav_major_septa_length <- mcav_mightyscope_sep %>% dplyr::select(frag_code, depth, site, length_septa)

### remove outliers

# cor
quartiles <- quantile(mcav_df$corralite_diameter_cm, probs = c(.25,.75))
IQR_cor <- IQR(mcav_df$corralite_diameter_cm)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

cor_no_outlier <- subset(mcav_df, mcav_df$corralite_diameter_cm >lower & mcav_df$corralite_diameter_cm< upper)
cor_no_outlier$log_cor <- log(cor_no_outlier$corralite_diameter_cm+1)
dim(cor_no_outlier)

### col

quartiles <- quantile(mcav_df$columnella_diameter_cm, probs = c(.25,.75))
IQR_col <- IQR(mcav_df$columnella_diameter_cm)

lower <- quartiles[1] - 1.5*IQR_col
upper <- quartiles[2] + 1.5*IQR_col

col_no_outlier <- subset(mcav_df, mcav_df$columnella_diameter_cm >lower & mcav_df$columnella_diameter_cm< upper)
col_no_outlier$log_col <- log(col_no_outlier$columnella_diameter_cm+1)

dim(col_no_outlier)

### sep

quartiles <- quantile(mcav_df$septa_height, probs = c(.25,.75))
IQR_sep <- IQR(mcav_df$septa_height)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

sep_no_outlier <- subset(mcav_df, mcav_df$septa_height >lower & mcav_df$septa_height< upper)
sep_no_outlier$log_sep <- log(sep_no_outlier$septa_height +1)

dim(sep_no_outlier)

### dis

quartiles <- quantile(mcav_df$distance_between_polyp, probs = c(.25,.75))
IQR_dis <- IQR(mcav_df$distance_between_polyp)

lower <- quartiles[1] - 1.5*IQR_dis
upper <- quartiles[2] + 1.5*IQR_dis

dis_no_outlier <- subset(mcav_df, mcav_df$distance_between_polyp >lower & mcav_df$distance_between_polyp< upper)
dis_no_outlier$log_dis <- log(dis_no_outlier$distance_between_polyp+1)

dim(dis_no_outlier)

###septa width
quartiles <- quantile(mcav_sem_width $septa_width, probs = c(.25,.75))
IQR_cor <- IQR(mcav_sem_width$septa_width)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

width_no_outlier <- subset(mcav_sem_width, mcav_sem_width$septa_width >lower & mcav_sem_width$septa_width< upper)

mean_cor_df <- cor_no_outlier %>% group_by(depth) %>% 
  summarise(mean_cor_diameter = mean(corralite_diameter_cm), sd = sd(corralite_diameter_cm), 
            se = sd/sqrt(length(corralite_diameter_cm)))
dim(width_no_outlier)

## major septa length
quartiles <- quantile(mcav_major_septa_length$length_septa, probs = c(.25,.75))
IQR_dis <- IQR(mcav_major_septa_length$length_septa,)

lower <- quartiles[1] - 1.5*IQR_dis
upper <- quartiles[2] + 1.5*IQR_dis

maj_sep_no_outlier <- subset(mcav_major_septa_length ,mcav_major_septa_length$length_septa >lower & mcav_major_septa_length$length_septa< upper)

dim(maj_sep_no_outlier)

## boxplot by depth 

mcav_col <- ggplot(col_no_outlier, aes(x = fct_rev(as.factor(depth)), y = columnella_diameter_cm*10, 
                                       fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") +
  xlab("Depth") + ylab("Columnella diameter (mm)") +
  coord_flip() +
  scale_fill_brewer() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)


mcav_cor <- ggplot(cor_no_outlier, aes(x = fct_rev(as.factor(depth)), y = corralite_diameter_cm*10,
                                       fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") +
  xlab("Depth") + ylab("Corralite diameter (mm)")  +
  coord_flip() +
  scale_fill_brewer() +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)


mcav_septa <- ggplot(sep_no_outlier, aes(x = fct_rev(as.factor(depth)), y = septa_height*10,
                                         fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") +
  xlab("Depth") + ylab("Septa height (mm)") +
  coord_flip()  +
  scale_fill_brewer() +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)



mcav_dist <- ggplot(dis_no_outlier, aes(x = fct_rev(as.factor(depth)), y = distance_between_polyp*10, 
                                        fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") +
  xlab("Depth") + ylab("Distance between polyps (mm)") +
  coord_flip() +
  scale_fill_brewer() +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)


mcav_septa_width <- ggplot(width_no_outlier, aes(x = fct_rev(as.factor(depth)), y = septa_width*10, 
                                                 fill = as.factor(depth))) +
  geom_boxplot() + 
  scale_fill_brewer() + 
  theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("") + ylab("Septa width (mm)") +
  coord_flip() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)


major_septa_length<- ggplot(maj_sep_no_outlier , aes(x = fct_rev(as.factor(depth)), y = length_septa*10, 
                                                     fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("") + ylab("Major septa length (mm)") +
  coord_flip()  +
  scale_fill_brewer() +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)



mon_fig <- ggarrange(mcav_cor + rremove("ylab"), mcav_col + rremove("ylab"), mcav_septa + rremove("ylab"),
                     mcav_dist + rremove("ylab"),  mcav_septa_width + rremove("ylab"), major_septa_length + rremove("ylab"),
                     ncol = 2, nrow = 3)



annotate_figure(mon_fig, left = textGrob("Depth (m)", rot = 90, vjust = 0.5, 
                                         gp = gpar(fontfamily = "TT Arial", cex = 1.2)))

## coralite statistical analysis

mcav_cor_aov <- aov(data = cor_no_outlier, corralite_diameter_cm ~ as.factor(depth))
summary(mcav_cor_aov)
cor_residuals <-mcav_cor_aov$residuals
shapiro.test(log(cor_residuals + 1))
cor_krus <- kruskal.test(data = cor_no_outlier, corralite_diameter_cm ~ as.factor(depth))
dunns_cor <- dunn.test(cor_no_outlier$corralite_diameter_cm, as.factor(cor_no_outlier$depth), list = TRUE)


#p 0.0000000

# mcav columnella

mcav_col_aov <- aov(data = col_no_outlier, columnella_diameter_cm ~ as.factor(depth))
summary(mcav_col_aov)
col_residuals <- mcav_col_aov$residuals
shapiro.test(log(col_residuals + 1))
kruskal.test(data = col_no_outlier, corralite_diameter_cm ~ as.factor(depth))
dunns_col <- dunn.test(col_no_outlier$corralite_diameter_cm, as.factor(col_no_outlier$depth), list = TRUE)

#mcav_septa_height

mcav_sep_aov <- aov(data= sep_no_outlier, log(septa_height) ~ as.factor(depth))
summary(mcav_col_aov)
sep_residuals <- mcav_sep_aov$residuals
shapiro.test(sep_residuals)
summary(mcav_sep_aov)
mcav_sep_tukey <- TukeyHSD(mcav_sep_aov)
mcav_sep_tukey <- mcav_sep_tukey %>% tidy() 
mcav_sep_sig <- mcav_sep_tukey %>% filter(!adj.p.value >0.05 )


# p 0.002255 

#mcav_distance_height

mcav_dis_aov <- aov(data = dis_no_outlier, log(distance_between_polyp) ~ as.factor(depth))
summary(mcav_col_aov)
dis_residuals<- mcav_dis_aov$residuals
shapiro.test(dis_residuals)
mcav_dis_tukey <- TukeyHSD(mcav_dis_aov)

mcav_dis_tukey <- mcav_dis_tukey %>% tidy() %>% filter(!adj.p.value >0.05 )
mcav_dis_tukey$parameter <- "Distance between polyps (cm)"

# p 0.9955

## mcav_septa_width

mcav_width_aov <- aov(data = width_no_outlier, septa_width ~ as.factor(depth))
summary(mcav_width_aov)
dis_residuals<- mcav_width_aov$residuals
shapiro.test(dis_residuals)
mcav_width_tukey <- TukeyHSD(mcav_width_aov)

mcav_septa_width_output <- mcav_width_tukey %>% tidy() %>% filter(!adj.p.value >0.05 )

# data analysis major septa length

maj_sep_aov <- aov(data = maj_sep_no_outlier, length_septa ~ as.factor(depth))
summary(maj_sep_aov)
maj_sep_residual <- maj_sep_aov$residuals
shapiro.test(maj_sep_residual)
maj_sep_tuk <- TukeyHSD(maj_sep_aov)
mcav_maj_sep_tuk <- maj_sep_tuk %>% tidy() %>% filter(!adj.p.value > 0.05)
