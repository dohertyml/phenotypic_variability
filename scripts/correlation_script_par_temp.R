###### Overall data frames for the correlation 

means_df <- data.frame(temp_df$depth, temp_df$temp, overall_par$par)

#### porites correlation df 

past_df <- fire.data %>% filter(Species %in% "PAST")

past_mean_df <- past_df %>% group_by(depth) %>%
  mutate(mean_fvfm = mean(`Fv/Fm`), mean_sig = mean(Sigma),
         mean_p = mean(p), mean_pmax = mean(Pmax.e.s))

past.df.10 <- past_df %>% filter(depth %in% "10")
past.df.10$temp <- 30.11396
past.df.10$par <- 498.62810
past.df.20 <- past_df %>% filter(depth %in% "20")
past.df.20$temp <- 29.92680
past.df.20$par <- 313.92023
past.df.30 <- past_df %>% filter(depth %in% "30")
past.df.30$temp <- 29.73963
past.df.30$par <- 197.23852
past.df.40 <- past_df %>% filter(depth %in% "40")
past.df.40$temp <- 29.55247
past.df.40$par <- 77.86399

past_cor_df <- rbind(past.df.10, past.df.20, past.df.30, past.df.40)

View(past_cor_df)
#mcav correlation df 

mcav_df <- fire.data %>% filter(Species %in% "MCAV")

mcav_mean_df <- mcav_df %>% group_by(depth) %>%
  mutate(mean_fvfm = mean(`Fv/Fm`), mean_sig = mean(Sigma),
         mean_p = mean(p), mean_pmax = mean(Pmax.e.s))

mcav.df.10 <- mcav_df %>% filter(depth %in% "10")
mcav.df.10$temp <- 30.11396
mcav.df.10$par <- 498.62810
mcav.df.20 <- mcav_df %>% filter(depth %in% "20")
mcav.df.20$temp <- 29.92680
mcav.df.20$par <- 313.92023
mcav.df.30 <- mcav_df %>% filter(depth %in% "30")
mcav.df.30$temp <- 29.73963
mcav.df.30$par <- 197.23852
mcav.df.40 <- mcav_df %>% filter(depth %in% "40")
mcav.df.40$temp <- 29.55247
mcav.df.40$par <- 122.92650
mcav.df.40 <- mcav_df %>% filter(depth %in% "50")
mcav.df.40$temp <- 29.36531
mcav.df.40$par <- 77.86399

mcav_cor_df <- rbind(mcav.df.10, mcav.df.20, mcav.df.30, mcav.df.40)


######### correlation matrix

library("Hmisc")
library(GGally)
library(corrplot)
library("FactoMineR")
library("factoextra")

#correlation matrix

test <- merge(sem_10m_past, past_cor_df, by = "depth")
range(test$temp)
View(test)

past_cor_1 <- test %>% dplyr::select(corralite_diameter_cm, columnella_diameter_cm, septa_height, 
                                             distance_between_polyp, septa_width, ug_ml, depth, fv_fm, sigma, p.1, pmax, cell_cm,
                                             chl_cell_ug, par) 


mydata.cor = cor(past_cor_1)

mydata.cor = cor(past_cor_1, method = c("spearman"))

mydata.rcorr = rcorr(as.matrix(past_cor_1 ))
mydata.rcorr


mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

corrplot(mydata.cor)

data.pca <- princomp(mydata.cor)

past_pca_output <- data.pca$loadings
summary(data.pca)

fviz_pca_var(data.pca, 
             repel = TRUE) + theme_classic()

past_corr_plot <- corrplot(mydata.cor, type = "upper", order = "original", 
                           tl.col = "black", tl.srt = 45, method = "number")



########cor mcav
mightyscope_mcav <- read_csv("~/Desktop/nsf_fire/skeletal_morphology/data/marthas_df_only.csv")

mcav_cor <- merge(mightyscope_mcav, mcav_cor_df, by = "depth")

mcav_cor <-mcav_cor%>% dplyr::select( corralite_diameter_cm, columnella_diameter_cm, septa_height, 
                                              distance_between_polyp, depth, ug_ml, fv_fm, sigma, p.1, pmax,
                                              cell_cm, chl_cell, par)


mcav.cor = cor(mcav_cor)

mcav.cor = cor(mcav_cor, method = c("spearman"))

mcav.rcorr = rcorr(as.matrix(mcav_cor))
mcav.rcorr


mcav.coeff = mcav.rcorr$r
mcav.p = mcav.rcorr$P

corrplot(mcav.cor)

corrplot(mydata.cor)

mcav.pca <- princomp(mcav.cor)

mcav_pca_output <-mcav.pca$loadings
summary(mcav.pca)

fviz_pca_var(mcav.pca, 
             repel = TRUE) + theme_classic()

mcav_corr_plot <- corrplot(mcav.cor, type = "upper", order = "original", 
                           tl.col = "black", tl.srt = 45, method = "number")


####### ggally

ggcorr(past_cor_1) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

