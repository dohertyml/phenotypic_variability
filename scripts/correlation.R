######### correlation matrix

library("Hmisc")
library(GGally)
library(corrplot)
library("FactoMineR")
library("factoextra")

#correlation matrix

sem_10m_past <- read_csv("~/Desktop/nsf_fire/skeletal_morphology/data/sem_10m_past_mf.csv")



past_cor_1 <- sem_10m_past %>% dplyr::select(corralite_diameter_cm, columnella_diameter_cm, septa_height, 
                                        distance_between_polyp, septa_width, ug_ml, depth, fv_fm, sigma, p, pmax, cell_cm,
                                        chl_cell_ug) 

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

View(mightyscope_mcav)

mcav_cor <-mightyscope_mcav%>% dplyr::select( corralite_diameter_cm, columnella_diameter_cm, septa_height, 
                                             distance_between_polyp, depth, ug_ml, fv_fm, sigma, p, pmax,
                                             cell_cm, chl_cell)

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
