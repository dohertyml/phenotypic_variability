######################## chlorophyll and zoox analysis
options(scipen = 999)
library(corrplot)
library(ggcorrplot)

chl_mcav <- read_csv("data/chl_mcav.csv")

chl_past <- read_csv("data/chl_past.csv")

chl_mcav <- chl_mcav %>% mutate(ug_ml = 0.3319 * six_three_zero - 1.7485 * six_four_seven + 11.9442 * six_six_four
                         - 1.4306 * six_nine_one,
                         cell_sa = avg_count/surface_area_cm2,
                         chlorophyll_zooxs = (ug_ml/cell_ml)* avg_count,
                         chl_cm = chlorophyll_zooxs*cell_sa)

## thin mcav sa

quartiles <- quantile(chl_mcav$cell_sa , probs = c(.25,.75))
IQR_sep <- IQR(chl_mcav$cell_sa)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

mcav_sa_no_outlier <- subset(chl_mcav, chl_mcav$cell_sa >lower & chl_mcav$cell_sa< upper)

dim(mcav_sa_no_outlier)

#thin mcav zooxs
quartiles <- quantile(chl_mcav$chlorophyll_zooxs, probs = c(.25,.75))
IQR_sep <- IQR(chl_mcav$chlorophyll_zooxs)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

mcav_chl_no_outlier <- subset(chl_mcav, chl_mcav$chlorophyll_zooxs >lower & chl_mcav$chlorophyll_zooxs< upper)

dim(mcav_chl_no_outlier)

#thin mcav chl_sa
quartiles <- quantile(chl_mcav$chl_cm, probs = c(.25,.75))
IQR_chl_sa <- IQR(chl_mcav$chl_cm)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

mcav_chl_sa_no_outlier <- subset(chl_mcav, chl_mcav$chl_cm >lower & chl_mcav$chl_cm < upper)

dim(mcav_chl_no_outlier)
### past 

chl_past <- chl_past %>% mutate(ug_ml = 0.3319 * six_three_zero - 1.7485 * six_four_seven + 11.9442 * six_six_four
                         - 1.4306 * six_nine_one,
                         cell_sa = avg_count/surface_area_cm2,
                         chlorophyll_zooxs = (ug_ml/cell_ml)* avg_count,
                         chl_cm = chlorophyll_zooxs*cell_sa) 

# thin past sa

quartiles <- quantile(chl_past$cell_sa , probs = c(.25,.75))
IQR_sep <- IQR(chl_past$cell_sa)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

past_sa_no_outlier <- subset(chl_past, chl_past$cell_sa >lower & chl_past$cell_sa< upper)
dim(past_sa_no_outlier)
# thin past zoox
quartiles <- quantile(chl_past$chlorophyll_zooxs, probs = c(.25,.75))
IQR_sep <- IQR(chl_past$chlorophyll_zooxs)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

past_chl_no_outlier <- subset(chl_past, chl_past$chlorophyll_zooxs >lower & chl_past$chlorophyll_zooxs< upper)
dim(past_chl_no_outlier)

#thin past chl_sa
quartiles <- quantile(chl_past$chl_cm, probs = c(.25,.75))
IQR_chl_sa <- IQR(chl_past$chl_cm)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

past_chl_sa_no_outlier <- subset(chl_past, chl_past$chl_cm >lower & chl_past$chl_cm < upper)

###mcav ggplot 

mcav_zooxs <- ggplot(mcav_sa_no_outlier, aes(x = fct_rev(as.factor(depth)), y = cell_sa, 
                                             fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("Depth") + ylab("Zooxanthellae cm2") +
  coord_flip()  +
  scale_fill_brewer() +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm()

mcav_chl <- ggplot(mcav_chl_no_outlier, aes(x = fct_rev(as.factor(depth)), y = chlorophyll_zooxs, 
                                                    fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("Depth") + ylab("chl/zoox") +
  coord_flip()  +
  scale_fill_brewer() +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14))+
  geom_beeswarm()

View(mcav_chl_sa_no_outlier)

mcav_chl_sa <- ggplot(mcav_chl_sa_no_outlier, aes(x = fct_rev(as.factor(depth)), y = chl_cm, 
                                            fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("Depth") + ylab("chl/zoox") +
  coord_flip()  +
  scale_fill_brewer() +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14))+
  geom_beeswarm()

ggarrange(mcav_zooxs, mcav_chl)

## statistical analysis
mcav_zoox_aov <- aov(data = mcav_sa_no_outlier, cell_sa ~ as.factor(depth))
summary(mcav_zoox_aov)
mcav_zoox_residual <- mcav_zoox_aov$residuals
shapiro.test(sqrt(mcav_zoox_residual))
kruskal.test(data = mcav_sa_no_outlier, cell_sa ~as.factor(depth))

## mcav chl 
mcav_chl_aov <- aov(data = mcav_chl_no_outlier, chlorophyll_zooxs ~ as.factor(depth))
summary(mcav_chl_aov)
mcav_chl_residual <- mcav_chl_aov$residuals
shapiro.test(log(mcav_chl_residual))
TukeyHSD(mcav_chl_aov) %>% tidy() %>% filter(!adj.p.value >0.05 )

## mcav chl_sa 
mcav_chl_sa_aov <- aov(data = mcav_chl_sa_no_outlier, chl_cm ~ as.factor(depth))
summary(mcav_chl_sa_aov)
mcav_chl_residual <- mcav_chl_aov$residuals
shapiro.test(log(mcav_chl_residual))
TukeyHSD(mcav_chl_aov) %>% tidy() %>% filter(!adj.p.value >0.05 )

####past ggplot zoox data

past_chl <- ggplot(past_chl_no_outlier, aes(x = fct_rev(as.factor(depth)), y = chlorophyll_zooxs, 
                                       fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("Depth") + ylab("Chlorophyll/zoox ug") +
  coord_flip()  +
  scale_fill_brewer(palette = 'Reds') +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm()

past_zooxs <- ggplot(past_sa_no_outlier, aes(x = fct_rev(as.factor(depth)), y = cell_sa, 
                                 fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("Depth") + ylab("Zooxanthellae/cm2") +
  coord_flip()  +
  scale_fill_brewer(palette = 'Reds') +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm()

past_chl_sa <- ggplot(past_chl_sa_no_outlier, aes(x = fct_rev(as.factor(depth)), y = chl_cm, 
                                             fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("Depth") + ylab("Zooxanthellae/cm2") +
  coord_flip()  +
  scale_fill_brewer(palette = 'Reds') +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm()

zooxs_fig <- ggarrange(mcav_zooxs + rremove("ylab"), mcav_chl + rremove("ylab"), 
                       past_zooxs + rremove("ylab"), past_chl + rremove("ylab"))

annotate_figure(zooxs_fig, left = textGrob("Depth (m)", rot = 90, vjust = 0.5, 
                                         gp = gpar(fontfamily = "TT Arial", cex = 1.2)))

## statistical analysis

past_zoox_aov <- aov(data = past_sa_no_outlier, cell_sa ~ as.factor(depth))
summary(past_zoox_aov)
past_zoox_residual <- past_zoox_aov$residuals
shapiro.test(past_zoox_residual)

past_chl_aov <- aov(data = past_chl_no_outlier, chlorophyll_zooxs ~ as.factor(depth))
summary(past_chl_aov)
past_chl_residual <- past_chl_aov$residuals
shapiro.test(sqrt(past_chl_residual))

past_chl_sa_no_outlier <- aov(data = past_chl_sa_no_outlier, chlorophyll_zooxs ~ as.factor(depth))
summary(past_chl_sa_no_outlier)
past_chl_sa_residual <- past_chl_sa_no_outlier$residuals
shapiro.test(past_chl_sa_residual)

##past corr

past_cor_1 <-chl_past %>% dplyr::select(depth, cell_cm, chlorophyll_zooxs)

mydata.cor = cor(past_cor_1)

mydata.cor = cor(past_cor_1, method = c("spearman"))

mydata.rcorr = rcorr(as.matrix(past_cor_1 ))
mydata.rcorr


mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(past_cor_1)
head(p.mat[, 1:3])

past_corr_plot <- corrplot(mydata.cor, type = "upper", order = "original", 
                           tl.col = "black", tl.srt = 45, method = "number")


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mydata.cor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
          sig.level = -0.02, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

##mcav corr

mcav_cor_1 <-chl_mcav %>% dplyr::select(depth, cell_cm, chlorophyll_zooxs)

mydata.cor = cor(mcav_cor_1)

mydata.cor = cor(mcav_cor_1, method = c("spearman"))

mydata.rcorr = rcorr(as.matrix(mcav_cor_1 ))
mydata.rcorr


mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

mcav_corr_plot <- corrplot(mydata.cor, type = "upper", order = "original", 
                           tl.col = "black", tl.srt = 45, method = "number")
