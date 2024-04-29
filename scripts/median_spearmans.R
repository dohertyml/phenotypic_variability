######## spearmans correlation 
# Load necessary libraries
library(readr) # For read_csv
library(dplyr) # For data manipulation
library("Hmisc") # For rcorr function
library(corrplot) # For corrplot visualization
library("FactoMineR") # For PCA analysis
library("factoextra") # For PCA visualization
########## all the medians need to be found. earlier we modelled the PAR so we will start, then we need all the medians of the measureable values
# per species.

depth <- c(10,20,30,40,50)
par <- c(655.0000, 475.1246, 344.6464, 250.0000, 181.3453)

####### mcav chl 
chl_mcav <- read_csv("data/chl_mcav.csv")

chl_mcav <- chl_mcav %>% mutate(ug_ml = 0.3319 * six_three_zero - 1.7485 * six_four_seven + 11.9442 * six_six_four
                                - 1.4306 * six_nine_one,
                                cell_sa = avg_count/surface_area_cm2,
                                chlorophyll_zooxs = (ug_ml/cell_ml)* avg_count,
                                chl_cm = chlorophyll_zooxs*cell_sa)


mcav_chl_corr <- chl_mcav %>% group_by(depth) %>%
  summarise(med_cell_sa = median(cell_sa),
            med_chl_zooxs = median(chlorophyll_zooxs),
            med_chl_cm = median(chl_cm))

### photophysiology

fire.data <- read_csv("~/Desktop/nsf_fire/MF_CC_FIRe_DepthGradient_July2023.csv")

fire_mcav <- fire.data %>% filter(Species %in% "MCAV")
 
mcav_median <- fire_mcav %>% group_by(depth) %>% 
  summarise(med_fv_fm = median(`Fv/Fm`),
            med_sig = median(Sigma),
            med_p = median(p),
            med_pmax = median(Pmax.e.s))

mcav_median <- cbind(par, mcav_median, mcav_chl_corr) [-7]

# Compute Spearman's rank correlation matrix
# Ensure mcav_median is correctly prepared for correlation analysis
cor_matrix <- cor(mcav_median, method = "spearman")

# Use the rcorr function on the dataset directly if it is structured correctly for correlation analysis
mydata_rcorr = rcorr(as.matrix(mcav_median), type = "spearman")

# Access the correlation coefficients and p-values
mydata_coeff = mydata_rcorr$r
mydata_p = mydata_rcorr$P

# Custom color scale: blue for negative, red for positive correlations
col <- colorRampPalette(c("blue", "white", "red"))(200)

# Visualize the correlation matrix with the adjusted color scheme
corrplot(mydata_coeff, method = "color", col = col, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", diag = FALSE)

# PCA analysis
data_pca <- prcomp(mcav_median, scale. = TRUE)

# Visualize PCA results
fviz_pca_var(data_pca, repel = TRUE) + theme_classic()


##### same for past
depth <- c(10,20,30,40)
par <- c(655.0000, 475.1246, 344.6464, 250.0000)

chl_past <- read_csv("data/chl_past.csv")

chl_past <- chl_past %>% mutate(ug_ml = 0.3319 * six_three_zero - 1.7485 * six_four_seven + 11.9442 * six_six_four
                                - 1.4306 * six_nine_one,
                                cell_sa = avg_count/surface_area_cm2,
                                chlorophyll_zooxs = (ug_ml/cell_ml)* avg_count,
                                chl_cm = chlorophyll_zooxs*cell_sa) 

past_chl_corr <- chl_past %>% group_by(depth) %>%
  summarise(med_cell_sa = median(cell_sa),
            med_chl_zooxs = median(chlorophyll_zooxs),
            med_chl_cm = median(chl_cm))

### photophysiology

fire.data <- read_csv("~/Desktop/nsf_fire/MF_CC_FIRe_DepthGradient_July2023.csv")

fire_past <- fire.data %>% filter(Species %in% "PAST")

past_median <- fire_past %>% group_by(depth) %>% 
  summarise(med_fv_fm = median(`Fv/Fm`),
            med_sig = median(Sigma),
            med_p = median(p),
            med_pmax = median(Pmax.e.s))

past_median <- cbind(par, past_median, past_chl_corr) [-7]

# Compute Spearman's rank correlation matrix
# Ensure past_median is correctly prepared for correlation analysis
cor_matrix <- cor(past_median, method = "spearman")

# Use the rcorr function on the dataset directly if it is structured correctly for correlation analysis
mydata_rcorr = rcorr(as.matrix(cor_matrix), type = "spearman")

# Access the correlation coefficients and p-values
mydata_coeff = mydata_rcorr$r
mydata_p = mydata_rcorr$P

# Custom color scale: blue for negative, red for positive correlations
col <- colorRampPalette(c("blue", "white", "red"))(200)

mydata_coeff <- round(mydata_coeff, 1)

# Visualize the correlation matrix with the adjusted color scheme
corrplot(mydata_coeff, method = "color", col = col, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", diag = FALSE)

# PCA analysis
data_pca <- prcomp(past_median, scale. = TRUE)

# Visualize PCA results
fviz_pca_var(data_pca, repel = TRUE) + theme_classic()
