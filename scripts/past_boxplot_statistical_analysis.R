########## PORITES ASTREOIDES BOXPLOT AND STATISTICAL ANALYSIS.
#filter for mcav and past 
past_df <- read_csv("data/mf_cc.csv")

length(past_df$frag_code)

### remove outliers

# cor
quartiles <- quantile(past_df$corralite_diameter_cm, probs = c(.25,.75))
IQR_cor <- IQR(past_df$corralite_diameter_cm)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

cor_no_outlier <- subset(past_df, past_df$corralite_diameter_cm >lower & past_df$corralite_diameter_cm< upper)
cor_no_outlier$log_cor <- log(cor_no_outlier$corralite_diameter_cm+1)

mean_cor_df <- cor_no_outlier %>% group_by(depth) %>% 
  summarise(mean_cor_diameter = mean(corralite_diameter_cm), sd = sd(corralite_diameter_cm), 
                                               se = sd/sqrt(length(corralite_diameter_cm)))

dim(cor_no_outlier)

### col

quartiles <- quantile(past_df$columnella_diameter_cm, probs = c(.25,.75))
IQR_col <- IQR(past_df$columnella_diameter_cm)

lower <- quartiles[1] - 1.5*IQR_col
upper <- quartiles[2] + 1.5*IQR_col

col_no_outlier <- subset(past_df, past_df$columnella_diameter_cm >lower & past_df$columnella_diameter_cm< upper)
col_no_outlier$log_col <- log(col_no_outlier$columnella_diameter_cm+1)

dim(col_no_outlier)

### sep

quartiles <- quantile(past_df$septa_height, probs = c(.25,.75))
IQR_sep <- IQR(past_df$septa_height)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

sep_no_outlier <- subset(past_df, past_df$septa_height >lower & past_df$septa_height< upper)
sep_no_outlier$log_sep <- log(sep_no_outlier$septa_height+1)

dim(sep_no_outlier)


# septa width

quartiles <- quantile(past_df$septa_width, probs = c(.25,.75))
IQR_sep <- IQR(past_df$septa_width)

lower <- quartiles[1] - 1.5*IQR_sep
upper <- quartiles[2] + 1.5*IQR_sep

width_no_outlier <- subset(past_df, past_df$septa_width >lower & past_df$septa_width< upper)

dim(past_df)
dim(width_no_outlier)


### dist 
quartiles <- quantile(past_df$distance_between_polyp_1, probs = c(.25,.75))
IQR_dis <- IQR(past_df$distance_between_polyp_1)

lower <- quartiles[1] - 1.5*IQR_dis
upper <- quartiles[2] + 1.5*IQR_dis

dis_no_outlier <- subset(past_df, past_df$distance_between_polyp_1 > lower & past_df$distance_between_polyp_1< upper)

dim(past_df)
dim(dis_no_outlier)

####### plot

## box plot by depth 
past_col <- ggplot(col_no_outlier, aes(x = fct_rev(as.factor(depth)), y = columnella_diameter_cm*10, 
                                       fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") + 
  xlab("Depth") + ylab("Columnella diameter (mm)") +
  coord_flip()  +
  scale_fill_brewer(palette ='Reds') +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)

past_cor <- ggplot(cor_no_outlier, aes(x = fct_rev(as.factor(depth)), y = corralite_diameter_cm*10,
                                       fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5) +
  xlab("Depth") + ylab("Corallite diameter (mm)") +
  coord_flip() +
  scale_fill_brewer(palette ='Reds') +
  theme(legend.position="none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)

past_septa <- ggplot(sep_no_outlier, aes(x = fct_rev(as.factor(depth)), y = septa_height*10, fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") +
  xlab("Depth") + ylab("Septa height (mm)") +
  coord_flip() +
  scale_fill_brewer(palette ='Reds') +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)

past_dist <- ggplot(dis_no_outlier, aes(x = fct_rev(as.factor(depth)), y = distance_between_polyp_1*10, 
                                        fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") +
  xlab("Depth") + ylab("Distance between polyps (mm)") +
  scale_fill_brewer(palette ='Reds') +
  coord_flip() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)

sep_width <- ggplot(width_no_outlier, aes(x = fct_rev(as.factor(depth)), y = septa_width*10, 
                           fill = as.factor(depth))) +
  geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.5, position = "dodge") +
  xlab("Depth") + ylab("Septa width (mm)") +
  scale_fill_brewer(palette ='Reds') +
  coord_flip() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 14)) +
  geom_beeswarm(size = 1)

past_fig <- ggarrange(past_cor + rremove("ylab"), past_col + rremove("ylab"), past_septa + rremove("ylab"),
                      past_dist + rremove("ylab"), sep_width + rremove ("ylab"), ncol = 2, nrow = 3)

annotate_figure(past_fig, left = textGrob("Depth (m)", rot = 90, vjust = 0.5, 
                                         gp = gpar(fontfamily = "TT Arial", cex = 1.2)))

## coralite statistical analysis

past_cor_aov <- aov(data = cor_no_outlier, corralite_diameter_cm ~ as.factor(depth))
summary(past_cor_aov)
past_cor_residual <- past_cor_aov$residuals
shapiro.test(log(past_cor_residual + 1))
kruskal.test(data = cor_no_outlier, corralite_diameter_cm ~as.factor(depth))
dunns_sep_past <- dunn.test(cor_no_outlier$corralite_diameter_cm, as.factor(cor_no_outlier$depth), list = TRUE)

# past columnella

past_col_aov <- aov(data = col_no_outlier, columnella_diameter_cm ~ as.factor(depth))
summary(past_col_aov)
pas_col_residuals <- past_col_aov$residuals
shapiro.test(pas_col_residuals)
TukeyHSD(past_col_aov)

# no significant differences

#past_septa_height

past_sep_aov <- aov(data= sep_no_outlier, septa_height ~ as.factor(depth))
summary(past_sep_aov)
past_sep_residual <- past_sep_aov$residuals
shapiro.test(past_sep_residual)
kruskal.test(data = sep_no_outlier, septa_height ~as.factor(depth))
dunns_sep_past <- dunn.test(sep_no_outlier$septa_height, as.factor(sep_no_outlier$depth), list = TRUE)

#past_distance

past_dis_aov <- aov(data = dis_no_outlier, distance_between_polyp_1 ~ as.factor(depth))
summary(past_dis_aov)
dis_residuals<- past_dis_aov$residuals
shapiro.test(dis_residuals)
TukeyHSD(past_dis_aov)

#past_sep_width

past_width_aov <- aov(data = width_no_outlier, septa_width ~ as.factor(depth))
summary(past_dis_aov)
width_residuals<- past_width_aov$residuals
shapiro.test(log(width_residuals+1))
kruskal.test(data = width_no_outlier, septa_width ~as.factor(depth))
dunns_sep_past <- dunn.test(width_no_outlier$septa_width, as.factor(width_no_outlier$depth), list = TRUE)


past_dis_sig <- data.frame(dunn_dis_past)
past_dis_sig$Species <- "Porites astreoides"
past_dis_sig$parameter <- "Distance between polyps (cm)"

past_sig_df <- rbind(past_sep_sig, past_dis_sig)

past_sig_dif1 <- past_sig_df %>% filter(!P.adjusted>0.05) 

past_sig_dif1$P_value <- round(past_sig_dif1$P.adjusted, 5)



dunns_past <- past_sig_dif1 %>%
  dplyr::rename("Parameter" = parameter,
                "Comparison" = comparisons) %>% 
  dplyr::select(Parameter, Comparison, P_value, Species)

significant_df <- rbind(ocutput, dunns_past)

significant_df$Comparison <- gsub(" ", "", significant_df$Comparison)

view(significant_df)
