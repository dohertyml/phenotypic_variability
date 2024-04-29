### par
MF_10m_PAR <- read_csv("~/Desktop/nsf_fire/PAR_Temp/MF_10m_PAR.csv")
MF_40m_PAR <- read_csv("~/Desktop/nsf_fire/PAR_Temp/MF_40m_PAR.csv")
CC_10m_PAR <- read_csv("~/Desktop/nsf_fire/PAR_Temp/CC_10m_PAR.csv")
CC_40m_PAR <- read_csv("~/Desktop/nsf_fire/PAR_Temp/CC_40m_PAR.csv")

mean(MF_10m_PAR$PAR)
mean(MF_40m_PAR$PAR)

#sort dates
MF_10m_PAR$date <- as.Date(MF_10m_PAR$`Colombia Time`)
MF_40m_PAR$date <- as.Date(MF_40m_PAR$`Colombia Time`)
CC_10m_PAR$date <- as.Date(CC_10m_PAR$`Colombia Time`)
CC_40m_PAR$date <- as.Date(CC_40m_PAR$`Colombia Time`)
# remove hyphons for easy filter

MF_10m_PAR$date_test <- as.character(gsub("-", "", MF_10m_PAR$date))
MF_40m_PAR$date_test <- as.character(gsub("-", "", MF_40m_PAR$date))
CC_10m_PAR$date_test <- as.character(gsub("-", "", CC_10m_PAR$date))
CC_40m_PAR$date_test <- as.character(gsub("-", "", CC_40m_PAR$date))

#using 21-6-2023 data only 

mf_10m_21_6 <- MF_10m_PAR %>% filter(date_test %in% "20230620") 
mf_40m_21_6 <- MF_40m_PAR %>% filter(date_test %in% "20230620")
cc_10m_21_6 <- CC_10m_PAR %>% filter(date_test %in% "20230620")
cc_40m_21_6<- CC_40m_PAR %>% filter(date_test %in% "20230620")
# thin by time 

mf_10m_21_6$Time <- format(mf_10m_21_6$`Colombia Time` ,("%H:%M:%S"))
mf_40m_21_6$Time <- format(mf_40m_21_6$`Colombia Time` ,("%H:%M:%S"))
cc_10m_21_6$Time<- format(cc_10m_21_6$`Colombia Time` ,("%H:%M:%S"))
cc_40m_21_6$Time  <- format(cc_40m_21_6$`Colombia Time` ,("%H:%M:%S"))

lower_time <- ('10:00:00')
upper_time <- ('14:00:00')

mf_thin_par_10 <- subset(mf_10m_21_6, mf_10m_21_6$Time >lower_time & mf_10m_21_6$Time < upper_time)
mf_thin_par_40 <- subset(mf_40m_21_6, mf_40m_21_6$Time > lower_time & mf_40m_21_6$Time < upper_time)
cc_thin_par_10 <- subset(cc_10m_21_6, cc_10m_21_6$Time >lower_time & cc_10m_21_6$Time < upper_time)
cc_thin_par_40 <- subset(cc_40m_21_6, cc_40m_21_6$Time > lower_time & cc_40m_21_6$Time < upper_time)

# find the means 

mean(mf_thin_par_10$PAR)
mean(mf_thin_par_40$PAR)
mean(cc_thin_par_10$PAR)
mean(cc_thin_par_40$PAR)

#merge df
par_10 <- rbind(mf_thin_par_10, cc_thin_par_10)
par_40 <- rbind(mf_thin_par_40, cc_thin_par_40)

# means of par per depth
mean(par_10$PAR)
mean(par_40$PAR)

depth <- c(10, 40)
par <- c(498.6281, 122.9265)
par <- log1p(par)

par_df <- data.frame(depth, par)
#predict values from model 
model <- glm(par~depth, data = par_df  )
new_depths <- data.frame(depth=c(20,30,50))
predict_pars <- predict(model, newdata = new_depths)

#reverse log

exp(predict_pars)

# new par df 
depth <- c(10, 20, 30, 40, 50)
par <- c(498.6281, 313.92023, 197.23852, 122.9265, 77.86399)

overall_par <- data.frame(depth, par)
