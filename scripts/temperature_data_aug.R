####### temperature analysis

###### temp
Marthas_10m <- read_csv("data/PAR_Temp/Marthas_10m.csv")
Marthas_40m <- read_csv("data/PAR_Temp/Marthas40m.csv")
CoralCity_10m <- read_csv("data/PAR_Temp/CoralCity_10m.csv")
CoralCity_40m <- read_csv("data/PAR_Temp/CoralCity_40m.csv")

## weird date formats so have to blag it

date_time <- strsplit(Marthas_10m$`Date Time, GMT-05:00`, " ")
Marthas_10m$time <- sapply(date_time, function(x) x[2])
date_time <- strsplit(Marthas_40m$`Date Time, GMT-05:00`, " ")
Marthas_40m$time <- sapply(date_time, function(x) x[2])
date_time <- strsplit(CoralCity_10m$`Date Time, GMT-05:00`, " ")
CoralCity_10m$time <- sapply(date_time, function(x) x[2])
date_time <- strsplit(CoralCity_40m$`Date Time, GMT-05:00`, " ")
CoralCity_40m$time <- sapply(date_time, function(x) x[2])

#sort dates
Marthas_10m$date <- as.Date(Marthas_10m$`Date Time, GMT-05:00`, format = "%m/%d/%y")
Marthas_40m$date <- as.Date(Marthas_40m$`Date Time, GMT-05:00`, format = "%m/%d/%y")
CoralCity_10m$date <- as.Date(CoralCity_10m$`Date Time, GMT-05:00` , format = "%m/%d/%y")
CoralCity_40m$date <- as.Date(CoralCity_40m$`Date Time, GMT-05:00`, format = "%m/%d/%y")

# remove hyphons for easy filter

Marthas_10m$date_test <- as.character(gsub("-", "", Marthas_10m$date))
Marthas_40m$date_test <- as.character(gsub("-", "", Marthas_40m$date))
CoralCity_10m$date_test <- as.character(gsub("-", "", CoralCity_10m$date))
CoralCity_40m$date_test <- as.character(gsub("-", "", CoralCity_40m$date))

# thin by time 
lower_time <- ('10:00')
upper_time <- ('14:00')

mf_thin_temp_10 <- subset(Marthas_10m, Marthas_10m$time >lower_time & Marthas_10m$time < upper_time)
mf_thin_temp_40 <- subset(Marthas_40m, Marthas_40m$time >lower_time & Marthas_40m$time < upper_time)
cc_thin_temp_10 <- subset(CoralCity_10m, CoralCity_10m$time >lower_time & CoralCity_10m$time < upper_time)
cc_thin_temp_40 <- subset(CoralCity_40m, CoralCity_40m$time >lower_time & CoralCity_40m$time < upper_time)

#using 21-6-2023 data only 

mf10_temp_jun <- Marthas_10m %>% filter(date_test %in% "20230620") 
mf40_temp_jun <- Marthas_40m %>% filter(date_test %in% "20230620")
cc10_temp_jun <- CoralCity_10m %>% filter(date_test %in% "20230620")
cc40_temp_jun <- CoralCity_40m %>% filter(date_test %in% "20230620")

## make temperature data frame and augment

mean(mf10_temp_jun$temp)
mean(mf40_temp_jun$temp)
mean(cc10_temp_jun$temp)
mean(cc40_temp_jun$temp)

temp_10 <- rbind(mf10_temp_jun, cc10_temp_jun)
temp_40 <- rbind(mf40_temp_jun, cc40_temp_jun)

###### find means

mean(temp_10$temp)
mean(temp_40$temp)

depth <- c(10, 40)
temp <- c(30.11396, 29.55247)

temp_df <- data.frame(depth, temp)
#predict values from model 
model <- glm(temp~depth, data = temp_df  )
new_depths <- data.frame(depth=c(20,30,50))
predict_pars <- predict(model, newdata = new_depths)

print(predict_pars)

# new temp df 
depth <- c(10, 20, 30, 40, 50)
temp <- c(30.11396, 29.92680, 29.73963, 29.55247, 29.36531)

temp_df <- data.frame(depth, temp)