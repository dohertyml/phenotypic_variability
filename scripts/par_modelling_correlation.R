########### PAR DATA MANAGEMENT AND MODELLING MD.
## read in the PAR data

CC_10m_PAR <- read_csv("data/PAR_Temp/CC_10m_PAR.csv")
CC_40m_PAR <- read_csv("data/PAR_Temp/CC_40m_PAR.csv")
MF_10m_PAR <- read_csv("data/PAR_Temp/MF_10m_PAR.csv")
MF_40m_PAR <- read_csv("data/PAR_Temp/MF_40m_PAR.csv")

### add sites to dataframe
CC_10m_PAR$site <- "coral_city"
CC_40m_PAR$site <- "coral_city"
MF_10m_PAR$site <- "marthas_finyard"
MF_40m_PAR$site <- "marthas_finyard"

#### add depths to dataframe
CC_10m_PAR$depth <- as.numeric("10")
CC_40m_PAR$depth <- as.numeric("40")
MF_10m_PAR$depth <- as.numeric("10")
MF_40m_PAR$depth <- as.numeric("40")

##rbind the data frame into one df
par_overall_df <- rbind(CC_10m_PAR, CC_40m_PAR, MF_10m_PAR, MF_40m_PAR)

### write the csv

write.csv(par_overall_df, "data/PAR_Temp/overall_par.csv")

# Convert 'Columbia_Time' column to POSIXct datetime object
par_overall_df$`Colombia Time` <- ymd_hms(par_overall_df$`Colombia Time`)

# Extract date from Columbia_Time and create a new Date column
par_overall_df$date <- as.Date(par_overall_df$`Colombia Time`)

# Format the time as HH:MM:SS
par_overall_df$time <- format(par_overall_df$`Colombia Time`, "%H:%M:%S")

daily_max_par <- par_overall_df %>% group_by(date, depth) %>% 
  summarize(max_par = max(PAR, na.rm = TRUE))

avg_par_depth <- daily_max_par %>% group_by(depth) %>%
  summarize(median_par_yr = median(max_par))

# Define the known PAR values at 10m and 40m depths
E10 <- 655  # PAR at 10m
E40 <- 250  # PAR at 40m
z_difference <- 40 - 10  # Difference in depth

# Calculate the light attenuation coefficient (Kd) ensuring it's positive
Kd <- abs(1 / z_difference * log(E10 / E40))

# Function to model the vertical profile of light intensity within the water column
calculate_Iz <- function(I0, Kd, z) {
  return(I0 * exp(-Kd * z))
}

# Depths to model
depths <- c(10, 20, 30, 40, 50)

# Calculate the light intensity at different depths
light_intensity_at_depths <- sapply(depths - 10, function(z) calculate_Iz(E10, Kd, z))

# Combine the depths and their corresponding light intensities
light_intensities <- setNames(light_intensity_at_depths, depths)

# Print the light attenuation coefficient and the light intensities at the specified depths
Kd
light_intensities
