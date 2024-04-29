#################################### Table building

list.of.packages <- c("gapminder", "gt", "tidyverse")

# install required packages, if necessary, and load them ----
{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)
}

library(gapminder)
library(gt)
library(tidyverse)

crn <- 30

test <- rbind(test, past_table)
test <- test %>% dplyr::filter(!contrast %in% "marthas-coral_city")


test1 <- test %>%
  gt(groupname_col = c("Species","Morphology factor")) %>%
  cols_label(
    contrast = md("Contrast"),
    estimate = md("Estimate"),
    conf.low= md("Confidence low"),
    conf.high = md("Confidence high"),
    adj.p.value = md("Adjusted p value")
  ) 


gt_table <- significant_df %>% 
  head(crn) %>% 
  gt(
    groupname_col = "Species",
    rowname_col = "Parameter"
  )

head(test)
