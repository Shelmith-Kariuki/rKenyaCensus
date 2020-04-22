## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping Table 2.5: Distribution of Population by Urban Centres, Sex* and County

## Load the libraries required ------
rm(list = ls())
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df_0 <- tabulizer::extract_tables("../../../Desktop/KenyanCensus2019/Resources/2019 KPHC Volume II_.pdf",
                                  pages = 252:257)

tab_func <- function(num){

## Rename the dataset
df <- data.frame(df_0[[num]])
colnames(df) <- c("Urban Center", "County", "Total", "Male", "Female")

## Drop the first row
df <- df %>% filter(County != "COUNTY")

## Remove commas and extra spaces
df[,-1:-2] <- data.frame(lapply(df[,-1:-2], function(x) gsub(",","",x)))
df[,-1:-2] <- data.frame(sapply(df[,-1:-2], function(x) as.numeric(gsub(" ","",x))))

return(df)

}

## Call the function
df_rows <- map_df(1:length(df_0), tab_func)

## Check to see if the scrapping has been done correctly

df_checker <- df_rows %>%
  mutate(total2 = Male + Female,
         check1 = Total == total2)

# Intersex figures are too few to be distributed at sub national level. Totals may therefore differ slightly
# **Urban Centres with a population of 2,000 and above

## Save the dataset
write_csv(df_rows, "../../../Desktop/KenyanCensus2019/Datasets/V2_T2.5.csv")
V2_T2.5 <- df_rows
usethis::use_data(V2_T2.5, overwrite = TRUE)
