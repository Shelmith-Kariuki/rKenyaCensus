## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping Table 1.2: List of Counties and Sub-Counties

## Load the libraries required ------
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df_0 <- tabulizer::extract_areas("../../../Desktop/KenyanCensus2019/Resources/2019 KPHC Volume II_.pdf",
                                 pages = 18:20)

tab_func <- function(num){
df <- df_0[[num]]

## Drop the first 6 rows
df <- data.frame(df[-1:-6,])

## Drop the blank rows
df <- df %>%
  filter(X1 != "")

## Stack X1 and X2

df <- df %>%
  pivot_longer(names_to = "Var", values_to = "Data", c(X1, X2)) %>%
  select(-Var)

regex <- "([0-9]+ [^0-9]+) (.+)"
df$X1 <- gsub(x=df$Data, pattern=regex, replacement='\\1')
df$X2 <- gsub(x=df$Data, pattern=regex, replacement='\\2')

## Split X1 to County Code and County and X2 to Subcounty Code and Subcounty

rexp <- "^(\\w+)\\s?(.*)$"
df$CountyCode <- as.numeric(sub(rexp,"\\1",df$X1))
df$County <- trimws(sub(rexp,"\\2",df$X1))
df$SubCountyCode <- as.numeric(sub(rexp,"\\1",df$X2))
df$SubCounty <- trimws(sub(rexp,"\\2",df$X2))

## Drop the variables that we nolonger need
df <- df %>%
  select(-Data, -X1, -X2)

## Order the data
df <- df %>%
  arrange(CountyCode, SubCountyCode)

return(df)
}

## Call the function
df_rows <- map_df(1:length(df_0), tab_func)

## Save the dataset
# write_csv(df_rows, "../../../Desktop/KenyanCensus2019/Datasets/V2_T1.2.csv")
# V2_T1.2 <- df_rows
# usethis::use_data(V2_T1.2)
