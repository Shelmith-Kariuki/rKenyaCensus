## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping Table 1.1: Summary of Census Counts in Kenya

## Load the libraries required ------
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df_0 <- tabulizer::extract_areas("../../../Desktop/KenyanCensus2019/Resources/2019 KPHC Volume II_.pdf",
                                pages = 12)
df <- data.frame(df_0)

## Filter the rows with numerical values
df <- df[18:26, ]

## Split the second variable into 2
df <- df %>%
  separate(X2, into = c("Year", "Population (millions)"), sep =" ") %>%
  select(-X1)

## Remove the commas ------
## Save the dataset ------
write_csv(df, "../../../Desktop/KenyanCensus2019/Datasets/V2_T1.1.csv")
V2_T1.1 <- df
usethis::use_data(V2_T1.1)
