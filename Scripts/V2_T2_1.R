## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping Table 2.1: Sub-locations with no People on the Census Night by Status/Reason

## Load the libraries required ------
rm(list = ls())
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df_0 <- tabulizer::extract_areas("../../../Desktop/KenyanCensus2019/Resources/2019 KPHC Volume II_.pdf",
                                 pages = 21)

## Keep Rows 10 to 23
df <- data.frame(df_0[[1]][10:23,], stringsAsFactors = F)

## Drop the first row because it contains the column names
df <- data.frame(df[-1,],stringsAsFactors = F)

## First row are the column names
colnames(df) <- c("County", "Sub-Location", "Status/Reason")

write_csv(df, "../../../Desktop/KenyanCensus2019/Datasets/V2_T2.1.csv")
V2_T2.1 <- df
usethis::use_data(V2_T2.1, overwrite = TRUE)
