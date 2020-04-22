## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping V1_T2.1

## Load the libraries required ------
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df <- tabulizer::extract_tables("../../../Desktop/KenyanCensus2019/Resources/VOLUME 1 KPHC 2019.pdf", pages = 15)
df <- data.frame(df)

## Drop the first row
df <- df[-1, ]

## Remove the commas and elipses
df <- df %>%
  mutate(X2 = as.numeric(str_squish(gsub(",|\\...", "", X2))))

## Remove the elipses from the first variable
df <- df %>%
  mutate(X1 = str_squish(gsub("\\.", "", X1)))

## Format the Number of Households ('000s)	variable
df [7,2] <- 12143.9

## Rename the variables
names(df) <- c("Indicator", "Value")

## Save the dataset ------
# write_csv(df, "../../../Desktop/KenyanCensus2019/Datasets/V1_T2.1.csv")
# V1_T2.1 <- df
# usethis::use_data(V1_T2.1)
