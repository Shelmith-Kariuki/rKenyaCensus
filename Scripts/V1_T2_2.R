## Author: Shelmith
## Date: 15th April, 2020
## Description: Scrapping V1_T2.2

## Load the libraries required ------
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df <- tabulizer::extract_tables("../../../Desktop/KenyanCensus2019/Resources/VOLUME 1 KPHC 2019.pdf", pages = 17)
df <- data.frame(df)

## Remove the elipses ------
df$X1 <- gsub("\\.", "", df$X1)

## Remove the commas ------

df <- map_df(df, ~gsub(",", "", .x))
df <- df %>%
  mutate(X1 = gsub(",","", X1),
         X2 = regmatches(X1, gregexpr("[[:digit:]]*$", X1))) %>%
  mutate(X1 = gsub("[[:digit:]]", "", X1))

## Remove spaces ------
df <- map_df(df, ~trimws(.x))

## Filter the rows labeled County, Male and the blank row ------
df <- df %>%
  filter(!X1 %in% grep("County|Male", X1, value = T, ignore.case = T))

df <- df %>%
  filter(X3 != "Sex")

## Rename the Kenya value to Total ------
df <- df %>%
  mutate(X1 = ifelse(X1 == "Kenya", "Total", X1))

## Convert the columns to the right type ------
df[,-1] <- map_df(df[,-1], ~as.numeric(.x))

## Rename the columns ------
names(df) <- c("County", "Male", "Female", "Intersex", "Total")

## Check whether the data is right ------
df_checker <- df %>%
  mutate(fig = Male + Female + Intersex) %>%
  mutate(checker = Total==fig)

## Save the dataset ------
write_csv(df, "../../../Desktop/KenyanCensus2019/Datasets/V1_T2.2.csv")
#V1_T2.2 <- df
#usethis::use_data(V1_T2.2)

