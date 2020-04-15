## Author: Shelmith
## Date: 15th April, 2020
## Description: Scrapping V1_T2.3

library(tidyverse)
library(tabulizer)

df <- tabulizer::extract_areas("../../../Desktop/KenyanCensus2019/Resources/VOLUME 1 KPHC 2019.pdf",
                               pages = 19)
df <- data.frame(df)

## Drop the first 7 rows

df$rownum <- 1:nrow(df)
df <- df %>% filter(rownum >=8 & rownum <57) %>% select(-rownum)

names(df) <- "X1"

df$X1 <- gsub("Homa Bay", "HomaBay", df$X1)
df$X1 <- gsub("Nairobi City", "NairobiCity", df$X1)
df$X1 <- gsub("Trans Nzoia", "TransNzoia", df$X1)
df$X1 <- gsub("West Pokot", "WestPokot", df$X1)
df$X1 <- gsub("Tana River", "TanaRiver", df$X1)
df$X1 <- gsub("Uasin Gishu", "UasinGishu", df$X1)


## Remove the commas
df <- map_df(df, ~gsub(",", "", .x))

df$County <- gsub("\\.", "", gsub("[[:digit:]]","",df$X1))
df$Values <- substr(df$X1, 16, nchar(df$X1))
df$Values <- trimws(gsub("\\.", "", df$Values))

df <-df %>%
  separate(Values, into = c("Population","NumberOfHouseholds","AverageHouseholdSize"), sep = " ")

df$X1 <- NULL

## Filter the rows labeled County, Male and the blank row

df <- df %>%
  filter(!County %in% grep("County", County, value = T, ignore.case = T))


## Rename the Kenya value to Total
df <- df %>%
  mutate(County = ifelse(County == "Kenya", "Total", County))

## Convert the columns to the right type
df[,-1] <- map_df(df[,-1], ~as.numeric(.x))


df <- df%>%
  mutate(AverageHouseholdSize = AverageHouseholdSize / 10)

# df <- df %>%
#   mutate(fig = round(Population / NumberOfHouseholds,1)) %>%
#   mutate(checker = AverageHouseholdSize==fig)



## Save the dataset
write_csv(df, "../../../Desktop/KenyanCensus2019/Datasets/V1_T2.3.csv")
V1_T2.3 <- df
#usethis::use_data(V1_T2.2)

