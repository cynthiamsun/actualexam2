# actualexam2

git commit -m "testing"

#clear environment
rm(list = ls(all=TRUE))

# load inequality excel data
library(readxl)
inequality_data <- read_excel(file.choose())

#cross-sectional data confirmation
head(inequality_data)

#subset for Denmark and Sweden
subset(inequality_data, country == "Denmark")
subset(inequality_data, country == "Sweden")

#inquality gini for Brazil
subset(inequality_data, country == "Brazil")

# quick peek at the data frame
head(inequality_data)

#create function to remove accent on Belarus
accent.remove <- function(n) {
  #substitute one character
  oldu <- "Ãº"
  newu <- "u"
  n1 <- chartr(oldu, newu, n)
}

inequality_data$country <- accent.remove(inequality_data$country)

head(inequality_data)

# sort data by ascending gini scores
inequality_data <- inequality_data[order(inequality_data$inequality_gini), ]
head(inequality_data, n=5)

# mean gini score
mean_gini <- mean(inequality_data$inequality_gini, na.rm = TRUE)
print(mean_gini)

# two dummy variables for high and low inequality gini scores
high_inequality <- ifelse(inequality_data$inequality_gini > 36.81375, 1, 0)
low_inequality <- ifelse(inequality_data$inequality_gini < 36.81375, 1, 0)
print(high_inequality)
print(low_inequality)

# 12 COMEBACK cross-tab inequality categories
library(descr)

crosstab(Inequality, row.vars = c("high_inequality", "low_inequality"),
        col.vars = mean_gini, type = "f")

# names for three actors
actors <- c('World Bank', 'African Development Bank', 
            'Bill and Melinda Gates Foundation')
for(i in actors) {
  print(i)
}

#import "Literacy rates, adult female" WDI var into R
remotes::install_github('vincentarelbundock/WDI')
library(WDI)
literacy_females = WDI(country = "all",
                       indicator = c("SE.ADT.LITR.FE.ZS"),
                       start = 2015, end = 2015, extra = FALSE,
                       cache = NULL)

# merge the inequality datasets
merged_df <- dplyr::left_join(inequality_data, literacy_females, 
                              by = c("country", "year"))

#fix iso2c.x and iso2x.y errors
library(tidyverse)
merged_df$iso2c.y = NULL
merged_df <- merged_df %>%
  dplyr::rename("iso2c" = "iso2c.x")
merged_df <- merged_df %>%
  dplyr::rename("literacy_female" = "SE.ADT.LITR.FE.ZS")

#remove missing data
merged_df <- na.omit(merged_df, 
                     select = c("inequality_gini", "literacy_female"))

# filter gini scores higher than 30
library(tidyverse)
data_greater_30 <- merged_df %>%
  dplyr::filter(inequality_gini > 30, preserve = TRUE)

#count countries with "ai"
grep("ai", data_greater_30)

#take sum of gini in dataset cOMEBACK
lapply(data_greater_30$inequality_gini, MARGIN =2, FUN = sum)

#label variables in merged_df
library(labelled)
var_label(merged_df) <- list(`iso2c` = "ISO-2 Country Code", 
                             `country` = "Country",
                             `inequality_gini` = "Gini Inequality Score",
                             `year` = "Year", `literacy_female` =
                               "Literacy Rate of Female Adults")

#save stata
library(rio)
export(merged_df, "final_data")
