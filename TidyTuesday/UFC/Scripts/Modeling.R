library(tidyverse)
library(tidymodels)
library(vip)
library(here)
library(finetune)
library(keras)


getwd()
setwd("/Users/doyeonpyun/Documents/R/TIdyTuesday/UFC Betting Model/")

df <- read_csv("fight_data.csv")
match_df <- read_csv("fights.csv")
fighter_df <- read_csv("fighter_table.csv")

