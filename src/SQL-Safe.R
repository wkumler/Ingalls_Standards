# Script for SQL-Safe, CMAP-Friendly compound names
library(tidyverse)


replace_special_characters <- function(x) (gsub("[^[:alnum:] ]", " ", x))

special.names <- read.csv("Ingalls_Lab_Standards_NEW.csv", stringsAsFactors = FALSE) %>%
  select(Compound.Name, Compound.Name_figure) %>%
  filter_all(any_vars(str_detect(., "[^[:alnum:] ]"))) %>%
  unique()

special.names.short <- read.csv("Ingalls_Lab_Standards_NEW.csv", stringsAsFactors = FALSE) %>%
  select(Compound.Name_figure) %>%
  filter_all(any_vars(str_detect(., "[^[:alnum:] ]"))) %>%
  unique()

test <- special.names %>%
  mutate_at(c("Compound.Name"), replace_special_characters)


