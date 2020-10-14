# Script for SQL-Safe, CMAP-Friendly compound names
library(tidyverse)

replace_special_characters <- function(x) (gsub("[^[:alnum:] ]", " ", x))
replace_double_spaces <- function(x) (gsub("  ", " ", x))


Special.Names <- read.csv("Ingalls_Lab_Standards_NEW.csv", stringsAsFactors = FALSE) %>%
  select(Compound.Name, Compound.Name_figure) %>%
  filter_all(any_vars(str_detect(., "[^[:alnum:] ]"))) %>%
  unique() %>%
  mutate(Fig.SpecialCharacter = ifelse(str_detect(Compound.Name_figure, "[^[:alnum:] ]"), TRUE, FALSE),
         Long.SpecialCharacter = ifelse(str_detect(Compound.Name, "[^[:alnum:] ]"), TRUE, FALSE))

Replace.Long <- Special.Names %>%
  mutate(Compound.Name_SQL = ifelse(Long.SpecialCharacter == FALSE, Compound.Name, Compound.Name_figure)) %>%
  mutate_at(c("Compound.Name_SQL"), replace_special_characters) %>%
  filter(Fig.SpecialCharacter == TRUE & Long.SpecialCharacter == TRUE)

Last.Fixes <- Replace.Long %>%
  mutate(Compound.Name_SQL = recode(Compound.Name_SQL,
                                    # Small fixes
                                    "2 Hydroxy 4  methylthio butyric acid" = "2 Hydroxy 4 methylthio butyric acid",
                                    "7 DHC" = "7 Dehydrocholesterol",
                                    "a Tocotrienol" = "alpha Tocotrienal",
                                    "b Alanine" = "beta Alanine",
                                    "b Carotene" = "beta Carotene",
                                    "b Cyclocitral" = "beta Cyclocitral", 
                                    "b Glutamic acid" = "beta Glutamic acid",
                                    "b Ionine" = "beta Ionine",
                                    "b Ionylidene acetaldehyde" = "beta Ionylidene acetaldehyde",
                                    "cinnamoyl HSL" = "Cinnamoyl HSL",
                                    "D SAM" = "Decarboxylated S Adenosylmethionine",
                                    "F6 phosphate" = "Fructose 6 phosphate",
                                    "Keto    methylthio butyric acid" = "Keto methylthiobutyric acid",
                                    "N e  Acetyl Lysine" = "Ne Acetyl lysine")) %>%
  mutate_at(c("Compound.Name_SQL"), replace_double_spaces)

Ingalls_Lab_Standards_SQLSafe <- Ingalls_Lab_Standards_NEW %>%
  left_join(Last.Fixes, by = "Compound.Name")

