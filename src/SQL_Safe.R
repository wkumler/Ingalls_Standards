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

Replace.Specifics <- Replace.Long %>%
  mutate(Compound.Name_SQL = recode(Compound.Name_SQL,
                                    # Small fixes
                                    "2 Hydroxy 4  methylthio butyric acid" = "2 Hydroxy 4 methylthio butyric acid",
                                    "4 Hydroxybenzaldehyde" = "4 hydroxybenzaldehyde",
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

LauraEditsSQL <- read.csv("data_raw/Standards_LeadingNumbers_LTC.csv") %>%
  select(Compound.Name_SQL, Compound.Name_SQL_LTC)
  
Last.Fixes <- Ingalls_Lab_Standards_Classyfire %>%
  left_join(Replace.Specifics, by = c("Compound.Name", "Compound.Name_figure")) %>%
  mutate(Fig.SpecialCharacter = ifelse(str_detect(Compound.Name_figure, "[^[:alnum:] ]"), TRUE, FALSE),
         Long.SpecialCharacter = ifelse(str_detect(Compound.Name, "[^[:alnum:] ]"), TRUE, FALSE)) %>%
  mutate(Compound.Name_SQL = ifelse(Fig.SpecialCharacter == FALSE & Long.SpecialCharacter == FALSE, 
                                    Compound.Name, Compound.Name_SQL),
         Compound.Name_SQL = ifelse(Fig.SpecialCharacter == FALSE, 
                                    Compound.Name_figure, Compound.Name_SQL),
         Compound.Name_SQL = ifelse(Long.SpecialCharacter == FALSE, 
                                    Compound.Name, Compound.Name_SQL)) %>%
  select(Compound.Type:Compound.Name_figure, Compound.Name_SQL, QE.LinRange:Classyfire)

Ingalls_Lab_Standards_SQLSafe <- Last.Fixes %>%
  left_join(LauraEditsSQL) %>%
  mutate(Compound.Name_SQL = ifelse(is.na(Compound.Name_SQL_LTC), Compound.Name_SQL, Compound.Name_SQL_LTC)) %>%
  select(-Compound.Name_SQL_LTC)
  

