## Classyfire IDs

library(tidyverse)

Classyfire <- read.csv("data_raw/classyfire_stds.csv") 
Ingalls_Lab_Standards <- read.csv("Ingalls_Lab_Standards_NEW.csv", check.names = FALSE)

Classyfire$Compound.Name <- gsub("_", " ", Classyfire$Compound.Name)

Combined.Classyfire <- Classyfire %>%
  unite(Combined, c("Level", "Classification"), sep = ": ") %>%
  arrange(Compound.Name)


## Create dataframe of 
split.by.compound <- split(Combined.Classyfire, f = Combined.Classyfire$Compound.Name)

New.rows <- lapply(split.by.compound, function(x) {
  new.df <- rbind(x, apply(x[2], 2, paste0, collapse = "; "))
  interim.frame <- data.frame(new.df[1, 1], new.df[9, 2]) %>%
    rename(Compound.Name = 1,
           Classyfire = 2)
})

All.Classyfire <- bind_rows(New.rows)

Ingalls_Lab_Standards_Classyfire <- Ingalls_Lab_Standards %>%
  left_join(All.Classyfire, by = "Compound.Name") %>%
  select(-1)

rownames(Ingalls_Lab_Standards_Classyfire) <- NULL