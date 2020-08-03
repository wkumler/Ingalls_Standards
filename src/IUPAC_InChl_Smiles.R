library(tidyverse)

## Incorporate IUPAC, InChl, and SMILES keys into Ingalls Standards csv.

Ingalls.Standards <- read.csv("Ingalls_Lab_Standards_NEW.csv") %>%
  select(-X)

Ingalls.Standards.IUPAC.InChl.SMILES <- read.csv("data_raw/Ingalls_Lab_Standards_Alt_Names.csv") %>%
  select(Column, Compound.Name, IUPAC.Name, InChI.Key.Name, Canon.SMILES.Name) %>%
  mutate_if(is.character, list(~na_if(., ""))) %>%
  group_by(Column, Compound.Name) %>%
  drop_na()

Ingalls.Standards.IUPAC.InChl.SMILES[Ingalls.Standards.IUPAC.InChl.SMILES==""]<-NA

Combined.Names <- Ingalls.Standards %>%
  left_join(Ingalls.Standards.IUPAC.InChl.SMILES, by = c("Compound.Name", "Column")) %>%
  unique()

Ingalls_Lab_Standards_IUPAC.InCHI.SMILES <- Combined.Names